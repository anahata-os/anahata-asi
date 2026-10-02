/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.tools.maven;

import uno.anahata.asi.toolkit.maven.MavenBuildResult;
import java.io.File;
import java.io.FileWriter;
import java.io.PrintWriter;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.prefs.Preferences;
import java.util.stream.Collectors;
import lombok.extern.slf4j.Slf4j;
import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.resolver.ArtifactNotFoundException;
import org.apache.maven.artifact.resolver.ArtifactResolutionException;
import org.apache.maven.execution.ExecutionEvent;
import org.apache.maven.model.Dependency;
import org.netbeans.modules.maven.model.ModelOperation;
import org.netbeans.modules.maven.model.Utilities;
import org.netbeans.modules.maven.model.pom.POMModel;
import org.netbeans.modules.xml.xam.Component;
import org.netbeans.modules.xml.xam.dom.AbstractDocumentModel;
import org.netbeans.modules.xml.xam.dom.AbstractDocumentComponent;
import org.netbeans.modules.xml.xam.dom.DocumentModelAccess;
import org.w3c.dom.Comment;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.netbeans.api.project.Project;
import org.netbeans.api.project.ProjectInformation;
import org.netbeans.api.project.ProjectUtils;
import org.netbeans.modules.maven.api.ModelUtils;
import org.netbeans.modules.maven.api.NbMavenProject;
import org.netbeans.modules.maven.api.execute.RunConfig;
import org.netbeans.modules.maven.api.execute.RunUtils;
import org.netbeans.modules.maven.embedder.EmbedderFactory;
import org.netbeans.modules.maven.embedder.MavenEmbedder;
import org.netbeans.modules.maven.execute.MavenCommandLineExecutor;
import org.netbeans.modules.maven.execute.cmd.ExecMojo;
import org.netbeans.modules.maven.execute.cmd.ExecutionEventObject;
import java.text.SimpleDateFormat;
import java.util.Date;
import org.netbeans.modules.maven.indexer.api.NBVersionInfo;
import org.netbeans.modules.maven.indexer.api.QueryField;
import org.netbeans.modules.maven.indexer.api.RepositoryInfo;
import org.netbeans.modules.maven.indexer.api.RepositoryPreferences;
import org.netbeans.modules.maven.indexer.api.RepositoryQueries;
import org.openide.execution.ExecutionEngine;
import org.openide.execution.ExecutorTask;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import org.openide.modules.InstalledFileLocator;
import org.openide.util.BaseUtilities;
import org.openide.util.NbPreferences;
import uno.anahata.asi.agi.message.RagMessage;
import org.openide.util.Task;
import org.openide.util.TaskListener;
import org.openide.windows.IOProvider;
import uno.anahata.asi.nb.tools.project.NbProjects;
import uno.anahata.asi.agi.tool.AnahataToolkit;
import uno.anahata.asi.nb.util.TeeInputOutput;
import uno.anahata.asi.agi.tool.AgiToolkit;
import uno.anahata.asi.agi.tool.AgiToolParam;
import uno.anahata.asi.agi.tool.AgiTool;
import uno.anahata.asi.toolkit.maven.DeclaredArtifact;
import uno.anahata.asi.toolkit.maven.DependencyGroup;
import uno.anahata.asi.toolkit.maven.DependencyScope;

/**
 * Consolidated "super-tool" class for all Maven-related AI operations.
 * This class combines functionality from the deprecated Maven, MavenPom, and MavenSearch classes.
 * It serves as the single, definitive entry point for searching, modifying, and executing Maven tasks.
 * 
 * @author anahata
 */
@AgiToolkit("A toolkit for using netbeans maven tools.")
@Slf4j
public class Maven extends AnahataToolkit {
    /** Logger instance for the Maven toolkit. */
    private static final Logger LOG = Logger.getLogger(Maven.class.getName());
    /** Maximum number of output lines to capture from a Maven build. */
    private static final int MAX_OUTPUT_LINES = 100;
    /** Maximum character length per output line. */
    private static final int MAX_LINE_LENGTH = 2000;
    /** Default timeout for Maven build execution (5 minutes). */
    private static final long DEFAULT_TIMEOUT_MS = 300_000; // 5 minutes

    /**
     * Default constructor for the Maven toolkit.
     */
    public Maven() {
        
    }

    /** 
     * {@inheritDoc} 
     * <p>Provides context-aware instructions for the Maven toolkit, 
     * specifying the current path to the Maven executable as configured in the IDE.</p> 
     */
    @Override
    public List<String> getSystemInstructions() throws Exception {
        return Collections.singletonList("Maven Command Line: " + getNbCommandLineMavenPath());
    }
    
    /**
     * Gets the path to the Maven installation configured in NetBeans.
     * <p>
     * Accesses the IDE's Maven preferences node to retrieve the standardized 
     * command-line Maven path.
     * </p>
     * 
     * @return the Maven path, or a failure message if not found or execution fails.
     */
    //@AgiTool("Gets the path to the Maven installation configured in NetBeans.")
    public static String getNbCommandLineMavenPath() {
        try {
            Preferences prefs = NbPreferences.root().node("org/netbeans/modules/maven");
            String path = prefs.get("commandLineMavenPath", null);
            if (path != null && !path.isBlank()) {
                return path + " (User Configured)";
            }

            // Fallback to bundled maven detection
            File mavenHome = InstalledFileLocator.getDefault().locate("maven", "org.netbeans.modules.maven", false);
            if (mavenHome != null && mavenHome.exists()) {
                String binPath = "bin/mvn";
                if (BaseUtilities.isWindows()) {
                    binPath = "bin\\mvn.cmd";
                }
                File mvnBin = new File(mavenHome, binPath);
                if (mvnBin.exists()) {
                    return mvnBin.getAbsolutePath() + " (Bundled)";
                } else {
                    return "Couldn't find the mvn executable in NetBeans Maven module's home: " + mavenHome.getAbsolutePath();
                }
            }

            return "PREFERENCE_NOT_FOUND";
        } catch (Throwable t) {
            return "EXECUTION_FAILED: " + t.toString();
        }
    }

    /** 
     * {@inheritDoc} 
     * <p>Populates the RAG message with current NetBeans Maven preferences and detected paths.</p> 
     */
    @Override
    public void populateMessage(RagMessage ragMessage) {
        StringBuilder sb = new StringBuilder(" Nb Preferences for org/netbeans/modules/maven:\n");
        sb.append("(These are the preferences the NetBeans Maven module uses when doing Maven stuff. Anahata's Maven toolkit uses the NetBeans Maven module.\n\n");
        
        Preferences node = NbPreferences.root().node("org/netbeans/modules/maven");
        try {
            String[] keys = node.keys();
            if (keys.length == 0) {
                sb.append("- No preferences set for this node.\n");
            } else {
                for (String key : keys) {
                    sb.append("- **").append(key).append("**: ").append(node.get(key, null)).append("\n");
                }
            }
            
            String cmdPath = node.get("commandLineMavenPath", null);
            if (cmdPath == null || cmdPath.isBlank()) {
                sb.append("\n> [!NOTE]\n");
                sb.append("> `commandLineMavenPath` not set. User is using the mvn version bundled with NetBeans, which is located at: `")
                  .append(getNbCommandLineMavenPath()).append("`\n");
            }
        } catch (Exception ex) {
            sb.append("- Error reading preferences: ").append(ex.getMessage()).append("\n");
        }

        sb.append("\n### Maven Repositories & Index Status\n");
        sb.append("- **Global Indexing Enabled**: ").append(RepositoryPreferences.isIndexRepositories() ? "✅ Yes" : "❌ No").append("\n");
        sb.append("- **Remote Index Download Active**: ").append(RepositoryPreferences.isIndexDownloadEnabledEffective() ? "✅ Yes" : "❌ No");
        if (RepositoryPreferences.isIndexDownloadPaused()) {
            sb.append(" (⏸️ Paused)");
        }
        sb.append("\n\n");

        try {
            List<RepositoryInfo> loaded = RepositoryQueries.getLoadedContexts();
            List<RepositoryInfo> repos = RepositoryPreferences.getInstance().getRepositoryInfos();
            SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");

            sb.append("| Repository ID | Name | Type | Index Loaded | Last Updated | URL / Path |\n");
            sb.append("|---|---|---|---|---|---|\n");
            for (RepositoryInfo repo : repos) {
                boolean hasIndex = loaded.stream().anyMatch(l -> l.getId().equals(repo.getId()));
                Date last = RepositoryPreferences.getLastIndexUpdate(repo.getId());
                String lastStr = (last != null && last.getTime() > 0) ? sdf.format(last) : "Never";
                String loc = repo.isLocal() ? "Local" : "Remote";
                String pathOrUrl = repo.getRepositoryUrl() != null ? repo.getRepositoryUrl() : repo.getRepositoryPath();
                sb.append("| `").append(repo.getId()).append("` | ")
                  .append(repo.getName()).append(" | ")
                  .append(loc).append(" | ")
                  .append(hasIndex ? "✅ Yes" : "❌ No").append(" | ")
                  .append(lastStr).append(" | `")
                  .append(pathOrUrl != null ? pathOrUrl : "").append("` |\n");
            }
        } catch (Exception ex) {
            sb.append("- Error inspecting repositories: ").append(ex.getMessage()).append("\n");
        }
        
        ragMessage.addTextPart(sb.toString());
    }
    
    //<editor-fold defaultstate="collapsed" desc="From MavenSearch.java">
    /**
     * Unified search across configured Maven repositories with structured boolean query clauses, sorting, version collapsing, and classifier resolution.
     * 
     * @param request The Maven search request parameters.
     * @return A paginated page of artifact search results.
     * @throws Exception if an error occurs during search execution.
     */
    @AgiTool("Unified search across configured Maven repositories with structured boolean query clauses, sorting, version collapsing, and classifier resolution.")
    public MavenSearchResultPage searchMaven(
            @AgiToolParam("The Maven search request parameters.") MavenSearchRequest request) throws Exception {

        if (request == null) {
            request = new MavenSearchRequest();
        }

        int start = request.getStartIndex() != null ? Math.max(0, request.getStartIndex()) : 0;
        int size = request.getPageSize() != null ? Math.max(1, request.getPageSize()) : 25;
        boolean collapse = request.getCollapseVersions() != null ? request.getCollapseVersions() : true;
        boolean stableOnly = request.getStableOnly() != null ? request.getStableOnly() : false;
        MavenSearchRequest.SortStrategy sortBy = request.getSortBy() != null ? request.getSortBy() : MavenSearchRequest.SortStrategy.LATEST_VERSION;

        List<RepositoryInfo> repos = RepositoryPreferences.getInstance().getRepositoryInfos();
        String gid = request.getGroupId() != null ? request.getGroupId().trim() : null;
        String aid = request.getArtifactId() != null ? request.getArtifactId().trim() : null;

        List<NBVersionInfo> candidateResults;

        // Fast-path: When both groupId and artifactId are specified and no text query/clause is supplied
        if (gid != null && !gid.isEmpty() && aid != null && !aid.isEmpty() && request.getClause() == null && (request.getQuery() == null || request.getQuery().isBlank())) {
            RepositoryQueries.Result<NBVersionInfo> vRes = RepositoryQueries.getVersionsResult(gid, aid, repos);
            if (vRes.isPartial()) {
                vRes.waitForSkipped();
            }
            candidateResults = vRes.getResults() != null ? new ArrayList<>(vRes.getResults()) : new ArrayList<>();
        } else if (request.getClause() != null) {
            candidateResults = executeClause(request.getClause(), repos);
        } else if (request.getQuery() != null && !request.getQuery().isBlank()) {
            String[] splits = request.getQuery().trim().split("\\s+");
            List<NBVersionInfo> intersected = null;

            for (String curText : splits) {
                List<QueryField> tokenFields = buildTokenFields(curText);
                List<NBVersionInfo> tokenHits = queryFields(tokenFields, repos);

                if (intersected == null) {
                    intersected = new ArrayList<>(tokenHits);
                } else {
                    Set<String> hitKeys = tokenHits.stream()
                            .map(h -> h.getGroupId() + ":" + h.getArtifactId() + ":" + h.getVersion())
                            .collect(Collectors.toSet());
                    intersected.removeIf(c -> !hitKeys.contains(c.getGroupId() + ":" + c.getArtifactId() + ":" + c.getVersion()));
                }
                if (intersected.isEmpty()) {
                    break;
                }
            }
            candidateResults = intersected != null ? intersected : new ArrayList<>();
        } else {
            candidateResults = Collections.emptyList();
        }

        // Apply additional coordinates filters if supplied alongside query/clause
        if (gid != null && !gid.isEmpty()) {
            candidateResults.removeIf(info -> !info.getGroupId().equalsIgnoreCase(gid));
        }
        if (aid != null && !aid.isEmpty()) {
            candidateResults.removeIf(info -> !info.getArtifactId().equalsIgnoreCase(aid));
        }

        // Apply stable-only filter if requested
        if (stableOnly) {
            candidateResults.removeIf(info -> !isStableVersion(info.getVersion()));
        }

        // Collapse versions if requested (keeping only the newest version per groupId:artifactId)
        if (collapse) {
            Map<String, NBVersionInfo> latestByArtifact = new LinkedHashMap<>();
            for (NBVersionInfo info : candidateResults) {
                String key = info.getGroupId() + ":" + info.getArtifactId();
                NBVersionInfo existing = latestByArtifact.get(key);
                if (existing == null || info.compareToWithoutRepoId(existing) < 0) {
                    latestByArtifact.put(key, info);
                }
            }
            candidateResults = new ArrayList<>(latestByArtifact.values());
        }

        // Apply sorting strategy
        if (sortBy == MavenSearchRequest.SortStrategy.LATEST_VERSION) {
            candidateResults.sort(NBVersionInfo::compareTo);
        } else if (sortBy == MavenSearchRequest.SortStrategy.COORDINATES) {
            candidateResults.sort((a, b) -> {
                int c = a.getGroupId().compareToIgnoreCase(b.getGroupId());
                if (c != 0) {
                    return c;
                }
                c = a.getArtifactId().compareToIgnoreCase(b.getArtifactId());
                if (c != 0) {
                    return c;
                }
                return a.compareTo(b);
            });
        } else if (sortBy == MavenSearchRequest.SortStrategy.RELEVANCE) {
            candidateResults.sort((a, b) -> Float.compare(b.getLuceneScore(), a.getLuceneScore()));
        }

        int totalCount = candidateResults.size();
        int toIndex = Math.min(totalCount, start + size);
        List<NBVersionInfo> pagedSlice = (start < totalCount) ? candidateResults.subList(start, toIndex) : Collections.emptyList();

        List<MavenArtifactSearchResult> pageResults = new ArrayList<>();
        for (NBVersionInfo info : pagedSlice) {
            List<String> classifiers = resolveClassifiers(info, repos);
            MavenArtifactSearchResult item = new MavenArtifactSearchResult(
                    info.getGroupId(),
                    info.getArtifactId(),
                    info.getVersion(),
                    info.getRepoId(),
                    info.getPackaging(),
                    info.getProjectDescription(),
                    classifiers,
                    collapse
            );
            pageResults.add(item);
        }

        return new MavenSearchResultPage(start, totalCount, pageResults);
    }

    /**
     * Executes a structured {@link MavenQueryClause} against the configured Maven repository indexes.
     * 
     * @param clause The query clause to evaluate.
     * @param repos The repositories to query.
     * @return The list of matching version records.
     */
    private static List<NBVersionInfo> executeClause(MavenQueryClause clause, List<RepositoryInfo> repos) {
        if (clause == null) {
            return Collections.emptyList();
        }
        if (clause.getSubClauses() != null && !clause.getSubClauses().isEmpty()) {
            List<NBVersionInfo> composite = null;
            for (MavenQueryClause sub : clause.getSubClauses()) {
                List<NBVersionInfo> subHits = executeClause(sub, repos);
                if (composite == null) {
                    composite = new ArrayList<>(subHits);
                } else if (clause.getOccur() == MavenQueryClause.Occur.SHOULD) {
                    Set<String> existing = composite.stream()
                            .map(h -> h.getGroupId() + ":" + h.getArtifactId() + ":" + h.getVersion())
                            .collect(Collectors.toSet());
                    for (NBVersionInfo h : subHits) {
                        if (existing.add(h.getGroupId() + ":" + h.getArtifactId() + ":" + h.getVersion())) {
                            composite.add(h);
                        }
                    }
                } else {
                    Set<String> subKeys = subHits.stream()
                            .map(h -> h.getGroupId() + ":" + h.getArtifactId() + ":" + h.getVersion())
                            .collect(Collectors.toSet());
                    composite.removeIf(c -> !subKeys.contains(c.getGroupId() + ":" + c.getArtifactId() + ":" + c.getVersion()));
                }
            }
            return composite != null ? composite : Collections.emptyList();
        }

        String val = clause.getValue();
        if (val == null || val.isBlank()) {
            return Collections.emptyList();
        }

        List<QueryField> fields = new ArrayList<>();
        int matchType = clause.getMatch() == MavenQueryClause.Match.EXACT ? QueryField.MATCH_EXACT : QueryField.MATCH_ANY;
        int occurType = clause.getOccur() == MavenQueryClause.Occur.MUST ? QueryField.OCCUR_MUST : QueryField.OCCUR_SHOULD;

        MavenQueryClause.TargetField tf = clause.getField() != null ? clause.getField() : MavenQueryClause.TargetField.ALL;
        if (tf == MavenQueryClause.TargetField.ALL) {
            List<String> allFields = List.of(
                QueryField.FIELD_GROUPID, QueryField.FIELD_ARTIFACTID,
                QueryField.FIELD_NAME, QueryField.FIELD_DESCRIPTION, QueryField.FIELD_CLASSES
            );
            for (String fld : allFields) {
                QueryField qf = new QueryField();
                qf.setField(fld);
                qf.setValue(val.trim());
                qf.setMatch(matchType);
                qf.setOccur(QueryField.OCCUR_SHOULD);
                fields.add(qf);
            }
        } else {
            QueryField qf = new QueryField();
            qf.setField(toQueryFieldName(tf));
            qf.setValue(val.trim());
            qf.setMatch(matchType);
            qf.setOccur(occurType);
            fields.add(qf);
        }

        return queryFields(fields, repos);
    }

    /**
     * Maps a {@link MavenQueryClause.TargetField} enum to the internal NetBeans {@link QueryField} string constant.
     * 
     * @param targetField The target field enum.
     * @return The corresponding NetBeans QueryField constant.
     */
    private static String toQueryFieldName(MavenQueryClause.TargetField targetField) {
        if (targetField == null) {
            return QueryField.FIELD_ANY;
        }
        return switch (targetField) {
            case GROUP_ID -> QueryField.FIELD_GROUPID;
            case ARTIFACT_ID -> QueryField.FIELD_ARTIFACTID;
            case VERSION -> QueryField.FIELD_VERSION;
            case PACKAGING -> QueryField.FIELD_PACKAGING;
            case NAME -> QueryField.FIELD_NAME;
            case DESCRIPTION -> QueryField.FIELD_DESCRIPTION;
            case CLASSES -> QueryField.FIELD_CLASSES;
            case ALL -> QueryField.FIELD_ANY;
        };
    }

    /**
     * Executes a list of {@link QueryField}s against the specified repository indexes and waits for results.
     * 
     * @param fields The list of query fields.
     * @param repos The repositories to query.
     * @return The list of matching version records.
     */
    private static List<NBVersionInfo> queryFields(List<QueryField> fields, List<RepositoryInfo> repos) {
        RepositoryQueries.Result<NBVersionInfo> results = RepositoryQueries.findResult(fields, repos);
        if (results.isPartial()) {
            results.waitForSkipped();
        }
        return results.getResults() != null ? new ArrayList<>(results.getResults()) : new ArrayList<>();
    }

    /**
     * Builds a list of {@link QueryField} objects for searching across all standard metadata fields for a single token.
     * 
     * @param token The search term.
     * @return The list of query fields configured with SHOULD occurrence.
     */
    private static List<QueryField> buildTokenFields(String token) {
        List<QueryField> fields = new ArrayList<>();
        List<String> fStrings = List.of(
            QueryField.FIELD_GROUPID, QueryField.FIELD_ARTIFACTID,
            QueryField.FIELD_NAME, QueryField.FIELD_DESCRIPTION, QueryField.FIELD_CLASSES
        );
        for (String fld : fStrings) {
            QueryField f = new QueryField();
            f.setField(fld);
            f.setValue(token);
            f.setMatch(QueryField.MATCH_ANY);
            f.setOccur(QueryField.OCCUR_SHOULD);
            fields.add(f);
        }
        return fields;
    }

    /**
     * Determines whether a Maven artifact version string represents a stable release.
     * 
     * @param version The version string to evaluate.
     * @return true if the version does not contain alpha, beta, rc, milestone, preview, or snapshot markers.
     */
    private static boolean isStableVersion(String version) {
        if (version == null) {
            return false;
        }
        String lower = version.toLowerCase(Locale.ENGLISH);
        return !lower.contains("alpha") && !lower.contains("beta") && !lower.contains("rc")
                && !lower.contains("snapshot") && !lower.contains("preview") && !lower.contains("-m")
                && !lower.matches(".*-(ea|cr|b)\\d+.*");
    }

    /**
     * Resolves all available classifiers (including sources and javadoc) for a given artifact version.
     * 
     * @param info The artifact version information.
     * @param repos The repositories to inspect.
     * @return A sorted list of available classifiers, or null if none are present.
     */
    private static List<String> resolveClassifiers(NBVersionInfo info, List<RepositoryInfo> repos) {
        List<String> classifiers = new ArrayList<>();
        try {
            RepositoryQueries.Result<NBVersionInfo> recordsRes = RepositoryQueries.getRecordsResult(
                    info.getGroupId(), info.getArtifactId(), info.getVersion(), repos);
            if (recordsRes != null && recordsRes.getResults() != null) {
                for (NBVersionInfo rec : recordsRes.getResults()) {
                    String cls = rec.getClassifier();
                    if (cls != null && !cls.isBlank() && !classifiers.contains(cls)) {
                        classifiers.add(cls);
                    }
                }
            }
        } catch (Exception e) {
            LOG.log(Level.FINE, "Failed to resolve record classifiers for {0}:{1}:{2}", new Object[]{info.getGroupId(), info.getArtifactId(), info.getVersion()});
        }

        if (info.isSourcesExists() && !classifiers.contains("sources")) {
            classifiers.add("sources");
        }
        if (info.isJavadocExists() && !classifiers.contains("javadoc")) {
            classifiers.add("javadoc");
        }
        Collections.sort(classifiers);
        return classifiers.isEmpty() ? null : classifiers;
    }
    //</editor-fold>

    //<editor-fold defaultstate="collapsed" desc="From MavenPom.java">
    /**
     * Resolves a Maven property expression (e.g. '${netbeans.version}' or '${project.version}')
     * against the active Maven project model, including all inherited parent POM and profile properties.
     * 
     * @param project The open NetBeans project.
     * @param version The literal version string or property expression.
     * @return The evaluated version string if resolved, or the original version string.
     */
    private static String resolvePropertyVersion(Project project, String version) {
        if (version == null || version.isBlank() || !version.startsWith("${") || !version.endsWith("}")) {
            return version;
        }
        try {
            NbMavenProject nbMavenProject = project.getLookup().lookup(NbMavenProject.class);
            if (nbMavenProject != null && nbMavenProject.getMavenProject() != null) {
                org.apache.maven.project.MavenProject mp = nbMavenProject.getMavenProject();
                String propName = version.substring(2, version.length() - 1).trim();

                if ("project.version".equals(propName) || "version".equals(propName)) {
                    return mp.getVersion();
                } else if ("project.groupId".equals(propName) || "groupId".equals(propName)) {
                    return mp.getGroupId();
                } else if ("project.artifactId".equals(propName) || "artifactId".equals(propName)) {
                    return mp.getArtifactId();
                } else {
                    String resolved = mp.getProperties().getProperty(propName);
                    if (resolved != null && !resolved.isBlank()) {
                        return resolved.trim();
                    }
                }
            }
        } catch (Exception e) {
            LOG.log(Level.FINE, "Could not resolve property version: " + version, e);
        }
        return version;
    }

    /**
     * Finds the managed dependency in the project's effective dependencyManagement, if present.
     * 
     * @param project The open NetBeans project.
     * @param groupId The groupId of the dependency.
     * @param artifactId The artifactId of the dependency.
     * @return The managed Dependency from the MavenProject model, or null if not managed.
     */
    private static Dependency findManagedDependency(Project project, String groupId, String artifactId) {
        try {
            NbMavenProject nbMavenProject = project.getLookup().lookup(NbMavenProject.class);
            if (nbMavenProject != null && nbMavenProject.getMavenProject() != null) {
                org.apache.maven.model.DependencyManagement dm = nbMavenProject.getMavenProject().getDependencyManagement();
                if (dm != null && dm.getDependencies() != null) {
                    for (Dependency d : dm.getDependencies()) {
                        if (groupId.equalsIgnoreCase(d.getGroupId()) && artifactId.equalsIgnoreCase(d.getArtifactId())) {
                            return d;
                        }
                    }
                }
            }
        } catch (Exception e) {
            LOG.log(Level.FINE, "Failed to inspect dependencyManagement", e);
        }
        return null;
    }

    /**
     * The definitive 'super-tool' for adding a Maven dependency.
     * <p>This tool follows a safe, multi-phase process:</p>
     * <ol>
     *   <li><b>Pre-flight:</b> Verifies artifact existence in remote repositories (resolving properties like {@code ${netbeans.version}}).</li>
     *   <li><b>Modification:</b> Atomically adds the dependency to the project's {@code pom.xml} using NetBeans POM Model, with optional relative positioning and comments.</li>
     *   <li><b>Resolution:</b> Runs {@code dependency:resolve} to ensure transitive dependencies are satisfied.</li>
     *   <li><b>Background:</b> Triggers asynchronous download of sources and javadocs.</li>
     * </ol>
     * <p>Finally, it triggers a NetBeans project reload to reflect changes in the IDE.</p>
     * 
     * @param projectPath The absolute path of the project to modify.
     * @param groupId The groupId of the dependency.
     * @param artifactId The artifactId of the dependency.
     * @param version The version of the dependency (supports property expressions like '${netbeans.version}', or null if managed by dependencyManagement). If omitted, the managed version is used. If specified and matches dependencyManagement, the redundant &lt;version&gt; tag is automatically omitted from pom.xml to prevent IDE warnings. If specified and different, it will explicitly override the managed version.
     * @param scope The scope of the dependency (e.g., 'compile', 'test'). If null, defaults to 'compile'.
     * @param classifier The classifier of the dependency (e.g., 'jdk17'). Can be null.
     * @param type The type of the dependency (e.g., 'test-jar'). If null, defaults to 'jar'.
     * @param beforeDependency Optional artifactId of an existing dependency to insert this dependency BEFORE.
     * @param afterDependency Optional artifactId of an existing dependency to insert this dependency AFTER.
     * @param comment Optional descriptive XML comment to place directly above the dependency in pom.xml.
     * @return an AddDependencyResult object containing the outcome of each phase.
     */
    @AgiTool("The definitive 'super-tool' for adding a Maven dependency. It follows a safe, multi-phase process, supports property versions like ${netbeans.version}, optional relative positioning, and XML comments.")
    public AddDependencyResult addDependency(
            @AgiToolParam("The absolute path of the project to modify.") String projectPath,
            @AgiToolParam("The groupId of the dependency.") String groupId,
            @AgiToolParam("The artifactId of the dependency.") String artifactId,
            @AgiToolParam(value = "The version of the dependency (supports property expressions like '${netbeans.version}', or null if managed by dependencyManagement). If omitted, the managed version is used. If specified and matches dependencyManagement, the redundant <version> tag is automatically omitted from pom.xml to prevent IDE warnings. If specified and different, it will explicitly override the managed version.", required = false) String version,
            @AgiToolParam(value = "The scope of the dependency (e.g., 'compile', 'test'). If null, defaults to 'compile'.", required = false) String scope,
            @AgiToolParam(value = "The classifier of the dependency (e.g., 'jdk17'). Can be null.", required = false) String classifier,
            @AgiToolParam(value = "The type of the dependency (e.g., 'test-jar'). If null, defaults to 'jar'.", required = false) String type,
            @AgiToolParam(value = "Optional artifactId of an existing dependency to insert this dependency BEFORE.", required = false) String beforeDependency,
            @AgiToolParam(value = "Optional artifactId of an existing dependency to insert this dependency AFTER.", required = false) String afterDependency,
            @AgiToolParam(value = "Optional descriptive XML comment to place directly above the dependency in pom.xml.", required = false) String comment) {
        
        AddDependencyResult.AddDependencyResultBuilder resultBuilder = AddDependencyResult.builder();
        StringBuilder summary = new StringBuilder();

        try {
            Project project = NbProjects.findOpenProject(projectPath);
            String effectiveScope = (scope == null || scope.isBlank()) ? null : scope.trim();
            String effectiveType = (type == null || type.isBlank()) ? null : type.trim();
            String effectiveClassifier = (classifier == null || classifier.isBlank() || "jar".equalsIgnoreCase(classifier.trim())) ? null : classifier.trim();

            Dependency managedDep = findManagedDependency(project, groupId, artifactId);
            String preflightVersion;
            final boolean versionOmittedBecauseManaged;

            if (version != null && !version.isBlank()) {
                preflightVersion = resolvePropertyVersion(project, version);
                if (managedDep != null && managedDep.getVersion() != null) {
                    String resolvedManagedVersion = resolvePropertyVersion(project, managedDep.getVersion());
                    if (preflightVersion.equals(resolvedManagedVersion) || version.trim().equals(managedDep.getVersion().trim())) {
                        versionOmittedBecauseManaged = true;
                        log("Dependency " + groupId + ":" + artifactId + " is managed by dependencyManagement (" + resolvedManagedVersion + "). Redundant <version> will be omitted from pom.xml.");
                    } else {
                        versionOmittedBecauseManaged = false;
                    }
                } else {
                    versionOmittedBecauseManaged = false;
                }
            } else if (managedDep != null && managedDep.getVersion() != null) {
                preflightVersion = resolvePropertyVersion(project, managedDep.getVersion());
                versionOmittedBecauseManaged = true;
                log("Dependency " + groupId + ":" + artifactId + " resolved from dependencyManagement: " + preflightVersion);
            } else {
                summary.append("Phase 1: Pre-flight check...\n");
                summary.append("Result: FAILED. Version was not specified and the dependency is not managed by dependencyManagement.");
                error("Version was not specified and the dependency is not managed by dependencyManagement.");
                return resultBuilder.summary(summary.toString()).build();
            }

            log("Pre-flight check: verifying " + groupId + ":" + artifactId + ":" + preflightVersion + (version != null && !preflightVersion.equals(version) ? " (resolved from " + version + ")" : ""));

            // Phase 1: Pre-flight Check
            summary.append("Phase 1: Pre-flight check...\n");
            boolean preflightSuccess = downloadDependencyArtifact(projectPath, groupId, artifactId, preflightVersion, effectiveClassifier, effectiveType);
            resultBuilder.preflightCheckSuccess(preflightSuccess);

            if (!preflightSuccess) {
                summary.append("Result: FAILED. Main artifact could not be resolved. pom.xml was not modified.");
                error("Result: FAILED. Main artifact could not be resolved. pom.xml was not modified.");
                return resultBuilder.summary(summary.toString()).build();
            }
            summary.append("Result: SUCCESS. Main artifact found.\n\n");

            // Phase 2: Modifying pom.xml...
            summary.append("Phase 2: Modifying pom.xml...\n");
            FileObject pom = project.getProjectDirectory().getFileObject("pom.xml");
            if (pom == null) {
                summary.append("Result: FAILED. Could not find pom.xml.");
                error("Could not find pom.xml");
                return resultBuilder.pomModificationSuccess(false).summary(summary.toString()).build();
            }

            ModelOperation<POMModel> operation = new ModelOperation<>() {
                @Override
                public void performOperation(POMModel model) {
                    var project = model.getProject();
                    var existingDeps = project.getDependencies();
                    var existingDep = project.findDependencyById(groupId, artifactId, null);
                    boolean isNew = (existingDep == null);

                    final var dep = isNew
                            ? model.getFactory().createDependency()
                            : existingDep;

                    if (isNew) {
                        dep.setGroupId(groupId);
                        dep.setArtifactId(artifactId);
                    }

                    if (versionOmittedBecauseManaged) {
                        dep.setVersion(null);
                    } else if (version != null && !version.isBlank()) {
                        dep.setVersion(version);
                    }
                    if (effectiveScope != null) {
                        dep.setScope(effectiveScope);
                    }
                    if (effectiveType != null && !"jar".equals(effectiveType)) {
                        dep.setType(effectiveType);
                    }
                    if (effectiveClassifier != null) {
                        dep.setClassifier(effectiveClassifier);
                    }

                    String anchorArtifact = (beforeDependency != null && !beforeDependency.isBlank())
                            ? beforeDependency.trim()
                            : (afterDependency != null && !afterDependency.isBlank() ? afterDependency.trim() : null);

                    var anchorDep = (anchorArtifact != null && !existingDeps.isEmpty())
                            ? existingDeps.stream()
                                    .filter(d -> anchorArtifact.equalsIgnoreCase(d.getArtifactId()) && !d.equals(dep))
                                    .findFirst()
                                    .orElse(null)
                            : null;

                    AbstractDocumentModel adm = (AbstractDocumentModel) model;

                    if (isNew) {
                        if (anchorDep != null) {
                            int anchorIndex = existingDeps.indexOf(anchorDep);
                            int targetIndex = (beforeDependency != null && !beforeDependency.isBlank()) ? anchorIndex : anchorIndex + 1;
                            Component parentComp = ((Component) anchorDep).getParent();
                            adm.addChildComponent(parentComp, (Component) dep, targetIndex);
                            log("Inserted dependency " + (beforeDependency != null && !beforeDependency.isBlank() ? "before: " : "after: ") + anchorArtifact);
                        } else if (!existingDeps.isEmpty()) {
                            Component parentComp = ((Component) existingDeps.get(0)).getParent();
                            adm.addChildComponent(parentComp, (Component) dep, existingDeps.size());
                            log("Appended dependency to existing dependencies list.");
                        } else {
                            project.addDependency(dep);
                            log("Added dependency to project.");
                        }
                    }

                    if (comment != null && !comment.isBlank()) {
                        try {
                            Component parentComp = ((Component) dep).getParent();
                            if (parentComp instanceof AbstractDocumentComponent adc) {
                                DocumentModelAccess access = adm.getAccess();
                                Comment commentNode = model.getDocument().createComment("<!-- " + comment.trim() + " -->");
                                Element depPeer = ((AbstractDocumentComponent) dep).getPeer();
                                access.insertBefore(adc.getPeer(), commentNode, depPeer, adc);
                                log("Inserted XML comment above dependency: " + comment.trim());
                            }
                        } catch (Exception e) {
                            error("Failed to insert comment: " + e.getMessage(), e);
                            LOG.log(Level.FINE, "Failed to insert comment", e);
                        }
                    }
                }
            };
            Utilities.performPOMModelOperations(pom, List.of(operation));

            resultBuilder.pomModificationSuccess(true);
            summary.append("Result: SUCCESS. Dependency added to pom.xml.\n\n");

            // Phase 3: Transitive Dependencies
            summary.append("Phase 3: Resolving transitive dependencies...\n");
            
            MavenBuildResult resolveResult = runGoals(projectPath, Collections.singletonList("dependency:resolve"), null, null, null, null);
            resultBuilder.dependencyResolveResult(resolveResult);
            log("resolve result: " + resolveResult);
            summary.append("Result: 'dependency:resolve' goal executed. See MavenBuildResult for details.\n\n");

            // Phase 4: Asynchronous Source/Javadoc Download
            summary.append("Phase 4: Triggering async download of sources and javadocs...\n");
            getExecutorService().submit(() -> {
                try {
                    downloadProjectDependencies(projectPath, Arrays.asList("sources", "javadoc"));
                } catch (Exception e) {
                    LOG.log(Level.WARNING, "Error during async source/javadoc download", e);
                }
            });
            resultBuilder.asyncDownloadsLaunched(true);
            summary.append("Result: Background download task launched.\n");

            // Final Step: Reload Project
            NbMavenProject.fireMavenProjectReload(project);
            summary.append("Project reload triggered.");

            return resultBuilder.summary(summary.toString()).build();

        } catch (Exception e) {
            summary.append("\nFATAL ERROR: An unexpected exception occurred: ").append(e.getMessage());
            error("FATAL ERROR: An unexpected exception occurred " + e.getMessage());            
            LOG.log(Level.SEVERE, "Add dependency failed", e);
            return resultBuilder.summary(summary.toString()).build();
        }
    }

    /**
     * Gets the list of dependencies directly declared in the pom.xml.
     * @param projectPath The absolute path of the project to analyze.
     * @return a list of DependencyScope objects.
     * @throws Exception if an error occurs.
     */
    @AgiTool("Gets the list of dependencies directly declared in the pom.xml, grouped by scope and groupId for maximum token efficiency.")
    public static List<DependencyScope> getDeclaredDependencies(
            @AgiToolParam("The absolute path of the project to analyze.") String projectPath) throws Exception {
        
        Project project = NbProjects.findOpenProject(projectPath);
        NbMavenProject nbMavenProject = project.getLookup().lookup(NbMavenProject.class);
        List<Dependency> dependencies = nbMavenProject.getMavenProject().getDependencies();
        return groupDeclaredDependencies(dependencies);
    }

    /**
     * Gets the final, fully resolved list of transitive dependencies for the project.
     * @param projectPath The absolute path of the project to analyze.
     * @return a list of ResolvedDependencyScope objects.
     * @throws Exception if an error occurs.
     */
    @AgiTool("Gets the final, fully resolved list of transitive dependencies for the project, representing the actual runtime classpath. The output is in an ultra-compact format (List<ResolvedDependencyScope>) for maximum token efficiency.")
    public List<ResolvedDependencyScope> getResolvedDependencies(
            @AgiToolParam("The absolute path of the project to analyze.") String projectPath) throws Exception {

        Project project = NbProjects.findOpenProject(projectPath);
        NbMavenProject nbMavenProject = project.getLookup().lookup(NbMavenProject.class);
        Collection<Artifact> artifacts = nbMavenProject.getMavenProject().getArtifacts();
        return groupResolvedArtifacts(artifacts);
    }

    /**
     * Groups a list of flat Maven dependencies into a hierarchical structure by scope and groupId.
     * 
     * @param dependencies The flat list of Maven dependencies.
     * @return A list of DependencyScope objects representing the grouped structure.
     */
    private static List<DependencyScope> groupDeclaredDependencies(List<Dependency> dependencies) {
        Map<String, List<Dependency>> dependenciesByScope = dependencies.stream()
                .collect(Collectors.groupingBy(dep -> dep.getScope() == null ? "compile" : dep.getScope()));

        List<DependencyScope> result = new ArrayList<>();

        for (Map.Entry<String, List<Dependency>> scopeEntry : dependenciesByScope.entrySet()) {
            String scope = scopeEntry.getKey();
            List<Dependency> depsInScope = scopeEntry.getValue();

            Map<String, List<Dependency>> dependenciesByGroup = depsInScope.stream()
                    .collect(Collectors.groupingBy(Dependency::getGroupId));

            List<DependencyGroup> dependencyGroups = new ArrayList<>();
            for (Map.Entry<String, List<Dependency>> groupEntry : dependenciesByGroup.entrySet()) {
                String groupId = groupEntry.getKey();
                List<Dependency> depsInGroup = groupEntry.getValue();

                List<DeclaredArtifact> declaredArtifacts = new ArrayList<>();
                for (Dependency dep : depsInGroup) {
                    StringBuilder artifactBuilder = new StringBuilder();
                    artifactBuilder.append(dep.getArtifactId()).append(':').append(dep.getVersion());
                    if (dep.getClassifier() != null && !dep.getClassifier().isEmpty()) {
                        artifactBuilder.append(':').append(dep.getClassifier());
                    }
                    if (dep.getType() != null && !dep.getType().equals("jar")) {
                        artifactBuilder.append(':').append(dep.getType());
                    }

                    List<String> exclusions = null;
                    if (dep.getExclusions() != null && !dep.getExclusions().isEmpty()) {
                        exclusions = dep.getExclusions().stream()
                                .map(ex -> ex.getGroupId() + ":" + ex.getArtifactId())
                                .collect(Collectors.toList());
                    }
                    
                    declaredArtifacts.add(new DeclaredArtifact(artifactBuilder.toString(), exclusions));
                }
                dependencyGroups.add(new DependencyGroup(groupId, declaredArtifacts));
            }
            result.add(new DependencyScope(scope, dependencyGroups));
        }
        
        return result;
    }
    
    /**
     * Groups a collection of resolved Maven artifacts into a hierarchical structure by scope and groupId.
     * 
     * @param artifacts The collection of resolved artifacts.
     * @return A list of ResolvedDependencyScope objects representing the grouped structure.
     */
    private static List<ResolvedDependencyScope> groupResolvedArtifacts(Collection<Artifact> artifacts) {
        Map<String, List<Artifact>> artifactsByScope = artifacts.stream()
                .collect(Collectors.groupingBy(art -> art.getScope() == null ? "compile" : art.getScope()));

        List<ResolvedDependencyScope> result = new ArrayList<>();

        for (Map.Entry<String, List<Artifact>> scopeEntry : artifactsByScope.entrySet()) {
            String scope = scopeEntry.getKey();
            List<Artifact> artifactsInScope = scopeEntry.getValue();

            Map<String, List<Artifact>> artifactsByGroup = artifactsInScope.stream()
                    .collect(Collectors.groupingBy(Artifact::getGroupId));

            List<ResolvedDependencyGroup> dependencyGroups = new ArrayList<>();
            for (Map.Entry<String, List<Artifact>> groupEntry : artifactsByGroup.entrySet()) {
                String groupId = groupEntry.getKey();
                List<Artifact> artifactsInGroup = groupEntry.getValue();

                List<String> compactArtifacts = new ArrayList<>();
                for (Artifact art : artifactsInGroup) {
                    StringBuilder artifactBuilder = new StringBuilder();
                    artifactBuilder.append(art.getArtifactId()).append(':').append(art.getVersion());
                    if (art.getClassifier() != null && !art.getClassifier().isEmpty()) {
                        artifactBuilder.append(':').append(art.getClassifier());
                    }
                    if (art.getType() != null && !art.getType().equals("jar")) {
                        artifactBuilder.append(':').append(art.getType());
                    }
                    
                    compactArtifacts.add(artifactBuilder.toString());
                }
                dependencyGroups.add(new ResolvedDependencyGroup(groupId, compactArtifacts));
            }
            result.add(new ResolvedDependencyScope(scope, dependencyGroups));
        }
        
        return result;
    }
    //</editor-fold>

    //<editor-fold defaultstate="collapsed" desc="From Maven.java">
    
    /**
     * Executes a list of Maven goals on a Project synchronously.
     * @param projectPath The ID of the project to run the goals on.
     * @param goals A list of Maven goals to execute.
     * @param profiles A list of profiles to activate.
     * @param properties A map of properties to set.
     * @param options A list of additional Maven options.
     * @param timeout The maximum time to wait for the build to complete.
     * @return a MavenBuildResult object.
     * @throws Exception if an error occurs.
     */
    @AgiTool(value = "Executes a list of Maven goals on a Project synchronously (waits for the build to finish), capturing the last " + MAX_OUTPUT_LINES + " lines of the output.")
    public MavenBuildResult runGoals(
            @AgiToolParam("The full path of the project to run the goals on.") String projectPath,
            @AgiToolParam("A list of Maven goals to execute (e.g., ['clean', 'install']).") List<String> goals,
            @AgiToolParam("A list of profiles to activate.") List<String> profiles,
            @AgiToolParam("A map of properties to set.") Map<String, String> properties,
            @AgiToolParam("A list of additional Maven options.") List<String> options,
            @AgiToolParam("The maximum time to wait for the build to complete, in milliseconds.") Long timeout) throws Exception {

        Project project = NbProjects.findOpenProject(projectPath);
        
        long effectiveTimeout = timeout != null ? timeout : DEFAULT_TIMEOUT_MS;
        
        ProjectInformation info = ProjectUtils.getInformation(project);
        String displayName = info.getDisplayName();
        String goalsString = String.join(" ", goals);
        String tabTitle = "Anahata - " + displayName + " (" + goalsString + ")";

        List<String> commandLine = new ArrayList<>(goals);
        if (options != null) {
            commandLine.addAll(options);
        }

        RunConfig config = RunUtils.createRunConfig(
                FileUtil.toFile(project.getProjectDirectory()),
                project,
                tabTitle,
                commandLine
        );

        if (profiles != null && !profiles.isEmpty()) {
            config.setActivatedProfiles(profiles);
        }

        if (properties != null) {
            properties.forEach(config::setProperty);
        }

        TeeInputOutput teeIO = new TeeInputOutput(IOProvider.getDefault().getIO(config.getTaskDisplayName(), true));
        MavenCommandLineExecutor executor = new MavenCommandLineExecutor(config, teeIO, null);

        LOG.info("Executing Maven build via ExecutionEngine to avoid RunUtils deadlock...");
        ExecutorTask task = ExecutionEngine.getDefault().execute(
            config.getTaskDisplayName(),
            executor,
            teeIO
        );
        executor.setTask(task);
        LOG.info("Task launched. Attaching SAFE listener.");

        CompletableFuture<Integer> future = new CompletableFuture<>();
        task.addTaskListener(new TaskListener() {
            @Override
            public void taskFinished(Task finishedTask) {
                LOG.info("SAFE LISTENER: Task finished.");
                int taskResult = -1; // Default to error
                if (finishedTask instanceof ExecutorTask) {
                    taskResult = ((ExecutorTask) finishedTask).result();
                } else {
                    LOG.log(Level.WARNING, "Task finished, but it was not an ExecutorTask. Cannot get exit code. Task type: {0}", finishedTask.getClass().getName());
                }
                future.complete(taskResult);
                finishedTask.removeTaskListener(this);
            }
        });

        MavenBuildResult.ProcessStatus status;
        Integer exitCode = null;

        try {
            exitCode = future.get(effectiveTimeout, TimeUnit.MILLISECONDS);
            status = MavenBuildResult.ProcessStatus.COMPLETED;
        } catch (TimeoutException e) {
            task.stop();
            status = MavenBuildResult.ProcessStatus.TIMEOUT;
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            task.stop();
            status = MavenBuildResult.ProcessStatus.INTERRUPTED;
        } catch (Exception e) {
            task.stop();
            status = MavenBuildResult.ProcessStatus.COMPLETED; // The future completed, but with an exception.
            exitCode = -1; // Generic error code
            LOG.log(Level.WARNING, "Error while waiting for build future", e);
        }

        String capturedOutput = teeIO.getCapturedOutput();
        String capturedError = teeIO.getCapturedError();
        String fullLogContent = "--- STDOUT ---\n" + capturedOutput + "\n\n--- STDERR ---\n" + capturedError;
        String logFilePath = null;

        File tempLogFile = File.createTempFile("anahata-maven-build-", ".log");
        try (PrintWriter out = new PrintWriter(new FileWriter(tempLogFile))) {
            out.println(fullLogContent);
        }
        logFilePath = tempLogFile.getAbsolutePath();
        
        int totalLines = (int) capturedOutput.lines().count();
        int startIndex = Math.max(0, totalLines - MAX_OUTPUT_LINES);
        
        // Ported from V1: Detailed build summary
        List<MavenBuildResult.BuildPhase> phases = extractBuildPhases(executor);

        return new MavenBuildResult(status, exitCode, capturedOutput, capturedError, logFilePath, phases);
    }

    /**
     * Extracts the build phases and their outcomes from the Maven executor using reflection.
     * 
     * @param executor The Maven executor.
     * @return A list of build phases.
     */
    private static List<MavenBuildResult.BuildPhase> extractBuildPhases(MavenCommandLineExecutor executor) {
        List<MavenBuildResult.BuildPhase> phases = new ArrayList<>();
        try {
            // Path: executor -> tabContext -> overview -> root
            Field tabContextField = executor.getClass().getSuperclass().getDeclaredField("tabContext");
            tabContextField.setAccessible(true);
            Object tabContext = tabContextField.get(executor);
            if (tabContext == null) return phases;

            Field overviewField = tabContext.getClass().getDeclaredField("overview");
            overviewField.setAccessible(true);
            Object overview = overviewField.get(tabContext);
            if (overview == null) return phases;

            Field rootField = overview.getClass().getDeclaredField("root");
            rootField.setAccessible(true);
            ExecutionEventObject.Tree tree = (ExecutionEventObject.Tree) rootField.get(overview);
            
            if (tree != null) {
                collectPhases(tree, phases);
            }
        } catch (Exception e) {
            LOG.log(Level.FINE, "Could not extract build phases via reflection", e);
        }
        return phases;
    }

    /**
     * Recursively collects build phases from the execution event tree.
     * 
     * @param node The current node in the tree.
     * @param phases The list to populate.
     */
    private static void collectPhases(ExecutionEventObject.Tree node, List<MavenBuildResult.BuildPhase> phases) {
        ExecutionEventObject start = node.getStartEvent();
        ExecutionEventObject end = node.getEndEvent();

        if (start instanceof ExecMojo) {
            ExecMojo mojo = (ExecMojo) start;
            boolean success = false;
            if (end != null) {
                success = ExecutionEvent.Type.MojoSucceeded.equals(end.type);
            }
            phases.add(new MavenBuildResult.BuildPhase(
                mojo.phase, 
                mojo.plugin.getId() + ":" + mojo.goal, 
                success, 
                0 // Duration calculation would require timestamps from events
            ));
        }

        for (ExecutionEventObject.Tree child : node.getChildrenNodes()) {
            collectPhases(child, phases);
        }
    }
    
    /**
     * Downloads all missing dependencies artifacts for a given Maven project.
     * @param projectPath The absolute path of the project to download dependencies for.
     * @param classifiers A list of classifiers to download.
     * @return a message indicating the result of the operation.
     * @throws java.lang.Exception if an error occurs.
     */
    @AgiTool("Downloads all missing dependencies artifacts (e.g., 'sources', 'javadoc') for a given Maven project's dependencies.")
        public String downloadProjectDependencies(
                @AgiToolParam("The absolute path of the project to download dependencies for.") String projectPath,
                @AgiToolParam("A list of classifiers to download (e.g., ['sources', 'javadoc']).") List<String> classifiers) throws Exception {
        Project project = NbProjects.findOpenProject(projectPath);
        NbMavenProject nbMavenProject = project.getLookup().lookup(NbMavenProject.class);
        if (nbMavenProject == null) {
            throw new IllegalStateException("Project '" + projectPath + "' is not a Maven project or could not be found.");
        }

        MavenEmbedder onlineEmbedder = EmbedderFactory.getOnlineEmbedder();
        Set<Artifact> artifacts = nbMavenProject.getMavenProject().getArtifacts();
        int totalSuccessCount = 0;
        int totalFailCount = 0;
        StringBuilder errors = new StringBuilder();

        for (String classifier : classifiers) {
            int successCount = 0;
            int failCount = 0;

            for (Artifact art : artifacts) {
                if (downloadArtifact(onlineEmbedder, nbMavenProject, art, classifier, errors)) {
                    successCount++;
                } else {
                    failCount++;
                }
            }
            totalSuccessCount += successCount;
            totalFailCount += failCount;
        }

        NbMavenProject.fireMavenProjectReload(project);

        String artifactTypeNames = classifiers.stream()
                .map(c -> c.substring(0, 1).toUpperCase() + c.substring(1))
                .collect(Collectors.joining(" and "));

        return buildResultString(artifactTypeNames, "Project", projectPath, totalSuccessCount, totalFailCount, errors);
    }

    /**
     * Downloads a specific classified artifact for a single dependency.
     * @param type The type of the dependency.
     * @param version The version of the dependency.
     * @param classifier The classifier of the artifact to download.
     * @param artifactId The artifactId of the dependency.
     * @param groupId The groupId of the dependency.
     * @param projectPath The absolute path of the project to use for repository context.
     * @return true on success, false on failure.
     * @throws java.lang.Exception if an error occurs.
     */
    @AgiTool("Downloads a specific classified artifact (e.g., 'sources', 'javadoc', or the main artifact if classifier is null) for a single dependency. This can be used to verify an artifact exists before adding it to a POM. Returns true on success, false on failure.")
        public boolean downloadDependencyArtifact(
                @AgiToolParam("The absolute path of the project to use for repository context.") String projectPath,
                @AgiToolParam("The groupId of the dependency.") String groupId,
                @AgiToolParam("The artifactId of the dependency.") String artifactId,
                @AgiToolParam("The version of the dependency (e.g., 'LATEST', '1.0.0').") String version,
                @AgiToolParam(value = "The classifier of the artifact to download (e.g., 'sources', 'javadoc'). Use null for the main artifact.", required = false) String classifier,
                @AgiToolParam(value = "The type of the dependency (e.g., 'test-jar'). If null, defaults to 'jar'.", required = false) String type) throws Exception {
        Project project = NbProjects.findOpenProject(projectPath);
        NbMavenProject nbMavenProject = project.getLookup().lookup(NbMavenProject.class);
        if (nbMavenProject == null) {
            throw new IllegalStateException("Project '" + projectPath + "' is not a Maven project or could not be found.");
        }

        MavenEmbedder embedder = EmbedderFactory.getOnlineEmbedder();

        String effectiveType = (type == null || type.isBlank()) ? "jar" : type.trim();
        String effectiveClassifier = (classifier == null || classifier.isBlank() || "jar".equalsIgnoreCase(classifier.trim())) ? null : classifier.trim();

        Artifact temporaryArtifact = embedder.createArtifactWithClassifier(
                groupId,
                artifactId,
                version,
                effectiveType,
                effectiveClassifier
        );

        return downloadArtifact(embedder, nbMavenProject, temporaryArtifact, effectiveClassifier, new StringBuilder());
    }
    
    /**
     * Attempts to resolve and download a specific artifact (or classified variant like sources/javadoc) 
     * using the provided Maven embedder.
     * 
     * @param embedder The Maven embedder to use for resolution.
     * @param project The project providing the remote repository configuration.
     * @param art The base artifact to resolve.
     * @param classifier The classifier to resolve (e.g., 'sources', 'javadoc', or null for main).
     * @param errors A StringBuilder to capture any resolution error messages.
     * @return true if the artifact was successfully resolved and downloaded to the local repository.
     */
    private static boolean downloadArtifact(MavenEmbedder embedder, NbMavenProject project, Artifact art, String classifier, StringBuilder errors) {
        if (Artifact.SCOPE_SYSTEM.equals(art.getScope())) {
            return false;
        }
        LOG.log(Level.INFO, "Attempting to resolve artifact: {0}:{1}:{2}:{3}:{4}", new Object[]{art.getGroupId(), art.getArtifactId(), art.getVersion(), art.getType(), classifier});
        try {
            Artifact artifactToResolve = embedder.createArtifactWithClassifier(
                    art.getGroupId(),
                    art.getArtifactId(),
                    art.getVersion(),
                    art.getType(),
                    classifier
            );
            
            embedder.resolveArtifact(
                    artifactToResolve,
                    project.getMavenProject().getRemoteArtifactRepositories(),
                    embedder.getLocalRepository()
            );
            LOG.log(Level.INFO, "Successfully resolved artifact: {0}", artifactToResolve.getId());
            return true;
        } catch (ArtifactNotFoundException e) {
            LOG.log(Level.WARNING, "Artifact not found: {0}", e.getMessage());
            errors.append(classifier).append(" not found for ").append(art.getId()).append("\n");
        } catch (ArtifactResolutionException e) {
            LOG.log(Level.WARNING, "Artifact resolution error: {0}", e.getMessage());
            errors.append("Could not resolve ").append(classifier).append(" for ").append(art.getId()).append(": ").append(e.getMessage()).append("\n");
        } catch (Exception e) {
            LOG.log(Level.SEVERE, "Unexpected error during artifact resolution", e);
            errors.append("An unexpected error occurred for ").append(art.getId()).append(" while downloading ").append(classifier).append(": ").append(e.getMessage()).append("\n");
        }
        return false;
    }
    
    /**
     * Builds a human-readable summary string for a batch download operation.
     * 
     * @param artifactType The name of the artifact type being downloaded (e.g., 'Sources').
     * @param targetType The type of the target (e.g., 'Project', 'Artifact').
     * @param targetId The identifier of the target.
     * @param success The number of successfully downloaded artifacts.
     * @param failed The number of artifacts that failed to download.
     * @param errors A StringBuilder containing the accumulated error details.
     * @return A descriptive result string.
     */
    private static String buildResultString(String artifactType, String targetType, String targetId, int success, int failed, StringBuilder errors) {
        String result = String.format("%s download for %s '%s' complete. Success: %d, Failed: %d.", artifactType, targetType, targetId, success, failed);
        if (failed > 0) {
            result += "\nErrors:\n" + errors.toString();
        }
        return result;
    }
    //</editor-fold>
}
