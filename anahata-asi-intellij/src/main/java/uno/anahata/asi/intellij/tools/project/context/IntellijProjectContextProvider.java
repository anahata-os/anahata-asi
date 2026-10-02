/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.intellij.tools.project.context;

import com.intellij.ide.highlighter.JavaFileType;
import com.intellij.openapi.application.ReadAction;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleManager;
import com.intellij.openapi.project.DumbService;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.project.ProjectManager;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.roots.ModuleRootManager;
import com.intellij.openapi.roots.ProjectFileIndex;
import com.intellij.openapi.roots.ProjectRootManager;
import com.intellij.openapi.util.Conditions;
import com.intellij.openapi.vcs.FileStatus;
import com.intellij.openapi.vcs.FileStatusManager;
import com.intellij.openapi.vfs.VfsUtilCore;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.problems.WolfTheProblemSolver;
import com.intellij.psi.PsiAnonymousClass;
import com.intellij.psi.PsiClass;
import com.intellij.psi.PsiClassType;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiJavaFile;
import com.intellij.psi.PsiManager;
import com.intellij.psi.javadoc.PsiDocComment;
import com.intellij.psi.search.FileTypeIndex;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.psi.util.PsiTreeUtil;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import org.jetbrains.idea.maven.model.MavenId;
import org.jetbrains.idea.maven.project.MavenProject;
import org.jetbrains.idea.maven.project.MavenProjectsManager;
import uno.anahata.asi.agi.context.ContextProvider;
import uno.anahata.asi.agi.message.RagMessage;
import uno.anahata.asi.intellij.internal.ProjectUtils;
import uno.anahata.asi.intellij.tools.maven.IntellijMaven;
import uno.anahata.asi.intellij.tools.project.IntellijProjects;
import uno.anahata.asi.intellij.tools.vcs.IntellijVCS;
import uno.anahata.asi.toolkit.maven.DependencyScope;
import uno.anahata.asi.toolkit.project.AbstractProjectContextProvider;
import uno.anahata.asi.toolkit.project.ProjectOverview;
import uno.anahata.asi.toolkit.project.ProjectStructureScope;

/**
 * Unified, hierarchical context provider for an IntelliJ project or submodule.
 * <p>
 * Combines Overview, Alerts, and AST/Source-root Structure into a single node.
 * Automatically mirrors the project/module hierarchy (handling multi-module and nested Maven parent trees)
 * without spawning separate micro-providers for alerts or structure.
 * </p>
 *
 * @author anahata
 */
@Slf4j
public class IntellijProjectContextProvider extends AbstractProjectContextProvider {

    /** The cached IntelliJ Project instance. */
    protected transient Project project;

    /** The cached IntelliJ Module instance if this node represents a submodule. */
    protected transient Module module;

    /** The module name if representing a submodule. */
    @Getter
    protected final String moduleName;

    /**
     * Constructs a root project node.
     *
     * @param projectsToolkit The parent Projects toolkit.
     * @param project The IntelliJ project instance.
     */
    public IntellijProjectContextProvider(IntellijProjects projectsToolkit, Project project) {
        super(project.getBasePath(),
              project.getName(),
              "Unified Project Context Provider for: " + project.getName(),
              projectsToolkit,
              project.getBasePath());
        this.project = project;
        this.module = null;
        this.moduleName = null;
        this.scope = null; // null means inherit from toolkit default

        syncModules();
        syncMdResource();
    }

    /**
     * Constructs a submodule node.
     *
     * @param parentProjectNode The parent project context provider node.
     * @param project The parent IntelliJ project instance.
     * @param module The IntelliJ module instance.
     */
    public IntellijProjectContextProvider(IntellijProjectContextProvider parentProjectNode, Project project, Module module) {
        super(module.getName(),
              module.getName(),
              "Unified Module Context Provider for: " + module.getName(),
              parentProjectNode,
              resolveModulePath(project, module));
        this.project = project;
        this.module = module;
        this.moduleName = module.getName();
        this.scope = null; // null means inherit from parent project or toolkit default

        syncMdResource();
    }

    /**
     * Resolves the IntelliJ Project instance, restoring it from the path if needed.
     *
     * @return The Project instance, or null if no longer open.
     */
    public Project getProject() {
        if (project != null && !project.isDisposed()) {
            return project;
        }
        if (projectPath != null) {
            VirtualFile vf = ProjectUtils.findVirtualFile(projectPath);
            if (vf != null) {
                Project p = ProjectUtils.findHostProject(vf);
                if (p != null && !p.isDisposed()) {
                    project = p;
                    return project;
                }
            }
            Path targetPath = Path.of(projectPath).toAbsolutePath();
            for (Project p : ProjectManager.getInstance().getOpenProjects()) {
                if (p != null && !p.isDisposed()) {
                    String basePath = p.getBasePath();
                    if (basePath != null) {
                        Path pPath = Path.of(basePath).toAbsolutePath();
                        if (targetPath.startsWith(pPath)) {
                            project = p;
                            return project;
                        }
                    }
                }
            }
        }
        Project[] open = ProjectManager.getInstance().getOpenProjects();
        if (open.length > 0 && !open[0].isDisposed()) {
            project = open[0];
            return project;
        }
        return null;
    }

    /**
     * Resolves the active IntelliJ Module instance, restoring it from name if needed.
     *
     * @return The active Module, or null if unconfigured or unloaded.
     */
    public Module getModule() {
        if (module != null && !module.isDisposed()) {
            return module;
        }
        if (moduleName != null) {
            Project p = getProject();
            if (p != null && !p.isDisposed()) {
                module = ModuleManager.getInstance(p).findModuleByName(moduleName);
            }
        }
        return module;
    }

    /**
     * Synchronizes child module nodes with the active modules in the project.
     */
    public synchronized void syncModules() {
        Project p = getProject();
        if (p == null || module != null) {
            return; // Only root project node discovers modules
        }

        Module[] modules = ModuleManager.getInstance(p).getModules();
        if (modules.length <= 1) {
            return;
        }

        List<String> currentModuleNames = new ArrayList<>();
        for (Module m : modules) {
            String modPath = resolveModulePath(p, m);
            if (modPath.equals(projectPath) || m.getName().equals(p.getName())) {
                continue;
            }
            currentModuleNames.add(m.getName());
            boolean exists = children.stream()
                    .anyMatch(c -> c instanceof IntellijProjectContextProvider gcp && m.getName().equals(gcp.getModuleName()));
            if (!exists) {
                IntellijProjectContextProvider child = new IntellijProjectContextProvider(this, p, m);
                children.add(child);
                log.info("Registered GrandProjectContextProvider for module: {}", m.getName());
            }
        }

        children.removeIf(c -> {
            if (c instanceof IntellijProjectContextProvider gcp) {
                if (gcp.getModuleName() != null && !currentModuleNames.contains(gcp.getModuleName())) {
                    log.info("Removing GrandProjectContextProvider for unloaded module: {}", gcp.getModuleName());
                    gcp.setProviding(false);
                    return true;
                }
            }
            return false;
        });
    }

    @Override
    public List<ContextProvider> getChildren() {
        if (module == null) {
            syncModules();
        }
        return super.getChildren();
    }

    @Override
    public String getName() {
        if (moduleName != null) {
            return moduleName;
        }
        Project p = getProject();
        return p != null ? p.getName() : super.getName();
    }

    /**
     * {@inheritDoc}
     * <p>
     * Returns the structured overview for this project or module.
     * </p>
     */
    @Override
    public ProjectOverview getOverview() {
        return buildOverview(getModule());
    }

    /**
     * {@inheritDoc}
     * <p>
     * Unified RAG message generator: emits the Project/Module Overview,
     * compiler alerts (if enabled and applicable), and AST type hierarchy structure.
     * </p>
     */
    @Override
    public void populateMessage(RagMessage ragMessage) throws Exception {
        Project p = getProject();
        if (p == null) {
            ragMessage.addTextPart("Couldn't fetch IntelliJ Project, project may have been closed");
            return;
        }

        Module m = getModule();
        ProjectStructureScope effectiveScope = getEffectiveScope();

        // 1. Overview Section
        ProjectOverview overview = getOverview();
        StringBuilder sb = new StringBuilder();
        sb.append(overview.toMarkdown()).append("\n");

        // 2. Alerts Section
        appendAlerts(sb, p, m, effectiveScope);

        // 3. Structure Section
        if (this.scope != null) {
            sb.append("\n> [!NOTE]\n> Project Structure Scope Override: `").append(this.scope).append("`\n\n");
        }
        appendStructure(sb, p, m, effectiveScope);

        ragMessage.addTextPart(sb.toString().trim());
    }

    private void appendAlerts(StringBuilder sb, Project p, Module m, ProjectStructureScope effectiveScope) {
        if (!effectiveScope.isShowAlerts()) {
            return;
        }

        if (DumbService.isDumb(p)) {
            log.info("Waiting for IntelliJ indexer/scanner to complete before querying alerts for: {}", p.getName());
            DumbService.getInstance(p).waitForSmartMode(60_000);
        }

        if (DumbService.isDumb(p)) {
            sb.append("\n  ## ").append(m != null ? "Module" : "Project").append(" Alerts: ")
              .append(m != null ? m.getName() : p.getName()).append(" (indexing in progress — alerts unavailable)\n");
            return;
        }

        ReadAction.runBlocking(() -> {
            WolfTheProblemSolver solver = WolfTheProblemSolver.getInstance(p);
            if (solver != null && solver.hasProblemFilesBeneath(Conditions.alwaysTrue())) {
                boolean hasSubmodules = ModuleManager.getInstance(p).getModules().length > 1;
                GlobalSearchScope searchScope;
                if (m != null) {
                    searchScope = GlobalSearchScope.moduleScope(m);
                } else if (hasSubmodules) {
                    GlobalSearchScope scope = GlobalSearchScope.projectScope(p);
                    for (Module mod : ModuleManager.getInstance(p).getModules()) {
                        scope = scope.intersectWith(GlobalSearchScope.notScope(GlobalSearchScope.moduleScope(mod)));
                    }
                    searchScope = scope;
                } else {
                    searchScope = GlobalSearchScope.projectScope(p);
                }
                List<VirtualFile> problemFiles = new ArrayList<>();
                for (VirtualFile file : FileTypeIndex.getFiles(JavaFileType.INSTANCE, searchScope)) {
                    if (solver.isProblemFile(file)) {
                        problemFiles.add(file);
                    }
                }
                if (!problemFiles.isEmpty()) {
                    sb.append("\n  ### ").append(m != null ? "Module" : "Project").append(" Alerts (Errors Found)\n");
                    for (VirtualFile file : problemFiles) {
                        sb.append("    - [ERROR] `").append(file.getPath()).append("` has compilation or unresolved-reference problems.\n");
                    }
                }
            }
        });
    }

    private void appendStructure(StringBuilder sb, Project p, Module m, ProjectStructureScope effectiveScope) {
        String structureMd = ReadAction.computeBlocking(() -> {
            StringBuilder s = new StringBuilder();
            s.append("\n  ## ").append(m != null ? "Module Structure: " + m.getName() : "Project Structure: " + p.getName()).append("\n\n");

            VirtualFile rootDir;
            VirtualFile[] sourceRoots;
            ProjectFileIndex fileIndex = ProjectRootManager.getInstance(p).getFileIndex();
            List<String> scanWarnings = new ArrayList<>();

            if (m != null) {
                ModuleRootManager rm = ModuleRootManager.getInstance(m);
                sourceRoots = rm.getSourceRoots();
                VirtualFile[] cr = rm.getContentRoots();
                rootDir = (cr.length > 0) ? cr[0] : null;
            } else {
                ProjectRootManager rm = ProjectRootManager.getInstance(p);
                sourceRoots = rm.getContentSourceRoots();
                VirtualFile[] cr = rm.getContentRoots();
                rootDir = (cr.length > 0) ? cr[0] : null;
            }

            if (effectiveScope.isShowRootFiles() && rootDir != null && rootDir.exists()) {
                appendRootFiles(rootDir, p, s, "  ", effectiveScope, scanWarnings);
            }

            boolean hasSubmodules = ModuleManager.getInstance(p).getModules().length > 1;
            List<VirtualFile> submoduleRoots = new ArrayList<>();
            if (m == null && hasSubmodules) {
                for (Module mod : ModuleManager.getInstance(p).getModules()) {
                    String modPath = resolveModulePath(p, mod);
                    if (!modPath.equals(projectPath) && !mod.getName().equals(p.getName())) {
                        for (VirtualFile crRoot : ModuleRootManager.getInstance(mod).getContentRoots()) {
                            submoduleRoots.add(crRoot);
                        }
                    }
                }
            }

            List<VirtualFile> effectiveSourceRoots = new ArrayList<>();
            if (sourceRoots != null) {
                for (VirtualFile root : sourceRoots) {
                    if (m == null && hasSubmodules) {
                        boolean belongsToSubmodule = false;
                        for (VirtualFile subRoot : submoduleRoots) {
                            if (VfsUtilCore.isAncestor(subRoot, root, false)) {
                                belongsToSubmodule = true;
                                break;
                            }
                        }
                        if (belongsToSubmodule) {
                            continue;
                        }
                    }
                    effectiveSourceRoots.add(root);
                }
            }

            if (effectiveSourceRoots.isEmpty()) {
                if (m == null && hasSubmodules) {
                    s.append("  - Source roots are organized under child modules.\n");
                } else {
                    s.append("  - No configured source roots.\n");
                }
                return s.toString();
            }

            for (VirtualFile root : effectiveSourceRoots) {
                String path = root.getPath();
                if (path.contains("generated-sources") || path.contains("generated-test-sources")) {
                    continue;
                }
                boolean isResource = path.contains("resources") || path.endsWith(".github");
                if (isResource && !effectiveScope.isShowResources()) {
                    continue;
                }
                boolean test = fileIndex.isInTestSourceContent(root);
                s.append("  ### Source Root (").append(test ? "test" : "main")
                  .append(isResource ? " resources" : "")
                  .append("): `").append(path).append("`\n");
                appendTree(root, p, s, "    ", 0, effectiveScope, scanWarnings);
            }

            if (!scanWarnings.isEmpty()) {
                s.append("\n  > [!WARNING]\n");
                s.append("  > Structure Scan Notice (").append(scanWarnings.size()).append(" warnings encountered):\n");
                for (String w : scanWarnings) {
                    s.append("  > - ").append(w).append("\n");
                }
                s.append("  > Check IDE logs for full stack traces and details.\n");
            }

            return s.toString();
        });

        sb.append(structureMd);
    }

    private void appendRootFiles(VirtualFile rootDir, Project p, StringBuilder sb, String indent, ProjectStructureScope effectiveScope, List<String> scanWarnings) {
        VirtualFile[] children = rootDir.getChildren();
        List<VirtualFile> files = new ArrayList<>();
        List<String> folders = new ArrayList<>();
        VirtualFile githubDir = null;
        VirtualFile ideaDir = null;
        for (VirtualFile child : children) {
            String cname = child.getName();
            if (".github".equals(cname) && child.isDirectory()) {
                githubDir = child;
                continue;
            }
            if (".idea".equals(cname) && child.isDirectory()) {
                ideaDir = child;
                continue;
            }
            if (cname.startsWith(".")) continue;
            if ("target".equals(cname) || "out".equals(cname) || "build".equals(cname)) continue;
            if (child.isDirectory()) {
                folders.add(cname);
            } else {
                files.add(child);
            }
        }
        if (!files.isEmpty() || !folders.isEmpty() || githubDir != null || ideaDir != null) {
            sb.append(indent).append("### Root Directory\n");
            if (!folders.isEmpty()) {
                Collections.sort(folders);
                sb.append(indent).append("  - Folders: `").append(String.join("`, `", folders)).append("`\n");
            }
            FileStatusManager fsm = FileStatusManager.getInstance(p);
            files.sort(Comparator.comparing(VirtualFile::getName, String.CASE_INSENSITIVE_ORDER));
            for (VirtualFile file : files) {
                sb.append(indent).append("  - 📄 `").append(file.getName()).append("`");
                if (effectiveScope.isShowVcsStatus()) {
                    FileStatus st = fsm.getStatus(file);
                    if (st != FileStatus.NOT_CHANGED && st != null) {
                        sb.append(" [").append(st.getText()).append("]");
                    }
                }
                if (effectiveScope.isShowFileSizes()) {
                    sb.append(String.format(" [%.1f KB]", file.getLength() / 1024.0));
                }
                sb.append("\n");
            }
            if (githubDir != null && effectiveScope.isShowResources()) {
                sb.append(indent).append("  - 📦 `.github`\n");
                appendTree(githubDir, p, sb, indent + "    ", 0, effectiveScope, scanWarnings);
            }
            if (ideaDir != null && effectiveScope.isShowResources()) {
                sb.append(indent).append("  - 📦 `.idea`\n");
                appendTree(ideaDir, p, sb, indent + "    ", 0, effectiveScope, scanWarnings);
            }
        }
    }

    private void appendTree(VirtualFile dir, Project p, StringBuilder sb, String indent, int depth, ProjectStructureScope effectiveScope, List<String> scanWarnings) {
        if (depth > 12) {
            return;
        }
        VirtualFile[] children = dir.getChildren();
        List<VirtualFile> sorted = new ArrayList<>(List.of(children));
        sorted.sort((f1, f2) -> {
            boolean d1 = f1.isDirectory();
            boolean d2 = f2.isDirectory();
            if (d1 != d2) return d1 ? -1 : 1;
            return f1.getName().compareToIgnoreCase(f2.getName());
        });
        for (VirtualFile child : sorted) {
            String name = child.getName();
            if (name.startsWith(".") || name.equals("target") || name.equals("out") || name.equals("build") || name.equals("caches")) {
                continue;
            }
            if (child.isDirectory()) {
                sb.append(indent).append("- 📦 `").append(name).append("`\n");
                appendTree(child, p, sb, indent + "  ", depth + 1, effectiveScope, scanWarnings);
            } else if ("java".equals(child.getExtension())) {
                sb.append(indent).append("- 🄹 `").append(name).append("`");
                PsiFile psi = PsiManager.getInstance(p).findFile(child);
                if (psi instanceof PsiJavaFile javaFile) {
                    for (PsiClass cls : javaFile.getClasses()) {
                        if (effectiveScope.isShowElementKind()) {
                            String k = cls.isInterface() ? (cls.isAnnotationType() ? "ANNOTATION_TYPE" : "INTERFACE")
                                     : cls.isEnum() ? "ENUM"
                                     : cls.isRecord() ? "RECORD" : "CLASS";
                            sb.append(" (").append(k).append(")");
                        }
                        if (effectiveScope.isShowSupertypes()) {
                            try {
                                StringBuilder stSb = new StringBuilder();
                                PsiClassType[] extendsList = cls.getExtendsListTypes();
                                if (extendsList.length > 0) {
                                    stSb.append("extends ").append(Arrays.stream(extendsList)
                                            .map(PsiClassType::getClassName)
                                            .collect(Collectors.joining(", ")));
                                }
                                PsiClassType[] implementsList = cls.getImplementsListTypes();
                                if (implementsList.length > 0) {
                                    if (stSb.length() > 0) {
                                        stSb.append(" ");
                                    }
                                    stSb.append("implements ").append(Arrays.stream(implementsList)
                                            .map(PsiClassType::getClassName)
                                            .collect(Collectors.joining(", ")));
                                }
                                if (stSb.length() > 0) {
                                    sb.append(" ").append(stSb);
                                }
                            } catch (Throwable t) {
                                log.warn("Failed to resolve supertypes for {}: {}", cls.getName(), t.getMessage(), t);
                                sb.append(" ⚠️ [Supertypes unavailable]");
                                scanWarnings.add("`" + name + "`: Failed to resolve supertypes: " + t.getMessage());
                            }
                        }
                        if (effectiveScope.isShowVcsStatus()) {
                            FileStatus st = FileStatusManager.getInstance(p).getStatus(child);
                            if (st != FileStatus.NOT_CHANGED && st != null) {
                                sb.append(" [").append(st.getText()).append("]");
                            }
                        }
                        if (effectiveScope.isShowFileSizes()) {
                            sb.append(String.format(" [%.1f KB]", child.getLength() / 1024.0));
                        }
                        if (effectiveScope.isShowJavadoc()) {
                            try {
                                PsiDocComment docComment = cls.getDocComment();
                                if (docComment != null) {
                                    String summary = AbstractProjectContextProvider.extractFirstSentence(docComment.getText());
                                    if (summary != null && !summary.isBlank()) {
                                        sb.append(" - ").append(summary);
                                    }
                                }
                            } catch (Throwable t) {
                                log.warn("Failed to resolve Javadoc for {}: {}", cls.getName(), t.getMessage(), t);
                                sb.append(" ⚠️ [Javadoc unavailable]");
                                scanWarnings.add("`" + name + "`: Failed to resolve Javadoc: " + t.getMessage());
                            }
                        }
                        if (effectiveScope.isShowInnerClasses()) {
                            appendInnerClasses(cls, sb, indent + "  ", effectiveScope);
                        }
                    }
                } else {
                    if (effectiveScope.isShowVcsStatus()) {
                        FileStatus st = FileStatusManager.getInstance(p).getStatus(child);
                        if (st != FileStatus.NOT_CHANGED && st != null) {
                            sb.append(" [").append(st.getText()).append("]");
                        }
                    }
                    if (effectiveScope.isShowFileSizes()) {
                        sb.append(String.format(" [%.1f KB]", child.getLength() / 1024.0));
                    }
                }
                sb.append("\n");
            } else {
                sb.append(indent).append("- 📄 `").append(child.getName()).append("`");
                if (effectiveScope.isShowFileSizes()) {
                    sb.append(String.format(" [%.1f KB]", child.getLength() / 1024.0));
                }
                sb.append("\n");
            }
        }
    }

    /**
     * Recursively appends named inner classes and sequentially numbered anonymous classes.
     */
    private void appendInnerClasses(PsiClass cls, StringBuilder sb, String indent, ProjectStructureScope effectiveScope) {
        for (PsiClass inner : cls.getInnerClasses()) {
            if (inner instanceof PsiAnonymousClass) {
                continue;
            }
            String iname = inner.getName();
            if (iname == null || iname.isBlank()) {
                continue;
            }
            sb.append("\n").append(indent).append("- `").append(iname).append("`");
            if (effectiveScope.isShowElementKind()) {
                String ik = inner.isInterface() ? (inner.isAnnotationType() ? "ANNOTATION_TYPE" : "INTERFACE")
                         : inner.isEnum() ? "ENUM"
                         : inner.isRecord() ? "RECORD" : "CLASS";
                sb.append(" (").append(ik).append(")");
            }
            appendInnerClasses(inner, sb, indent + "  ", effectiveScope);
        }

        PsiAnonymousClass[] anonClasses = PsiTreeUtil.getChildrenOfType(cls, PsiAnonymousClass.class);
        if (anonClasses != null && anonClasses.length > 0) {
            int anonIndex = 1;
            for (PsiAnonymousClass ignored : anonClasses) {
                sb.append("\n").append(indent).append("- `").append(anonIndex++).append("` (CLASS)");
            }
        }
    }

    /**
     * Resolves the filesystem root path for a module.
     *
     * @param project The parent IntelliJ project.
     * @param module The target module.
     * @return The absolute path to the module directory.
     */
    public static String resolveModulePath(Project project, Module module) {
        VirtualFile[] cr = ModuleRootManager.getInstance(module).getContentRoots();
        if (cr.length > 0 && cr[0] != null) {
            return cr[0].getPath();
        }
        return project.getBasePath();
    }

    /**
     * Builds a structured {@link ProjectOverview} model for this project or module.
     *
     * @param targetModule Optional module instance, or null for root project.
     * @return The populated {@link ProjectOverview}.
     */
    protected ProjectOverview buildOverview(Module targetModule) {
        Project p = getProject();
        String targetName = targetModule != null ? targetModule.getName() : (p != null ? p.getName() : getName());
        String packaging = targetModule != null ? "jar" : "pom";
        String mavenGroupId = null;
        String mavenArtifactId = null;
        String mavenVersion = null;

        if (p != null) {
            MavenProjectsManager mavenMgr = MavenProjectsManager.getInstance(p);
            MavenProject mp = null;
            if (targetModule != null) {
                mp = mavenMgr.findProject(targetModule);
            }
            if (mp == null) {
                VirtualFile pomVf = ProjectUtils.findVirtualFile(Path.of(projectPath).resolve("pom.xml").toString());
                if (pomVf != null) {
                    mp = mavenMgr.findProject(pomVf);
                }
            }
            if (mp != null) {
                packaging = mp.getPackaging();
                MavenId mid = mp.getMavenId();
                if (mid != null) {
                    mavenGroupId = mid.getGroupId();
                    mavenArtifactId = mid.getArtifactId();
                    mavenVersion = mid.getVersion();
                }
            }
        }

        String sdkInfo = null;
        if (targetModule != null) {
            Sdk sdk = ModuleRootManager.getInstance(targetModule).getSdk();
            if (sdk != null) {
                sdkInfo = sdk.getName() + " (" + (sdk.getVersionString() != null ? sdk.getVersionString() : "unknown") + ")";
            }
        } else if (p != null) {
            Sdk sdk = ProjectRootManager.getInstance(p).getProjectSdk();
            if (sdk != null) {
                sdkInfo = sdk.getName() + " (" + (sdk.getVersionString() != null ? sdk.getVersionString() : "unknown") + ")";
            }
        }

        List<DependencyScope> declaredDeps = null;
        try {
            declaredDeps = IntellijMaven.getDeclaredDependencies(projectPath);
        } catch (Exception e) {
            log.debug("No declared dependencies resolved for: {}", projectPath);
        }

        String vcsOverview = null;
        if (projectsToolkit.getAgi() != null) {
            Optional<IntellijVCS> vcsOpt = projectsToolkit.getAgi().getToolkit(IntellijVCS.class);
            if (vcsOpt.isPresent() && vcsOpt.get().isRepoRoot(projectPath)) {
                try {
                    vcsOverview = vcsOpt.get().getRepositoryOverview(projectPath);
                } catch (Exception e) {
                    log.debug("VCS overview not applicable for: {}", projectPath);
                }
            }
        }

        return ProjectOverview.builder()
                .id(targetName)
                .displayName(targetName)
                .projectDirectory(projectPath)
                .packaging(packaging)
                .mavenGroupId(mavenGroupId)
                .mavenArtifactId(mavenArtifactId)
                .mavenVersion(mavenVersion)
                .javaSourceLevel(sdkInfo)
                .mavenDeclaredDependencies(declaredDeps)
                .vcsOverview(vcsOverview)
                .build();
    }
}
