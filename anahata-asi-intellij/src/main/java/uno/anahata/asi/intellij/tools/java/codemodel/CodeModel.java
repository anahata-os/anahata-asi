/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.intellij.tools.java.codemodel;

import com.intellij.openapi.application.ReadAction;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.project.ProjectManager;
import com.intellij.openapi.roots.OrderEntry;
import com.intellij.openapi.roots.ProjectFileIndex;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.JavaPsiFacade;
import com.intellij.psi.PsiClass;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiField;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiMethod;
import com.intellij.psi.PsiModifier;
import com.intellij.psi.PsiModifierList;
import com.intellij.psi.PsiPackage;
import com.intellij.psi.PsiParameter;
import com.intellij.psi.PsiParameterList;
import com.intellij.psi.javadoc.PsiDocComment;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.psi.search.PsiShortNamesCache;
import com.intellij.psi.search.searches.ClassInheritorsSearch;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.resource.handle.StringHandle;
import uno.anahata.asi.agi.tool.AgiTool;
import uno.anahata.asi.agi.tool.AgiToolException;
import uno.anahata.asi.agi.tool.AgiToolParam;
import uno.anahata.asi.agi.tool.AgiToolkit;
import uno.anahata.asi.agi.tool.AnahataToolkit;
import uno.anahata.asi.agi.tool.Page;
import uno.anahata.asi.agi.tool.ToolPermission;
import uno.anahata.asi.intellij.internal.JavaPsi;

/**
 * Universal JVM Code Model toolkit for IntelliJ IDEA.
 * <p>
 * Provides clean-room, token-efficient programmatic navigation, member inspection,
 * source extraction, and hierarchy traversal across all JVM languages supported by
 * the IntelliJ Platform (Java, Kotlin, Groovy, Scala) with deterministic location-based
 * disambiguation for conflicting FQNs.
 * </p>
 *
 * @author anahata
 */
@Slf4j
@AgiToolkit("Explores types, members, sources, javadocs, and hierarchies across all JVM languages (Java, Kotlin, Groovy, Scala) in open projects, library dependencies, and the IntelliJ Platform SDK with deterministic location disambiguation.")
public class CodeModel extends AnahataToolkit {

    /**
     * {@inheritDoc}
     * <p>Provides context-aware instructions for the CodeModel2 universal JVM toolkit.</p>
     */
    @Override
    public List<String> getSystemInstructions() throws Exception {
        List<String> instructions = new ArrayList<>(super.getSystemInstructions());
        String codeModel2Instructions = """
                ### CodeModel2 Universal JVM Toolkit Instructions:
                - **Universal JVM Language Support**: Operates natively across ALL JVM languages supported by IntelliJ (Java, Kotlin, Groovy, Scala).
                - **Scope & Classpath (`GlobalSearchScope.allScope`)**: Queries open project modules, external library dependencies (Maven/Gradle JARs such as Kryo, Spring, Jackson), and the IntelliJ SDK/JDK.
                - **Deterministic Disambiguation via `location`**: Supplying only `fqn` is optimistic and fast. If multiple types exist with the same FQN across classpaths, modules, or SDKs, the tool fails fast with a list of matching locations; pass the `location` parameter to resolve deterministically.
                - **Single Unified Tools**: `getSource` and `getJavadoc` work seamlessly for both types and individual members. `getHierarchy` navigates either SUBTYPES or SUPERTYPES in a single clean tree without redundant empty lists.
                - **Token Efficiency**: Member items and hierarchy nodes do not duplicate redundant URL headers.
                """;
        instructions.add(codeModel2Instructions);
        return instructions;
    }

    /**
     * Finds types matching a query across all JVM languages (Java, Kotlin, Groovy, Scala) in open projects,
     * library dependencies, and the IntelliJ Platform SDK/JDK using indexed caches.
     *
     * @param query          the search query: simple class name, FQN, wildcard (*, ?), or camel-hump abbreviation.
     * @param caseSensitive  whether the search should be case-sensitive. Defaults to false.
     * @param startIndex     starting index (0-based) for pagination. Defaults to 0.
     * @param pageSize       maximum number of results to return per page. Defaults to 50.
     * @return a paginated result of {@link TypeItem} records.
     * @throws AgiToolException if indexing is still in progress.
     */
    @AgiTool("Finds types across all JVM languages (Java, Kotlin, Groovy, Scala) in open projects, library dependencies (e.g. Kryo, Jackson, Spring), and the IntelliJ SDK/JDK using indexed caches. Supports simple names, FQNs, wildcards (*, ?), and camel-hump matching (e.g. 'NPE' -> NullPointerException).")
    public Page<TypeItem> findTypes(
            @AgiToolParam("Search query: simple class name (e.g. 'CodeModel'), FQN (e.g. 'java.util.List'), wildcard pattern (e.g. '*Renderer'), or camel-hump abbreviation (e.g. 'NPE'). Do not include file extensions.") String query,
            @AgiToolParam(value = "Whether the search should be case-sensitive. Defaults to false.", required = false) Boolean caseSensitive,
            @AgiToolParam(value = "Starting index (0-based) for pagination. Defaults to 0.", required = false) Integer startIndex,
            @AgiToolParam(value = "Maximum number of results to return per page. Defaults to 50.", required = false) Integer pageSize) throws AgiToolException {

        awaitSmart();
        boolean isCaseSensitive = caseSensitive != null && caseSensitive;

        List<TypeItem> allResults = ReadAction.computeBlocking(() -> {
            List<TypeItem> results = new ArrayList<>();
            boolean hasWildcard = query.contains("*") || query.contains("?");
            Pattern pattern = hasWildcard ? Pattern.compile(
                    "^" + Pattern.quote(query).replace("*", "\\E.*\\Q").replace("?", "\\E.\\Q") + "$",
                    isCaseSensitive ? 0 : Pattern.CASE_INSENSITIVE) : null;

            for (Project project : ProjectManager.getInstance().getOpenProjects()) {
                PsiShortNamesCache cache = PsiShortNamesCache.getInstance(project);

                if (!hasWildcard) {
                    PsiClass[] exactClasses = cache.getClassesByName(query, GlobalSearchScope.allScope(project));
                    for (PsiClass cl : exactClasses) {
                        addTypeItemToResults(cl, project, results);
                    }

                    if (query.contains(".")) {
                        PsiClass[] fqnMatches = JavaPsiFacade.getInstance(project).findClasses(query, GlobalSearchScope.allScope(project));
                        for (PsiClass cl : fqnMatches) {
                            addTypeItemToResults(cl, project, results);
                        }
                    }
                }

                if (results.size() < 20 || hasWildcard) {
                    String[] names = cache.getAllClassNames();
                    int count = 0;
                    for (String name : names) {
                        boolean match = hasWildcard
                                ? pattern.matcher(name).matches()
                                : (isCaseSensitive ? name.contains(query) : name.toLowerCase().contains(query.toLowerCase()));
                        if (match) {
                            PsiClass[] matches = cache.getClassesByName(name, GlobalSearchScope.allScope(project));
                            for (PsiClass cl : matches) {
                                addTypeItemToResults(cl, project, results);
                            }
                            count++;
                            if (count > 200) {
                                break;
                            }
                        }
                    }
                }
            }
            return results;
        });

        allResults.sort((t1, t2) -> t1.getFqn().compareTo(t2.getFqn()));
        int start = startIndex != null ? Math.max(0, startIndex) : 0;
        int size = pageSize != null ? Math.max(1, pageSize) : 50;
        return new Page<>(allResults, start, size);
    }

    /**
     * Gets a paginated list of all members (methods, constructors, fields, properties, inner types)
     * for a type across any JVM language.
     *
     * @param fqn              the fully-qualified name of the type.
     * @param location         optional origin location descriptor to disambiguate if multiple types share the same FQN.
     * @param nameQuery        optional query string to filter members by name (case-insensitive substring match).
     * @param kindFilters      optional list of member kinds to filter by (e.g. ['METHOD', 'FIELD']).
     * @param includeInherited whether to include inherited members from supertypes. Defaults to false.
     * @param startIndex       starting index (0-based) for pagination. Defaults to 0.
     * @param pageSize         maximum number of results to return per page. Defaults to 108.
     * @return a {@link MemberPage} with enclosing type metadata and members.
     * @throws Exception if resolution or member extraction fails.
     */
    @AgiTool("Gets a paginated list of all members (methods, constructors, fields, properties, inner types) for a type across any JVM language. Does not repeat redundant URLs on members. Supports deterministic disambiguation via 'location' if multiple types share the same FQN.")
    public MemberPage getMembers(
            @AgiToolParam("The fully-qualified name of the type (e.g. 'java.util.List' or 'uno.anahata.asi.core.Agi').") String fqn,
            @AgiToolParam(value = "Optional origin location descriptor, module name, or jar path to disambiguate if multiple types share the same FQN.", required = false) String location,
            @AgiToolParam(value = "Optional query string to filter members by name (case-insensitive substring match).", required = false) String nameQuery,
            @AgiToolParam(value = "Optional list of member kinds to filter by (e.g. ['METHOD', 'FIELD']).", required = false) List<String> kindFilters,
            @AgiToolParam(value = "Whether to include inherited members from superclasses and interfaces. Defaults to false (declared members only).", required = false) Boolean includeInherited,
            @AgiToolParam(value = "Starting index (0-based) for pagination. Defaults to 0.", required = false) Integer startIndex,
            @AgiToolParam(value = "Maximum number of results to return per page. Defaults to 108.", required = false) Integer pageSize) throws Exception {

        awaitSmart();
        PsiClass cl = resolveUniquePsiClass(fqn, location);
        boolean inherited = includeInherited != null && includeInherited;

        List<MemberItem> allMembers = ReadAction.computeBlocking(() -> {
            List<MemberItem> members = new ArrayList<>();
            String declaringClassFqn = cl.getQualifiedName();
            String lang = detectLanguage(cl);

            // Fields
            PsiField[] fields = inherited ? cl.getAllFields() : cl.getFields();
            for (PsiField field : fields) {
                boolean isInherited = !cl.equals(field.getContainingClass());
                String originClass = (field.getContainingClass() != null) ? field.getContainingClass().getQualifiedName() : declaringClassFqn;
                String memberFqn = originClass + "." + field.getName();
                String typeText = (field.getType() != null) ? field.getType().getPresentableText() : "";
                members.add(MemberItem.builder()
                        .fqn(memberFqn)
                        .name(field.getName())
                        .kind("FIELD")
                        .language(lang)
                        .signature(buildFieldSignature(field))
                        .returnType(typeText)
                        .modifiers(extractModifiers(field.getModifierList()))
                        .isInherited(isInherited)
                        .declaringClassFqn(originClass)
                        .build());
            }

            // Methods & Constructors
            PsiMethod[] methods = inherited ? cl.getAllMethods() : cl.getMethods();
            for (PsiMethod method : methods) {
                boolean isInherited = !cl.equals(method.getContainingClass());
                String originClass = (method.getContainingClass() != null) ? method.getContainingClass().getQualifiedName() : declaringClassFqn;
                String memberFqn = buildMethodCanonicalFqn(originClass, method);
                String returnTypeText = (method.getReturnType() != null) ? method.getReturnType().getPresentableText() : "void";
                String kind = method.isConstructor() ? "CONSTRUCTOR" : "METHOD";
                members.add(MemberItem.builder()
                        .fqn(memberFqn)
                        .name(method.getName())
                        .kind(kind)
                        .language(lang)
                        .signature(buildMethodSignature(method))
                        .returnType(returnTypeText)
                        .modifiers(extractModifiers(method.getModifierList()))
                        .isInherited(isInherited)
                        .declaringClassFqn(originClass)
                        .build());
            }

            // Inner Types
            PsiClass[] innerClasses = inherited ? cl.getAllInnerClasses() : cl.getInnerClasses();
            for (PsiClass inner : innerClasses) {
                boolean isInherited = !cl.equals(inner.getContainingClass());
                String originClass = (inner.getContainingClass() != null) ? inner.getContainingClass().getQualifiedName() : declaringClassFqn;
                String memberFqn = originClass + "$" + inner.getName();
                members.add(MemberItem.builder()
                        .fqn(memberFqn)
                        .name(inner.getName())
                        .kind(resolveElementKind(inner))
                        .language(detectLanguage(inner))
                        .signature(resolveElementKind(inner).toLowerCase() + " " + inner.getName())
                        .returnType(inner.getName())
                        .modifiers(extractModifiers(inner.getModifierList()))
                        .isInherited(isInherited)
                        .declaringClassFqn(originClass)
                        .build());
            }

            return members;
        });

        if (nameQuery != null && !nameQuery.isBlank()) {
            String lower = nameQuery.toLowerCase();
            allMembers = allMembers.stream()
                    .filter(m -> m.getName() != null && m.getName().toLowerCase().contains(lower))
                    .collect(Collectors.toList());
        }

        if (kindFilters != null && !kindFilters.isEmpty()) {
            List<String> upperKinds = kindFilters.stream().map(String::toUpperCase).toList();
            allMembers = allMembers.stream()
                    .filter(m -> upperKinds.contains(m.getKind().toUpperCase()))
                    .collect(Collectors.toList());
        }

        int start = startIndex != null ? Math.max(0, startIndex) : 0;
        int size = pageSize != null ? Math.max(1, pageSize) : 108;
        String lang = ReadAction.computeBlocking(() -> detectLanguage(cl));
        String loc = ReadAction.computeBlocking(() -> resolveLocation(cl, cl.getProject()));

        return new MemberPage(allMembers, start, size, fqn, lang, loc);
    }

    /**
     * Retrieves the source code declaration of a type OR a specific member across any JVM language
     * (Java, Kotlin, Groovy, Scala). Automatically navigates decompiled bytecode or attached library source files.
     *
     * @param targetFqn the fully-qualified name of the type OR canonical member FQN.
     * @param location  optional origin location to disambiguate if multiple types share the same FQN.
     * @return the raw source text.
     * @throws Exception if resolution fails.
     */
    @AgiTool("Retrieves the full source code declaration of a type OR a specific member across any JVM language (Java, Kotlin, Groovy, Scala). Automatically navigates decompiled bytecode or attached library source files.")
    public String getSource(
            @AgiToolParam("The fully-qualified name of the type (e.g. 'java.util.List') OR canonical member FQN (e.g. 'java.util.List.add(java.lang.Object)' or 'pkg.Class.fieldName').") String targetFqn,
            @AgiToolParam(value = "Optional origin location to disambiguate if multiple types share the same FQN.", required = false) String location) throws Exception {

        awaitSmart();
        return ReadAction.computeBlocking(() -> {
            boolean isMember = isMemberFqn(targetFqn);
            if (isMember) {
                String typeFqn = extractTypeFqnFromMemberFqn(targetFqn);
                PsiClass cl = resolveUniquePsiClass(typeFqn, location);
                PsiElement targetElement = findMemberElement(cl, targetFqn);
                if (targetElement != null) {
                    PsiElement nav = targetElement.getNavigationElement();
                    return (nav != null) ? nav.getText() : targetElement.getText();
                }
                throw new AgiToolException("Member not found: " + targetFqn + " in type " + typeFqn);
            } else {
                PsiClass cl = resolveUniquePsiClass(targetFqn, location);
                PsiElement nav = cl.getNavigationElement();
                return (nav != null) ? nav.getText() : cl.getText();
            }
        });
    }

    /**
     * Retrieves the Javadoc, KDoc, or Groovydoc documentation of a type OR a specific member across any JVM language.
     *
     * @param targetFqn the fully-qualified name of the type OR canonical member FQN.
     * @param location  optional origin location to disambiguate if multiple types share the same FQN.
     * @return documentation text, or an empty string if none is present.
     * @throws Exception if resolution fails.
     */
    @AgiTool("Retrieves the Javadoc, KDoc, or Groovydoc documentation of a type OR a specific member across any JVM language.")
    public String getJavadoc(
            @AgiToolParam("The fully-qualified name of the type OR canonical member FQN.") String targetFqn,
            @AgiToolParam(value = "Optional origin location to disambiguate if multiple types share the same FQN.", required = false) String location) throws Exception {

        awaitSmart();
        return ReadAction.computeBlocking(() -> {
            boolean isMember = isMemberFqn(targetFqn);
            if (isMember) {
                String typeFqn = extractTypeFqnFromMemberFqn(targetFqn);
                PsiClass cl = resolveUniquePsiClass(typeFqn, location);
                PsiElement targetElement = findMemberElement(cl, targetFqn);
                if (targetElement instanceof PsiMethod method && method.getDocComment() != null) {
                    return method.getDocComment().getText();
                } else if (targetElement instanceof PsiField field && field.getDocComment() != null) {
                    return field.getDocComment().getText();
                } else if (targetElement instanceof PsiClass inner && inner.getDocComment() != null) {
                    return inner.getDocComment().getText();
                }
                return "";
            } else {
                PsiClass cl = resolveUniquePsiClass(targetFqn, location);
                PsiDocComment doc = cl.getDocComment();
                return doc != null ? doc.getText() : "";
            }
        });
    }

    /**
     * Recursively explores the type hierarchy tree (either SUBTYPES or SUPERTYPES) for a given type across any JVM language.
     *
     * @param fqn       the fully-qualified name of the starting type.
     * @param direction the traversal direction: 'SUBTYPES' (subclasses and implementors) or 'SUPERTYPES' (base classes and interfaces).
     * @param location  optional origin location to disambiguate if multiple types share the same FQN.
     * @param maxDepth  maximum recursion depth. Defaults to 3.
     * @return a clean, recursive {@link HierarchyNode} tree.
     * @throws Exception if resolution fails.
     */
    @AgiTool("Recursively explores the type hierarchy tree (either SUBTYPES or SUPERTYPES) for a given type across any JVM language. Returns a clean, token-efficient recursive tree without empty lists or nested URL bloat.")
    public HierarchyNode getHierarchy(
            @AgiToolParam("The fully-qualified name of the starting type (e.g. 'java.lang.CharSequence').") String fqn,
            @AgiToolParam("The traversal direction: 'SUBTYPES' (finds subclasses and implementors) or 'SUPERTYPES' (finds base classes and interfaces).") HierarchyDirection direction,
            @AgiToolParam(value = "Optional origin location to disambiguate if multiple types share the same FQN.", required = false) String location,
            @AgiToolParam(value = "Maximum recursion depth. Defaults to 3.", required = false) Integer maxDepth) throws Exception {

        awaitSmart();
        PsiClass cl = resolveUniquePsiClass(fqn, location);
        int depth = (maxDepth != null && maxDepth > 0) ? maxDepth : 3;

        return ReadAction.computeBlocking(() -> buildHierarchyNode(cl, direction, depth, 0));
    }

    /**
     * Loads the source files for one or more types as managed resources into the session context.
     * Supports local project files and external library dependencies across all JVM languages.
     *
     * @param fqns     list of fully-qualified type names to load into context.
     * @param location optional origin location to disambiguate if any FQN is ambiguous.
     * @return summary message.
     * @throws Exception if loading fails.
     */
    @AgiTool(value = "Loads the source files for one or more types as managed resources into the session context. Supports local project files (with full live editing/VCS tracking) and external library dependencies (attached sources or Fernflower decompiled bytecode via StringHandle) across all JVM languages.", permission = ToolPermission.APPROVE_ALWAYS)
    public String loadSources(
            @AgiToolParam("List of fully-qualified type names to load into context.") List<String> fqns,
            @AgiToolParam(value = "Optional origin location to disambiguate if any FQN is ambiguous.", required = false) String location) throws Exception {

        awaitSmart();
        String actor = getModelId() + " via @AgiTool loadSources";
        StringBuilder sb = new StringBuilder();

        for (String fqn : fqns) {
            try {
                PsiClass cl = resolveUniquePsiClass(fqn, location);
                String outcome = ReadAction.computeBlocking(() -> {
                    PsiElement nav = cl.getNavigationElement();
                    PsiFile psiFile = (nav != null) ? nav.getContainingFile() : cl.getContainingFile();
                    if (psiFile == null) {
                        return "Containing file not found.";
                    }

                    VirtualFile vf = psiFile.getVirtualFile();
                    if (vf != null && vf.isInLocalFileSystem()) {
                        Path localPath = Path.of(vf.getPath());
                        if (Files.exists(localPath)) {
                            getAgi().getResourceManager().registerPaths(List.of(localPath), actor);
                            return "Registered local file: " + localPath.getFileName();
                        }
                    }

                    String text = psiFile.getText();
                    if (text == null || text.isBlank()) {
                        return "Source content was empty.";
                    }

                    String fileName = psiFile.getName();
                    String resName = (cl.getQualifiedName() != null) ? cl.getQualifiedName() + " (" + fileName + ")" : fileName;
                    StringHandle handle = new StringHandle(resName, text);
                    if (vf != null) {
                        handle.setContextPath(vf.getPath());
                    }
                    getAgi().getResourceManager().registerHandle(handle, actor);
                    return "Registered memory resource: " + resName;
                });

                if (!sb.isEmpty()) {
                    sb.append("\n");
                }
                sb.append(fqn).append(": ").append(outcome);
            } catch (Exception e) {
                error("Could not load source for " + fqn + ": " + e.getMessage(), e);
                if (!sb.isEmpty()) {
                    sb.append("\n");
                }
                sb.append(fqn).append(": FAILED (").append(e.getMessage()).append(")");
            }
        }

        return sb.toString();
    }

    /**
     * Finds all types within a package across any JVM language (Java, Kotlin, Groovy, Scala), with an option for recursive search.
     *
     * @param packageName the fully-qualified name of the package.
     * @param kindFilter  optional kind of type to filter by (CLASS, INTERFACE, ENUM, RECORD, ANNOTATION, OBJECT, TRAIT).
     * @param recursive   if true, search includes all subpackages. Defaults to false.
     * @param location    optional origin location to limit search to a specific module, SDK, or jar.
     * @param startIndex  starting index (0-based) for pagination. Defaults to 0.
     * @param pageSize    maximum number of results to return per page. Defaults to 50.
     * @return paginated Page of TypeItems.
     * @throws AgiToolException if indexing is in progress.
     */
    @AgiTool("Finds all types within a package across any JVM language (Java, Kotlin, Groovy, Scala), with an option for recursive search.")
    public Page<TypeItem> findTypesInPackage(
            @AgiToolParam("The fully-qualified name of the package (e.g. 'java.util' or 'uno.anahata.asi.core').") String packageName,
            @AgiToolParam(value = "Optional kind of type to filter by (CLASS, INTERFACE, ENUM, RECORD, ANNOTATION, OBJECT, TRAIT).", required = false) String kindFilter,
            @AgiToolParam(value = "If true, search includes all subpackages. Defaults to false.", required = false) Boolean recursive,
            @AgiToolParam(value = "Optional origin location to limit search to a specific module, SDK, or jar.", required = false) String location,
            @AgiToolParam(value = "Starting index (0-based) for pagination. Defaults to 0.", required = false) Integer startIndex,
            @AgiToolParam(value = "Maximum number of results to return per page. Defaults to 50.", required = false) Integer pageSize) throws AgiToolException {

        awaitSmart();
        boolean isRecursive = recursive != null && recursive;
        String filterKindUpper = (kindFilter != null && !kindFilter.isBlank()) ? kindFilter.trim().toUpperCase() : null;

        List<TypeItem> allResults = ReadAction.computeBlocking(() -> {
            List<TypeItem> results = new ArrayList<>();
            for (Project project : ProjectManager.getInstance().getOpenProjects()) {
                PsiPackage pkg = JavaPsiFacade.getInstance(project).findPackage(packageName);
                if (pkg != null) {
                    collectTypesInPackage(pkg, project, isRecursive, filterKindUpper, location, results, new HashSet<>());
                }
            }
            return results;
        });

        allResults.sort((t1, t2) -> t1.getFqn().compareTo(t2.getFqn()));
        int start = startIndex != null ? Math.max(0, startIndex) : 0;
        int size = pageSize != null ? Math.max(1, pageSize) : 50;
        return new Page<>(allResults, start, size);
    }

    /**
     * Recursively collects types within a PSI package using IntelliJ's indexed package hierarchy.
     *
     * @param pkg             the starting PSI package.
     * @param project         the active project.
     * @param recursive       whether to recurse into subpackages.
     * @param filterKindUpper optional uppercase element kind filter.
     * @param location        optional origin location filter.
     * @param results         the accumulating result list.
     * @param visitedPkgs     set of visited package FQNs to guard against circular package graphs.
     */
    private void collectTypesInPackage(PsiPackage pkg, Project project, boolean recursive, String filterKindUpper, String location, List<TypeItem> results, Set<String> visitedPkgs) {
        if (pkg == null || !visitedPkgs.add(pkg.getQualifiedName())) {
            return;
        }
        GlobalSearchScope scope = GlobalSearchScope.allScope(project);
        PsiClass[] classes = pkg.getClasses(scope);
        for (PsiClass cl : classes) {
            String elementKind = resolveElementKind(cl);
            if (filterKindUpper == null || filterKindUpper.equalsIgnoreCase(elementKind)) {
                if (location == null || location.isBlank() || resolveLocation(cl, project).toLowerCase().contains(location.toLowerCase())) {
                    addTypeItemToResults(cl, project, results);
                }
            }
        }
        if (recursive) {
            PsiPackage[] subPkgs = pkg.getSubPackages(scope);
            for (PsiPackage sub : subPkgs) {
                collectTypesInPackage(sub, project, true, filterKindUpper, location, results, visitedPkgs);
            }
        }
    }

    /**
     * Resolves a unique {@link PsiClass} matching the given FQN and optional location.
     * Throws a descriptive {@link AgiToolException} with candidate locations if multiple types exist.
     *
     * @param fqn      the fully-qualified type name.
     * @param location optional location filter to disambiguate.
     * @return the unique matching {@link PsiClass}.
     * @throws AgiToolException if the type cannot be found or is ambiguous.
     */
    private PsiClass resolveUniquePsiClass(String fqn, String location) throws AgiToolException {
        return ReadAction.computeBlocking(() -> {
            List<PsiClass> matches = new ArrayList<>();
            for (Project project : ProjectManager.getInstance().getOpenProjects()) {
                PsiClass[] found = JavaPsiFacade.getInstance(project).findClasses(fqn, GlobalSearchScope.allScope(project));
                for (PsiClass c : found) {
                    if (!matches.contains(c)) {
                        matches.add(c);
                    }
                }
            }

            if (matches.isEmpty()) {
                throw new AgiToolException("Type not found: " + fqn);
            }

            if (matches.size() == 1) {
                return matches.get(0);
            }

            // Multiple matches: attempt disambiguation by location
            if (location != null && !location.isBlank()) {
                String locLower = location.trim().toLowerCase();
                List<PsiClass> filtered = matches.stream()
                        .filter(c -> resolveLocation(c, c.getProject()).toLowerCase().contains(locLower))
                        .toList();

                if (filtered.size() == 1) {
                    return filtered.get(0);
                } else if (!filtered.isEmpty()) {
                    matches = filtered;
                }
            }

            // Still ambiguous: provide actionable disambiguation choices
            StringBuilder sb = new StringBuilder("Multiple types found for FQN '").append(fqn).append("':\n");
            for (int i = 0; i < matches.size(); i++) {
                PsiClass c = matches.get(i);
                sb.append("  ").append(i + 1).append(". ").append(resolveLocation(c, c.getProject())).append("\n");
            }
            sb.append("Please specify the 'location' parameter (e.g. module name, SDK name, or file path) to select the target class.");
            throw new AgiToolException(sb.toString().trim());
        });
    }

    /**
     * Appends a {@link TypeItem} descriptor to the results collection, de-duplicating by FQN and location.
     *
     * @param cl      the PSI class.
     * @param project the host project.
     * @param results the accumulating results list.
     */
    private void addTypeItemToResults(PsiClass cl, Project project, List<TypeItem> results) {
        String fqn = cl.getQualifiedName();
        if (fqn != null) {
            String loc = resolveLocation(cl, project);
            boolean exists = results.stream().anyMatch(t -> fqn.equals(t.getFqn()) && loc.equals(t.getLocation()));
            if (!exists) {
                results.add(TypeItem.builder()
                        .fqn(fqn)
                        .simpleName(cl.getName())
                        .kind(resolveElementKind(cl))
                        .language(detectLanguage(cl))
                        .location(loc)
                        .build());
            }
        }
    }

    /**
     * Resolves a human-readable origin location tag for a PSI class (e.g. Module name, SDK name, or JAR).
     *
     * @param cl      the target class.
     * @param project the active project.
     * @return a descriptive origin location string.
     */
    private String resolveLocation(PsiClass cl, Project project) {
        PsiFile file = cl.getContainingFile();
        if (file == null) {
            return "Unknown";
        }
        VirtualFile vf = file.getVirtualFile();
        if (vf == null) {
            return "Virtual: " + file.getName();
        }

        ProjectFileIndex index = ProjectFileIndex.getInstance(project);
        if (index.isInSourceContent(vf)) {
            Module module = index.getModuleForFile(vf);
            String moduleName = (module != null) ? module.getName() : project.getName();
            return "Module: " + moduleName + " (" + vf.getPath() + ")";
        }

        List<OrderEntry> orderEntries = index.getOrderEntriesForFile(vf);
        if (!orderEntries.isEmpty()) {
            OrderEntry entry = orderEntries.get(0);
            return entry.getPresentableName() + " (" + vf.getPath() + ")";
        }

        return vf.getPath();
    }

    /**
     * Detects the programming language for a PSI class across JVM languages.
     *
     * @param cl the PSI class.
     * @return 'JAVA', 'KOTLIN', 'GROOVY', 'SCALA', etc.
     */
    private String detectLanguage(PsiClass cl) {
        if (cl == null || cl.getLanguage() == null) {
            return "JAVA";
        }
        return cl.getLanguage().getDisplayName().toUpperCase();
    }

    /**
     * Resolves the structural element kind of a PSI class across JVM languages.
     *
     * @param cl the target class.
     * @return 'CLASS', 'INTERFACE', 'ENUM', 'RECORD', 'ANNOTATION', 'OBJECT', or 'TRAIT'.
     */
    private String resolveElementKind(PsiClass cl) {
        if (cl.isAnnotationType()) {
            return "ANNOTATION";
        }
        if (cl.isRecord()) {
            return "RECORD";
        }
        if (cl.isEnum()) {
            return "ENUM";
        }
        if (cl.isInterface()) {
            return "INTERFACE";
        }
        String lang = detectLanguage(cl);
        if ("KOTLIN".equals(lang) && cl.getName() != null && cl.getName().endsWith("Kt")) {
            return "OBJECT";
        }
        return "CLASS";
    }

    /**
     * Extracts modifier keywords from a modifier list as a list of clean strings.
     *
     * @param list the modifier list.
     * @return list of active modifiers.
     */
    private List<String> extractModifiers(PsiModifierList list) {
        if (list == null) {
            return Collections.emptyList();
        }
        List<String> mods = new ArrayList<>();
        if (list.hasModifierProperty(PsiModifier.PUBLIC)) mods.add("public");
        if (list.hasModifierProperty(PsiModifier.PROTECTED)) mods.add("protected");
        if (list.hasModifierProperty(PsiModifier.PRIVATE)) mods.add("private");
        if (list.hasModifierProperty(PsiModifier.STATIC)) mods.add("static");
        if (list.hasModifierProperty(PsiModifier.FINAL)) mods.add("final");
        if (list.hasModifierProperty(PsiModifier.ABSTRACT)) mods.add("abstract");
        if (list.hasModifierProperty(PsiModifier.SYNCHRONIZED)) mods.add("synchronized");
        if (list.hasModifierProperty(PsiModifier.TRANSIENT)) mods.add("transient");
        if (list.hasModifierProperty(PsiModifier.VOLATILE)) mods.add("volatile");
        return mods;
    }

    /**
     * Builds a human-readable declaration signature for a field (e.g. 'public static final String NAME').
     *
     * @param field the field.
     * @return the declaration signature.
     */
    private String buildFieldSignature(PsiField field) {
        StringBuilder sb = new StringBuilder();
        for (String mod : extractModifiers(field.getModifierList())) {
            sb.append(mod).append(" ");
        }
        if (field.getType() != null) {
            sb.append(field.getType().getPresentableText()).append(" ");
        }
        sb.append(field.getName());
        return sb.toString().trim();
    }

    /**
     * Builds a human-readable declaration signature for a method (e.g. 'public boolean add(E e)').
     *
     * @param method the method.
     * @return the declaration signature.
     */
    private String buildMethodSignature(PsiMethod method) {
        StringBuilder sb = new StringBuilder();
        for (String mod : extractModifiers(method.getModifierList())) {
            sb.append(mod).append(" ");
        }
        if (!method.isConstructor() && method.getReturnType() != null) {
            sb.append(method.getReturnType().getPresentableText()).append(" ");
        }
        sb.append(method.getName()).append("(");
        PsiParameterList paramList = method.getParameterList();
        for (int i = 0; i < paramList.getParametersCount(); i++) {
            PsiParameter p = paramList.getParameter(i);
            sb.append(p.getType().getPresentableText()).append(" ").append(p.getName());
            if (i < paramList.getParametersCount() - 1) {
                sb.append(", ");
            }
        }
        sb.append(")");
        return sb.toString();
    }

    /**
     * Builds the canonical method FQN matching Anahata standards (e.g. 'pkg.Type.name(paramType,...)').
     *
     * @param classFqn the declaring class FQN.
     * @param method   the method.
     * @return canonical method FQN.
     */
    private String buildMethodCanonicalFqn(String classFqn, PsiMethod method) {
        StringBuilder sb = new StringBuilder(classFqn).append(".");
        if (method.isConstructor()) {
            sb.append("<init>");
        } else {
            sb.append(method.getName());
        }
        sb.append("(");
        PsiParameterList paramList = method.getParameterList();
        for (int i = 0; i < paramList.getParametersCount(); i++) {
            sb.append(paramList.getParameter(i).getType().getCanonicalText());
            if (i < paramList.getParametersCount() - 1) {
                sb.append(",");
            }
        }
        sb.append(")");
        return sb.toString();
    }

    /**
     * Determines whether a given FQN target represents a member rather than a class.
     *
     * @param targetFqn the FQN to check.
     * @return true if it targets a method or field.
     */
    private boolean isMemberFqn(String targetFqn) {
        if (targetFqn.contains("(") && targetFqn.endsWith(")")) {
            return true;
        }
        int lastDot = targetFqn.lastIndexOf('.');
        if (lastDot <= 0) {
            return false;
        }
        String simpleName = targetFqn.substring(lastDot + 1);
        return Character.isLowerCase(simpleName.charAt(0)) && !simpleName.contains("$");
    }

    /**
     * Extracts the declaring type FQN from a canonical member FQN.
     *
     * @param memberFqn the member FQN.
     * @return enclosing type FQN.
     */
    private String extractTypeFqnFromMemberFqn(String memberFqn) {
        if (memberFqn.contains("(")) {
            String beforeParen = memberFqn.substring(0, memberFqn.indexOf('('));
            int lastDot = beforeParen.lastIndexOf('.');
            return (lastDot > 0) ? beforeParen.substring(0, lastDot) : beforeParen;
        }
        int lastDot = memberFqn.lastIndexOf('.');
        return (lastDot > 0) ? memberFqn.substring(0, lastDot) : memberFqn;
    }

    /**
     * Locates the specific PSI member element matching a member FQN inside a class.
     *
     * @param cl        the declaring class.
     * @param memberFqn the member FQN.
     * @return the matching {@link PsiElement}, or null.
     */
    private PsiElement findMemberElement(PsiClass cl, String memberFqn) {
        String declaringFqn = cl.getQualifiedName();
        for (PsiMethod m : cl.getMethods()) {
            if (memberFqn.equals(buildMethodCanonicalFqn(declaringFqn, m))) {
                return m;
            }
        }
        for (PsiField f : cl.getFields()) {
            if (memberFqn.equals(declaringFqn + "." + f.getName())) {
                return f;
            }
        }
        for (PsiClass inner : cl.getInnerClasses()) {
            if (memberFqn.equals(declaringFqn + "$" + inner.getName())) {
                return inner;
            }
        }
        return null;
    }

    /**
     * Recursively builds a {@link HierarchyNode} tree for subtypes or supertypes.
     *
     * @param cl           starting class.
     * @param direction    traversal direction.
     * @param maxDepth     maximum recursion depth.
     * @param currentDepth current depth.
     * @return recursive hierarchy node.
     */
    private HierarchyNode buildHierarchyNode(PsiClass cl, HierarchyDirection direction, int maxDepth, int currentDepth) {
        HierarchyNode node = HierarchyNode.builder()
                .fqn(cl.getQualifiedName())
                .simpleName(cl.getName())
                .kind(resolveElementKind(cl))
                .language(detectLanguage(cl))
                .location(resolveLocation(cl, cl.getProject()))
                .direction(direction)
                .build();

        if (currentDepth < maxDepth) {
            if (direction == HierarchyDirection.SUPERTYPES) {
                for (PsiClass sup : cl.getSupers()) {
                    if (sup.getQualifiedName() != null && !"java.lang.Object".equals(sup.getQualifiedName())) {
                        node.getChildren().add(buildHierarchyNode(sup, direction, maxDepth, currentDepth + 1));
                    }
                }
            } else if (direction == HierarchyDirection.SUBTYPES) {
                try {
                    Collection<PsiClass> inheritors = ClassInheritorsSearch.search(cl).findAll();
                    for (PsiClass sub : inheritors) {
                        if (sub.getQualifiedName() != null) {
                            node.getChildren().add(buildHierarchyNode(sub, direction, maxDepth, currentDepth + 1));
                        }
                    }
                } catch (Exception e) {
                    error("Error searching inheritors for class: " + cl.getQualifiedName(), e);
                }
            }
        }
        return node;
    }

    /**
     * Blocks (bounded) until all open projects finish indexing before running PSI queries.
     *
     * @throws AgiToolException if indexing does not complete within the timeout.
     */
    private void awaitSmart() throws AgiToolException {
        JavaPsi.requireSmartForOpenProjects();
    }
}
