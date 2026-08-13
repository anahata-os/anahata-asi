/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.intellij.tools.java;

import com.intellij.openapi.application.ReadAction;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.project.ProjectManager;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.*;
import com.intellij.psi.javadoc.PsiDocComment;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.psi.search.PsiShortNamesCache;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.tool.Page;
import uno.anahata.asi.agi.tool.AnahataToolkit;
import uno.anahata.asi.agi.tool.AgiToolkit;
import uno.anahata.asi.agi.tool.AgiToolParam;
import uno.anahata.asi.agi.tool.AgiTool;
import uno.anahata.asi.agi.tool.AgiToolException;
import uno.anahata.asi.agi.tool.ToolPermission;
import uno.anahata.asi.intellij.internal.JavaPsi;

import javax.lang.model.element.ElementKind;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;
import java.util.stream.Collectors;

/**
 * Provides tools for interacting with the Java code model in IntelliJ IDEA.
 * This includes finding types, listing members, retrieving source code fragments, 
 * loading Javadocs, and navigating type hierarchies.
 * 
 * @author anahata
 */
@Slf4j
@AgiToolkit("A toolkit for browsing types, members, sources and javadocs.")
public class CodeModel extends AnahataToolkit {

    /**
     * Default constructor for the IntelliJ CodeModel toolkit.
     */
    public CodeModel() {
        super();
    }

    /**
     * {@inheritDoc}
     * <p>
     * Provides context-aware system instructions for the CodeModel toolkit, detailing
     * the usage of FQN-based methods versus discovery-based searches.
     * </p>
     */
    @Override
    public List<String> getSystemInstructions() throws Exception {
        String instructions = "CodeModel Toolkit Instructions:\n" 
                + "- **One Shot Methods (`loadXxxxByFqn` or `getXxxxByFqn`)**: If you already know or can work out the FQN of a type or member, use these methods to skip discovery.\n" 
                + "- **Disambiguation**: If a `xxxxByFqn` method fails, use `findTypes` or `getMembers` to get the explicit high-precision FQN.\n"
                + "- **Hierarchy**: Use `getSubtypes` and `getSupertypes` to explore inheritance.\n";
        return Collections.singletonList(instructions);
    }

    /**
     * Finds any Java types matching a query within the aggregated classpath of all open projects.
     * 
     * @param query The search query.
     * @param caseSensitive Whether the search should be case-sensitive.
     * @param preferOpenProjects Prioritize open projects.
     * @param startIndex Start index for pagination.
     * @param pageSize Max results per page.
     * @return Paginated result of JavaType.
     */
    @AgiTool("Finds any Java types matching a query within the aggregated classpath of all open projects (exactly like NetBeans `Ctrl+O`) and returns a paginated result of minimalist, machine-readable keys.")
    public Page<JavaType> findTypes(
            @AgiToolParam("The search query for the types (e.g., simple name, FQN, wildcards). Never include the file extension.") String query,
            @AgiToolParam("Whether the search should be case-sensitive.") boolean caseSensitive,
            @AgiToolParam("Whether to prioritize results from open projects.") boolean preferOpenProjects,
            @AgiToolParam(value = "The starting index (0-based) for pagination.", required = false) Integer startIndex,
            @AgiToolParam(value = "The maximum number of results to return per page.", required = false) Integer pageSize) throws AgiToolException {

        awaitSmart();
        List<JavaType> allResults = ReadAction.compute(() -> {
            List<JavaType> results = new ArrayList<>();
            for (Project project : ProjectManager.getInstance().getOpenProjects()) {
                PsiShortNamesCache cache = PsiShortNamesCache.getInstance(project);

                // Try exact class name first
                PsiClass[] exactClasses = cache.getClassesByName(query, GlobalSearchScope.allScope(project));
                for (PsiClass cl : exactClasses) {
                    addClassToResults(cl, results);
                }

                // If query is an FQN, use JavaPsiFacade
                if (query.contains(".")) {
                    PsiClass cl = JavaPsiFacade.getInstance(project).findClass(query, GlobalSearchScope.allScope(project));
                    if (cl != null) {
                        addClassToResults(cl, results);
                    }
                }

                // Fallback: Prefix/partial scanning with safety bounds to avoid token bloat
                if (results.size() < 20) {
                    String[] names = cache.getAllClassNames();
                    int count = 0;
                    for (String name : names) {
                        boolean match = caseSensitive ? name.contains(query) : name.toLowerCase().contains(query.toLowerCase());
                        if (match) {
                            PsiClass[] matches = cache.getClassesByName(name, GlobalSearchScope.allScope(project));
                            for (PsiClass cl : matches) {
                                addClassToResults(cl, results);
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
        int start = startIndex != null ? startIndex : 0;
        int size = pageSize != null ? pageSize : 100;
        return new Page<>(allResults, start, size);
    }

    /**
     * Appends a class to the result list as a {@link JavaType} keychain DTO, de-duplicating by
     * fully-qualified name. Classes without an FQN (anonymous/local) are skipped.
     *
     * @param cl      the PSI class to add.
     * @param results the accumulating result list.
     */
    private void addClassToResults(PsiClass cl, List<JavaType> results) {
        String fqn = cl.getQualifiedName();
        if (fqn != null) {
            URL url = getUrlOfClass(cl);
            JavaType type = new JavaType(fqn, url);
            if (!results.contains(type)) {
                results.add(type);
            }
        }
    }

    /**
     * Resolves the source/class-file {@code file:} URL backing a PSI class, used as the keychain
     * for later source/javadoc lookups.
     *
     * @param cl the PSI class.
     * @return the file URL, or {@code null} if it cannot be resolved.
     */
    private URL getUrlOfClass(PsiClass cl) {
        try {
            PsiFile file = cl.getContainingFile();
            if (file != null) {
                VirtualFile vFile = file.getVirtualFile();
                if (vFile != null) {
                    return Path.of(vFile.getPath()).toUri().toURL();
                }
            }
        } catch (Exception e) {
            // ignore
        }
        return null;
    }

    /**
     * Loads the source file for a given JavaType as a managed text resource.
     * 
     * @param javaType The target JavaType.
     * @return Confirmation message.
     * @throws Exception on execution failure.
     */
    @AgiTool("Loads the source file for a given `JavaType` (as returned by `Codemodel.findTypes`) as a managed text resource. Works only for outer types (whole java files that can be loaded into context), for inner classess use `getMemberSources` or `getMemberSourcesByFqn`")
    public String loadTypeSources(
            @AgiToolParam("The minimalist keychain DTO from a findTypes call.") JavaType javaType) throws Exception {
        if (javaType.getUrl() != null) {
            Path path = Path.of(javaType.getUrl().toURI());
            if (Files.exists(path)) {
                String actor = getModelId() + " via @AgiTool getTypeSources";
                getAgi().getResourceManager().registerPaths(List.of(path), actor);
                return "Source file '" + path.getFileName() + "' registered as a managed resource.";
            }
        }
        return "Source code not available for this type.";
    }

    /**
     * Loads the source file for of a java type as a managed resource by its fully qualified name.
     * 
     * @param fqn The target FQN.
     * @return Confirmation message.
     * @throws Exception on execution failure.
     */
    @AgiTool(value = "Loads the source file for of a java type as a managed resource by its fully qualified name (fqn). Fails if the FQN is ambiguous.", permission = ToolPermission.APPROVE_ALWAYS)
    public String loadTypeSourcesByFqn(
            @AgiToolParam("The fully qualified name of the type.") String fqn) throws Exception {
        return loadTypeSources(resolveUniqueType(fqn));
    }

    /**
     * Gets the Javadoc for a given JavaType.
     * 
     * @param javaType The target JavaType.
     * @return The Javadoc content.
     * @throws Exception on execution failure.
     */
    @AgiTool("Gets the Javadoc for a given JavaType.")
    public String getTypeJavadocs(
            @AgiToolParam("The keychain DTO for the type to inspect.") JavaType javaType) throws Exception {
        awaitSmart();
        return ReadAction.compute(() -> {
            PsiClass cl = findPsiClass(javaType.getFqn());
            if (cl != null) {
                PsiDocComment doc = cl.getDocComment();
                return doc != null ? doc.getText() : "";
            }
            return "";
        });
    }

    /**
     * Gets the Javadoc for a type specified by its fully qualified name.
     * 
     * @param fqn The target FQN.
     * @return The Javadoc content.
     * @throws Exception on execution failure.
     */
    @AgiTool(value = "Gets the Javadoc for a type specified by its fully qualified name. Fails if the FQN is ambiguous.", permission = ToolPermission.APPROVE_ALWAYS)
    public String getTypeJavadocsByFqn(
            @AgiToolParam("The fully qualified name of the type.") String fqn) throws Exception {
        return getTypeJavadocs(resolveUniqueType(fqn));
    }

    /**
     * Gets the source code for a specific JavaMember.
     * 
     * @param member The target member.
     * @return Member source text.
     * @throws Exception on execution failure.
     */
    @AgiTool("Gets the source code for a specific JavaMember.")
    public String getMemberSources(
            @AgiToolParam("The keychain DTO for the member to inspect.") JavaMember member) throws Exception {
        awaitSmart();
        return ReadAction.compute(() -> {
            PsiClass cl = findPsiClass(member.getFqn().substring(0, member.getFqn().lastIndexOf('.')));
            if (cl != null) {
                return getPsiMemberSource(cl, member.getFqn());
            }
            throw new AgiToolException("Class not found for member: " + member.getFqn());
        });
    }

    /**
     * Gets the source code for a member specified by its fully qualified name.
     * 
     * @param memberFqn The target member FQN.
     * @return Member source text.
     * @throws Exception on execution failure.
     */
    @AgiTool(value = "Gets the source code for a member specified by its fully qualified name. Fails if the FQN is ambiguous.", permission = ToolPermission.APPROVE_ALWAYS)
    public String getMemberSourcesByFqn(
            @AgiToolParam("The fully qualified name of the member.") String memberFqn) throws Exception {
        return getMemberSources(resolveUniqueMember(memberFqn));
    }

    /**
     * Gets the Javadoc for a specific JavaMember.
     * 
     * @param member The target member.
     * @return Member Javadoc comment text.
     * @throws Exception on execution failure.
     */
    @AgiTool("Gets the Javadoc for a specific JavaMember.")
    public String getMemberJavadocs(
            @AgiToolParam("The keychain DTO for the member to inspect.") JavaMember member) throws Exception {
        awaitSmart();
        return ReadAction.compute(() -> {
            PsiClass cl = findPsiClass(member.getFqn().substring(0, member.getFqn().lastIndexOf('.')));
            if (cl != null) {
                return getPsiMemberJavadoc(cl, member.getFqn());
            }
            return "";
        });
    }

    /**
     * Gets the Javadoc for a member specified by its fully qualified name.
     * 
     * @param memberFqn The target member FQN.
     * @return Javadoc comment text.
     * @throws Exception on execution failure.
     */
    @AgiTool(value = "Gets the Javadoc for a member specified by its fully qualified name. Fails if the FQN is ambiguous.", permission = ToolPermission.APPROVE_ALWAYS)
    public String getMemberJavadocsByFqn(
            @AgiToolParam("The fully qualified name of the member.") String memberFqn) throws Exception {
        return getMemberJavadocs(resolveUniqueMember(memberFqn));
    }

    /**
     * Gets a paginated list of all members for a given type.
     * 
     * @param javaType The target JavaType.
     * @param nameQuery Name filter query.
     * @param startIndex Start index for pagination.
     * @param pageSize Page size.
     * @param kindFilters Type kinds filter list.
     * @return Member page.
     * @throws Exception on execution failure.
     */
    @AgiTool("Gets a paginated list of all members (fields, constructors, methods) for a given type. The returned JavaMember objects will not contain a url as they all have the same url, use the returned 'urlOfAllMembers' if you intend to use the returned JavaMember in further calls to getMemberSources(JavaMember) or getMemberJavadocs(JavaMember).")
    public JavaMemberPage getMembers(
            @AgiToolParam("The keychain DTO for the type to inspect.") JavaType javaType, 
            @AgiToolParam(value = "Optional query string to filter members by name ignoring casing (uses memberNameLowerCase.contains(nameQueryLowerCase))", required = false) String nameQuery, 
            @AgiToolParam(value = "The starting index (0-based) for pagination.", required = false) Integer startIndex, 
            @AgiToolParam(value = "The maximum number of results to return per page. Defaults to 108 if not provided.", required = false) Integer pageSize, 
            @AgiToolParam(value = "Optional list of member kinds to filter by.", required = false) List<ElementKind> kindFilters) throws Exception {

        awaitSmart();
        List<JavaMember> allMembers = ReadAction.compute(() -> {
            PsiClass cl = findPsiClass(javaType.getFqn());
            if (cl == null) {
                throw new AgiToolException("Class not found: " + javaType.getFqn());
            }
            return getPsiMembers(cl);
        });
        if (nameQuery != null && !nameQuery.isBlank()) {
            allMembers = allMembers.stream()
                    .filter(m -> m.getName() != null && m.getName().toLowerCase().contains(nameQuery.toLowerCase()))
                    .collect(Collectors.toList());
        }
        if (kindFilters != null && !kindFilters.isEmpty()) {
            allMembers = allMembers.stream()
                    .filter(m -> kindFilters.contains(m.getKind()))
                    .collect(Collectors.toList());
        }

        int start = startIndex != null ? startIndex : 0;
        int size = pageSize != null ? pageSize : 108;
        return new JavaMemberPage(allMembers, start, size, javaType.getUrl());
    }

    /**
     * Gets a paginated list of all members for a type specified by its fully qualified name.
     * 
     * @param fqn The target FQN.
     * @param nameQuery Name filter query.
     * @param startIndex Start index.
     * @param pageSize Page size.
     * @param kindFilters Kinds filter.
     * @return Member page.
     * @throws Exception on execution failure.
     */
    @AgiTool(value = "Gets a paginated list of all members for a type specified by its fully qualified name. Fails if the FQN is ambiguous. The returned JavaMember objects will not contain a url as they all have the same url, use the returned 'urlOfAllMembers' if you intend to use the returned JavaMember in further calls to getMemberSources(JavaMember) or getMemberJavadocs(JavaMember).", permission = ToolPermission.APPROVE_ALWAYS)
    public JavaMemberPage getMembersByFqn(
            @AgiToolParam("The fully qualified name of the type.") String fqn, 
            @AgiToolParam(value = "Optional query string to filter members by name (uses memberName.contains(nameQuery))", required = false) String nameQuery, 
            @AgiToolParam(value = "The starting index (0-based) for pagination.", required = false) Integer startIndex, 
            @AgiToolParam(value = "The maximum number of results to return per page.", required = false) Integer pageSize, 
            @AgiToolParam(value = "Optional list of member kinds to filter by.", required = false) List<ElementKind> kindFilters) throws Exception {
        return getMembers(resolveUniqueType(fqn), nameQuery, startIndex, pageSize, kindFilters);
    }

    /**
     * Finds all types within a given package, with an option for recursive search.
     * 
     * @param packageName Package FQN.
     * @param kindFilter Target kind filter.
     * @param recursive Recursive search.
     * @param startIndex Start index.
     * @param pageSize Page size.
     * @return Paginated Page.
     */
    @AgiTool("Finds all types within a given package, with an option for recursive search. Do not use for packages in open projects if the project's Structure context provider is 'providing' and already including the types of each package")
    public Page<JavaType> findTypesInPackage(
            @AgiToolParam("The fully qualified name of the package to search (e.g., 'java.util').") String packageName,
            @AgiToolParam(value = "Optional kind of type to search for.", required = false) ElementKind kindFilter,
            @AgiToolParam("If true, the search will include all subpackages.") boolean recursive,
            @AgiToolParam(value = "The starting index (0-based) for pagination.", required = false) Integer startIndex,
            @AgiToolParam(value = "The maximum number of results to return per page.", required = false) Integer pageSize) throws AgiToolException {

        awaitSmart();
        List<JavaType> allResults = ReadAction.compute(() -> {
            List<JavaType> results = new ArrayList<>();
            for (Project project : ProjectManager.getInstance().getOpenProjects()) {
                PsiShortNamesCache cache = PsiShortNamesCache.getInstance(project);
                String[] names = cache.getAllClassNames();
                for (String name : names) {
                    PsiClass[] matches = cache.getClassesByName(name, GlobalSearchScope.allScope(project));
                    for (PsiClass cl : matches) {
                        String fqn = cl.getQualifiedName();
                        if (fqn != null) {
                            int lastDot = fqn.lastIndexOf('.');
                            String pkg = lastDot > -1 ? fqn.substring(0, lastDot) : "";
                            boolean matchesPackage = recursive ? pkg.startsWith(packageName) : pkg.equals(packageName);
                            if (matchesPackage) {
                                ElementKind kind = cl.isInterface() ? ElementKind.INTERFACE : (cl.isEnum() ? ElementKind.ENUM : ElementKind.CLASS);
                                if (kindFilter == null || kindFilter == kind) {
                                    addClassToResults(cl, results);
                                }
                            }
                        }
                    }
                }
            }
            return results;
        });

        allResults.sort((t1, t2) -> t1.getFqn().compareTo(t2.getFqn()));
        int start = startIndex != null ? startIndex : 0;
        int size = pageSize != null ? pageSize : 108;
        return new Page<>(allResults, start, size);
    }

    /**
     * Recursively searches for all subtypes of a given JavaType.
     * 
     * @param javaType Starting Type.
     * @param maxDepth Max recursion depth.
     * @return Hierarchy node.
     * @throws Exception on execution failure.
     */
    @AgiTool("Recursively searches for all subtypes (implementations and subclasses) of a given JavaType.")
    public JavaHierarchyNode getSubtypes(
            @AgiToolParam("The keychain DTO for the starting type.") JavaType javaType,
            @AgiToolParam(value = "The maximum depth to recurse. Defaults to 3 if null.", required = false) Integer maxDepth) throws Exception {
        awaitSmart();
        return ReadAction.compute(() -> {
            PsiClass cl = findPsiClass(javaType.getFqn());
            if (cl != null) {
                return getSubtypesNode(cl, maxDepth != null ? maxDepth : 3, 0);
            }
            throw new AgiToolException("Class not found: " + javaType.getFqn());
        });
    }

    /**
     * Recursively searches for all subtypes of a type specified by its fully qualified name.
     * 
     * @param fqn FQN of starting type.
     * @param maxDepth Max depth.
     * @return Hierarchy node.
     * @throws Exception on execution failure.
     */
    @AgiTool(value = "Recursively searches for all subtypes of a type specified by its fully qualified name. Fails if the FQN is ambiguous.", permission = ToolPermission.APPROVE_ALWAYS)
    public JavaHierarchyNode getSubtypesByFqn(
            @AgiToolParam("The fully qualified name of the type.") String fqn,
            @AgiToolParam(value = "The maximum depth to recurse. Defaults to 3.", required = false) Integer maxDepth) throws Exception {
        return getSubtypes(resolveUniqueType(fqn), maxDepth);
    }

    /**
     * Recursively searches for all supertypes of a given JavaType.
     * 
     * @param javaType Starting type.
     * @param maxDepth Max depth.
     * @return Hierarchy node.
     * @throws Exception on execution failure.
     */
    @AgiTool("Recursively searches for all supertypes (base classes and interfaces) of a given JavaType.")
    public JavaHierarchyNode getSupertypes(
            @AgiToolParam("The keychain DTO for the starting type.") JavaType javaType,
            @AgiToolParam(value = "The maximum depth to recurse up. Defaults to 3.", required = false) Integer maxDepth) throws Exception {
        awaitSmart();
        return ReadAction.compute(() -> {
            PsiClass cl = findPsiClass(javaType.getFqn());
            if (cl != null) {
                return getSupertypesNode(cl, maxDepth != null ? maxDepth : 3, 0);
            }
            throw new AgiToolException("Class not found: " + javaType.getFqn());
        });
    }

    /**
     * Recursively searches for all supertypes of a type specified by its fully qualified name.
     * 
     * @param fqn FQN of starting type.
     * @param maxDepth Max depth.
     * @return Hierarchy node.
     * @throws Exception on execution failure.
     */
    @AgiTool(value = "Recursively searches for all supertypes of a type specified by its fully qualified name. Fails if the FQN is ambiguous.", permission = ToolPermission.APPROVE_ALWAYS)
    public JavaHierarchyNode getSupertypesByFqn(
            @AgiToolParam("The fully qualified name of the type.") String fqn,
            @AgiToolParam(value = "The maximum depth to recurse up. Defaults to 3.", required = false) Integer maxDepth) throws Exception {
        return getSupertypes(resolveUniqueType(fqn), maxDepth);
    }

    /**
     * Blocks (bounded) until every open project has finished indexing, so the PSI short-name
     * caches, stub indexes and inheritor searches this toolkit relies on return complete
     * results instead of throwing {@code IndexNotReadyException} mid-index.
     *
     * @throws AgiToolException if any open project is still indexing after the bounded wait.
     */
    private void awaitSmart() throws AgiToolException {
        JavaPsi.requireSmartForOpenProjects();
    }

    /**
     * Finds a PSI class by fully-qualified name across all open projects. Must be called inside a
     * read action.
     *
     * @param fqn the fully-qualified type name.
     * @return the first matching class, or {@code null} if none is found.
     */
    private PsiClass findPsiClass(String fqn) {
        for (Project project : ProjectManager.getInstance().getOpenProjects()) {
            PsiClass cl = JavaPsiFacade.getInstance(project).findClass(fqn, GlobalSearchScope.allScope(project));
            if (cl != null) {
                return cl;
            }
        }
        return null;
    }

    /**
     * Resolves a fully-qualified type name to a unique {@link JavaType} keychain, waiting for
     * indexing to finish first.
     *
     * @param fqn the fully-qualified type name.
     * @return the resolved type keychain.
     * @throws AgiToolException if the type is not found.
     */
    private JavaType resolveUniqueType(String fqn) throws AgiToolException {
        awaitSmart();
        return ReadAction.compute(() -> {
            PsiClass cl = findPsiClass(fqn);
            if (cl != null) {
                return new JavaType(fqn, getUrlOfClass(cl));
            }
            throw new AgiToolException("Type not found: " + fqn);
        });
    }

    /**
     * Resolves a canonical member FQN to a unique {@link JavaMember} keychain, waiting for indexing
     * to finish first.
     *
     * @param memberFqn the canonical member FQN.
     * @return the resolved member keychain.
     * @throws Exception if the member is not found or is ambiguous.
     */
    private JavaMember resolveUniqueMember(String memberFqn) throws Exception {
        awaitSmart();
        return ReadAction.compute(() -> {
            int lastDot = memberFqn.lastIndexOf('.');
            if (lastDot <= 0) {
                throw new AgiToolException("Invalid member FQN: " + memberFqn);
            }
            String typeFqn = memberFqn.substring(0, lastDot);
            PsiClass cl = findPsiClass(typeFqn);
            if (cl == null) {
                throw new AgiToolException("Class not found for member: " + memberFqn);
            }

            List<JavaMember> matches = getPsiMembers(cl).stream()
                    .filter(m -> memberFqn.equals(m.getFqn()))
                    .toList();

            if (matches.size() == 1) {
                return matches.get(0);
            }
            throw new AgiToolException("Member not found or ambiguous: " + memberFqn);
        });
    }

    /**
     * Collects the fields, methods, constructors and inner classes of a PSI class as
     * {@link JavaMember} keychain DTOs. Must be called inside a read action.
     *
     * @param cl the declaring class.
     * @return the list of member keychains.
     */
    private List<JavaMember> getPsiMembers(PsiClass cl) {
        List<JavaMember> members = new ArrayList<>();
        URL url = getUrlOfClass(cl);
        String classFqn = cl.getQualifiedName();

        if (classFqn == null) return members;

        // Fields
        for (PsiField field : cl.getFields()) {
            String name = field.getName();
            String fqn = classFqn + "." + name;
            Set<String> modifiers = getModifiersSet(field.getModifierList());
            members.add(new JavaMember(fqn, name, ElementKind.FIELD, url, modifiers));
        }

        // Methods & Constructors
        for (PsiMethod method : cl.getMethods()) {
            String name = method.getName();
            ElementKind kind = method.isConstructor() ? ElementKind.CONSTRUCTOR : ElementKind.METHOD;
            String fqn = getMethodFqn(cl, method);
            Set<String> modifiers = getModifiersSet(method.getModifierList());
            members.add(new JavaMember(fqn, name, kind, url, modifiers));
        }

        // Inner classes
        for (PsiClass inner : cl.getInnerClasses()) {
            String name = inner.getName();
            String fqn = classFqn + "$" + name;
            Set<String> modifiers = getModifiersSet(inner.getModifierList());
            ElementKind kind = inner.isInterface() ? ElementKind.INTERFACE : (inner.isEnum() ? ElementKind.ENUM : ElementKind.CLASS);
            members.add(new JavaMember(fqn, name, kind, url, modifiers));
        }

        return members;
    }

    /**
     * Builds the canonical method FQN ({@code pkg.Type.name(erasedArg,...)}; constructors use
     * {@code <init>}) so member FQNs round-trip with the other Java toolkits.
     *
     * @param cl     the declaring class.
     * @param method the method.
     * @return the canonical method FQN.
     */
    private String getMethodFqn(PsiClass cl, PsiMethod method) {
        StringBuilder sb = new StringBuilder(cl.getQualifiedName()).append(".");
        if (method.isConstructor()) {
            sb.append("<init>");
        } else {
            sb.append(method.getName());
        }
        sb.append("(");
        PsiParameterList parameterList = method.getParameterList();
        for (int i = 0; i < parameterList.getParametersCount(); i++) {
            sb.append(parameterList.getParameter(i).getType().getCanonicalText());
            if (i < parameterList.getParametersCount() - 1) {
                sb.append(",");
            }
        }
        sb.append(")");
        return sb.toString();
    }

    /**
     * Extracts the Java modifier keywords (public, static, final, …) present on a modifier list.
     *
     * @param modifierList the modifier list, or {@code null}.
     * @return the set of modifier keywords (empty if none or {@code null}).
     */
    private Set<String> getModifiersSet(PsiModifierList modifierList) {
        if (modifierList == null) return Collections.emptySet();
        Set<String> modifiers = new HashSet<>();
        if (modifierList.hasModifierProperty(PsiModifier.PUBLIC)) modifiers.add("public");
        if (modifierList.hasModifierProperty(PsiModifier.PROTECTED)) modifiers.add("protected");
        if (modifierList.hasModifierProperty(PsiModifier.PRIVATE)) modifiers.add("private");
        if (modifierList.hasModifierProperty(PsiModifier.STATIC)) modifiers.add("static");
        if (modifierList.hasModifierProperty(PsiModifier.FINAL)) modifiers.add("final");
        if (modifierList.hasModifierProperty(PsiModifier.ABSTRACT)) modifiers.add("abstract");
        if (modifierList.hasModifierProperty(PsiModifier.SYNCHRONIZED)) modifiers.add("synchronized");
        if (modifierList.hasModifierProperty(PsiModifier.TRANSIENT)) modifiers.add("transient");
        if (modifierList.hasModifierProperty(PsiModifier.VOLATILE)) modifiers.add("volatile");
        return modifiers;
    }

    /**
     * Returns the source text of a member (method, field or inner class) identified by its canonical
     * FQN within a class. Must be called inside a read action.
     *
     * @param cl        the declaring class.
     * @param memberFqn the canonical member FQN.
     * @return the member's source text.
     * @throws Exception if no member matches the FQN.
     */
    private String getPsiMemberSource(PsiClass cl, String memberFqn) throws Exception {
        for (PsiMethod method : cl.getMethods()) {
            if (memberFqn.equals(getMethodFqn(cl, method))) {
                return method.getText();
            }
        }
        for (PsiField field : cl.getFields()) {
            if (memberFqn.equals(cl.getQualifiedName() + "." + field.getName())) {
                return field.getText();
            }
        }
        for (PsiClass inner : cl.getInnerClasses()) {
            if (memberFqn.equals(cl.getQualifiedName() + "$" + inner.getName())) {
                return inner.getText();
            }
        }
        throw new Exception("Member not found: " + memberFqn);
    }

    /**
     * Returns the Javadoc comment text of a member (method, field or inner class) identified by its
     * canonical FQN within a class. Must be called inside a read action.
     *
     * @param cl        the declaring class.
     * @param memberFqn the canonical member FQN.
     * @return the member's Javadoc text, or an empty string if it has none.
     * @throws Exception if resolution fails.
     */
    private String getPsiMemberJavadoc(PsiClass cl, String memberFqn) throws Exception {
        for (PsiMethod method : cl.getMethods()) {
            if (memberFqn.equals(getMethodFqn(cl, method))) {
                PsiDocComment doc = method.getDocComment();
                return doc != null ? doc.getText() : "";
            }
        }
        for (PsiField field : cl.getFields()) {
            if (memberFqn.equals(cl.getQualifiedName() + "." + field.getName())) {
                PsiDocComment doc = field.getDocComment();
                return doc != null ? doc.getText() : "";
            }
        }
        for (PsiClass inner : cl.getInnerClasses()) {
            if (memberFqn.equals(cl.getQualifiedName() + "$" + inner.getName())) {
                PsiDocComment doc = inner.getDocComment();
                return doc != null ? doc.getText() : "";
            }
        }
        return "";
    }

    /**
     * Recursively builds the supertype hierarchy node for a class up to a maximum depth, skipping
     * {@code java.lang.Object}. Must be called inside a read action.
     *
     * @param cl           the starting class.
     * @param maxDepth     the maximum recursion depth.
     * @param currentDepth the current recursion depth.
     * @return the hierarchy node rooted at the class.
     */
    private JavaHierarchyNode getSupertypesNode(PsiClass cl, int maxDepth, int currentDepth) {
        JavaType type = new JavaType(cl.getQualifiedName(), getUrlOfClass(cl));
        JavaHierarchyNode node = new JavaHierarchyNode();
        node.setType(type);
        
        if (currentDepth < maxDepth) {
            for (PsiClass sup : cl.getSupers()) {
                if (sup.getQualifiedName() != null && !sup.getQualifiedName().equals("java.lang.Object")) {
                    node.getSupertypes().add(getSupertypesNode(sup, maxDepth, currentDepth + 1));
                }
            }
        }
        return node;
    }

    /**
     * Recursively builds the subtype (implementors/subclasses) hierarchy node for a class up to a
     * maximum depth via {@code ClassInheritorsSearch}. Must be called inside a read action.
     *
     * @param cl           the starting class.
     * @param maxDepth     the maximum recursion depth.
     * @param currentDepth the current recursion depth.
     * @return the hierarchy node rooted at the class.
     */
    private JavaHierarchyNode getSubtypesNode(PsiClass cl, int maxDepth, int currentDepth) {
        JavaType type = new JavaType(cl.getQualifiedName(), getUrlOfClass(cl));
        JavaHierarchyNode node = new JavaHierarchyNode();
        node.setType(type);
        
        if (currentDepth < maxDepth) {
            try {
                Collection<PsiClass> inheritors = com.intellij.psi.search.searches.ClassInheritorsSearch.search(cl).findAll();
                for (PsiClass sub : inheritors) {
                    if (sub.getQualifiedName() != null) {
                        node.getSubtypes().add(getSubtypesNode(sub, maxDepth, currentDepth + 1));
                    }
                }
            } catch (Exception e) {
                // Ignore search exceptions
            }
        }
        return node;
    }
}
