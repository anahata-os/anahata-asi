/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi.nb.tools.java;

import java.net.URL;
import java.util.Collections;
import java.util.EnumSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import javax.lang.model.element.ElementKind;
import lombok.extern.slf4j.Slf4j;
import org.netbeans.api.java.classpath.ClassPath;
import org.netbeans.api.java.classpath.GlobalPathRegistry;
import org.netbeans.api.java.source.ClassIndex;
import org.netbeans.api.java.source.ClasspathInfo;
import org.netbeans.api.java.source.ElementHandle;
import org.netbeans.api.java.source.SourceUtils;
import org.netbeans.spi.java.classpath.support.ClassPathSupport;
import org.openide.filesystems.FileObject;
import uno.anahata.asi.agi.tool.Page;
import uno.anahata.asi.nb.resources.handle.NbHandle;
import uno.anahata.asi.agi.tool.AiTool;
import uno.anahata.asi.agi.tool.AiToolException;
import uno.anahata.asi.agi.tool.AiToolParam;
import uno.anahata.asi.agi.tool.AiToolkit;
import uno.anahata.asi.agi.tool.AnahataToolkit;

/**
 * Provides tools for interacting with the Java code model in NetBeans.
 * This includes finding types, getting members, and retrieving source code.
 */
@Slf4j
@AiToolkit("A toolkit for browsing types, members, sources and javadocs.")
public class CodeModel extends AnahataToolkit {

    /** 
     * {@inheritDoc} 
     * <p>Provides context-aware instructions for the CodeModel toolkit, 
     * detailing the usage of one-shot FQN methods versus discovery-based searches.</p> 
     */
    @Override
    public List<String> getSystemInstructions() throws Exception {
        String instructions = "CodeModel Toolkit Instructions:\n"                
                + "- **One Shot Methods (`loadXxxxByFqn` or `getXxxxByFqn`)**: If you already know or can work out the fully qualified name (FQN) of a type or member, you can use the `xxxByFqn` methods to skip the discovery turn. These methods will fail if the FQN is ambiguous (e.g., exists in the classpath of multiple open projects).\n"
                + "- **Member FQNs**: Members are identified by an FQN following the pattern `className.memberName` (e.g., `com.foo.MyClass.myMethod`).\n"
                + "- **Disambiguation**: If a `xxxxByFqn` method fails due to ambiguity, use `CodeModel.findTypes` to get the explicit `JavaType` or `JavaMember` DTO and use the standard methods instead.\n"
                + "- **Hierarchy**: Use `getSubtypes` and `getSupertypes` to explore the inheritance tree. These return a recursive `JavaHierarchyNode` structure.\n";
        return Collections.singletonList(instructions);
    }

    /**
     * Finds multiple Java types matching a query and returns a paginated result of minimalist, machine-readable keys.
     * @param query The search query for the types (e.g., simple name, FQN, wildcards).
     * @param caseSensitive Whether the search should be case-sensitive.
     * @param preferOpenProjects Whether to prioritize results from open projects.
     * @param startIndex The starting index (0-based) for pagination.
     * @param pageSize The maximum number of results to return per page.
     * @return a paginated result of JavaType objects.
     */
    @AiTool("Finds any Java types matching a query within the aggregated classpath of all open projects (exactly like NetBeans `Ctrl+O`) and returns a paginated result of minimalist, machine-readable keys. Use only for discovery or disambigutation of fqns (as when there are two types with the same fqn available on the classpath). Don't use it if you aready know the fqn of a type is or you can work it out from the project's `Structure` context provider. Use only if the `CodeModel.loadXxxByFqn` fails due to multiple types with the same fqn, you dont'know the fqn or you are in a discovery adventure.")
    public Page<JavaType> findTypes(
            @AiToolParam("The search query for the types (e.g., simple name, FQN, wildcards).") String query,
            @AiToolParam("Whether the search should be case-sensitive.") boolean caseSensitive,
            @AiToolParam("Whether to prioritize results from open projects.") boolean preferOpenProjects,
            @AiToolParam(value = "The starting index (0-based) for pagination.", required = false) Integer startIndex,
            @AiToolParam(value = "The maximum number of results to return per page.", required = false) Integer pageSize) {

        JavaTypeSearch finder = new JavaTypeSearch(query, caseSensitive, preferOpenProjects);
        List<JavaType> allResults = finder.getResults();

        int start = startIndex != null ? startIndex : 0;
        int size = pageSize != null ? pageSize : 100;

        return new Page<>(allResults, start, size);
    }

    /**
     * Gets the source file for a given JavaType and automatically registers it as a resource.
     * @param javaType The minimalist keychain DTO from a findTypes call.
     * @return a confirmation message.
     * @throws Exception if the source cannot be retrieved.
     */
    @AiTool("Loads the source file for a given `JavaType` (as returned by `Codemodel.findTypes`) as a managed text resource.")
    public String loadTypeSources(
            @AiToolParam("The minimalist keychain DTO from a findTypes call.") JavaType javaType) throws Exception {
        JavaTypeSource source = javaType.getSource();
        FileObject fo = source.getSourceFile();
        if (fo != null) {
            // DIRECT REGISTRATION: Create handle with FileObject to avoid subsequent lookups
            NbHandle handle = new NbHandle(fo);
            String actor = getModelId() + " via @AiTool getTypeSources";
            getAgi().getResourceManager().registerHandle(handle, actor);
            return "Source file '" + fo.getNameExt() + "' registered as a managed resource.";
        }
        return "Source code not available for this type (it may be a library binary without source attached).";
    }

    /**
     * Gets the source file for a type specified by its fully qualified name and registers it as a resource.
     * @param fqn The fully qualified name of the type.
     * @return a confirmation message.
     * @throws Exception if the type is not found or ambiguous.
     */
    @AiTool(value = "Loads the source file for of a java type as a managed resource by its fully qualified name (fqn). Fails if the FQN is ambiguous.", requiresApproval = false)
    public String loadTypeSourcesByFqn(
            @AiToolParam("The fully qualified name of the type.") String fqn) throws Exception {
        return loadTypeSources(resolveUniqueType(fqn));
    }
    
    /**
     * Gets the Javadoc for a given JavaType.
     * @param javaType The keychain DTO for the type to inspect.
     * @return the Javadoc comment.
     * @throws Exception if the Javadoc cannot be retrieved.
     */
    @AiTool("Gets the Javadoc for a given JavaType.")
    public String getTypeJavadocs(
            @AiToolParam("The keychain DTO for the type to inspect.") JavaType javaType) throws Exception {
        return javaType.getJavadoc().getJavadoc();
    }

    /**
     * Gets the Javadoc for a type specified by its fully qualified name.
     * @param fqn The fully qualified name of the type.
     * @return the Javadoc comment.
     * @throws Exception if the Javadoc cannot be found or ambiguous.
     */
    @AiTool("Gets the Javadoc for a type specified by its fully qualified name. Fails if the FQN is ambiguous.")
    public String getTypeJavadocsByFqn(
            @AiToolParam("The fully qualified name of the type.") String fqn) throws Exception {
        return resolveUniqueType(fqn).getJavadoc().getJavadoc();
    }

    /**
     * Gets the source code for a specific JavaMember.
     * @param member The keychain DTO for the member to inspect.
     * @return the source code of the member.
     * @throws Exception if the source cannot be retrieved.
     */
    @AiTool("Gets the source code for a specific JavaMember.")
    public String getMemberSources(
            @AiToolParam("The keychain DTO for the member to inspect.") JavaMember member) throws Exception {
        return member.getSource().getContent();
    }

    /**
     * Gets the source code for a member specified by its fully qualified name.
     * @param memberFqn The FQN of the member (e.g., 'com.foo.Class.method').
     * @return the source code of the member.
     * @throws Exception if the member is not found or ambiguous.
     */
    @AiTool("Gets the source code for a member specified by its fully qualified name. Fails if the FQN is ambiguous.")
    public String getMemberSourcesByFqn(
            @AiToolParam("The fully qualified name of the member.") String memberFqn) throws Exception {
        return resolveUniqueMember(memberFqn).getSource().getContent();
    }

    /**
     * Gets the Javadoc for a specific JavaMember.
     * @param member The keychain DTO for the member to inspect.
     * @return the Javadoc comment.
     * @throws Exception if the Javadoc cannot be retrieved.
     */
    @AiTool("Gets the Javadoc for a specific JavaMember.")
    public String getMemberJavadocs(
            @AiToolParam("The keychain DTO for the member to inspect.") JavaMember member) throws Exception {
        return member.getJavadoc().getJavadoc();
    }

    /**
     * Gets the Javadoc for a member specified by its fully qualified name.
     * @param memberFqn The FQN of the member (e.g., 'com.foo.Class.method').
     * @return the Javadoc comment.
     * @throws Exception if the Javadoc cannot be found or ambiguous.
     */
    @AiTool("Gets the Javadoc for a member specified by its fully qualified name. Fails if the FQN is ambiguous.")
    public String getMemberJavadocsByFqn(
            @AiToolParam("The fully qualified name of the member.") String memberFqn) throws Exception {
        return resolveUniqueMember(memberFqn).getJavadoc().getJavadoc();
    }

    /**
     * Gets a paginated list of all members (fields, constructors, methods) for a given type.
     * @param javaType The keychain DTO for the type to inspect.
     * @param startIndex The starting index (0-based) for pagination.
     * @param pageSize The maximum number of results to return per page.
     * @param kindFilters Optional list of member kinds to filter by (e.g., ['METHOD', 'FIELD']).
     * @return a paginated result of JavaMember objects.
     * @throws Exception if the members cannot be retrieved.
     */
    @AiTool("Gets a paginated list of all members (fields, constructors, methods) for a given type.")
    public Page<JavaMember> getMembers(
            @AiToolParam("The keychain DTO for the type to inspect.") JavaType javaType,
            @AiToolParam(value = "The starting index (0-based) for pagination.", required = false) Integer startIndex,
            @AiToolParam(value = "The maximum number of results to return per page.", required = false) Integer pageSize,
            @AiToolParam(value = "Optional list of member kinds to filter by.", required = false) List<ElementKind> kindFilters) throws Exception {
        
        List<JavaMember> allMembers = javaType.getMembers();
        
        if (kindFilters != null && !kindFilters.isEmpty()) {
            allMembers = allMembers.stream()
                    .filter(m -> kindFilters.contains(m.getKind()))
                    .collect(Collectors.toList());
        }

        int start = startIndex != null ? startIndex : 0;
        int size = pageSize != null ? pageSize : 108;

        return new Page<>(allMembers, start, size);
    }

    /**
     * Gets a paginated list of all members for a type specified by its fully qualified name.
     * @param fqn The fully qualified name of the type.
     * @param startIndex The starting index (0-based) for pagination.
     * @param pageSize The maximum number of results to return per page.
     * @param kindFilters Optional list of member kinds to filter by.
     * @return a paginated result of JavaMember objects.
     * @throws Exception if the type is not found or ambiguous.
     */
    @AiTool("Gets a paginated list of all members for a type specified by its fully qualified name. Fails if the FQN is ambiguous.")
    public Page<JavaMember> getMembersByFqn(
            @AiToolParam("The fully qualified name of the type.") String fqn,
            @AiToolParam(value = "The starting index (0-based) for pagination.", required = false) Integer startIndex,
            @AiToolParam(value = "The maximum number of results to return per page.", required = false) Integer pageSize,
            @AiToolParam(value = "Optional list of member kinds to filter by.", required = false) List<ElementKind> kindFilters) throws Exception {
        return getMembers(resolveUniqueType(fqn), startIndex, pageSize, kindFilters);
    }

    /**
     * Finds all types within a given package, with an option for recursive search.
     * @param packageName The fully qualified name of the package to search (e.g., 'java.util').
     * @param kindFilter Optional kind of type to search for (CLASS, INTERFACE, etc.).
     * @param recursive If true, the search will include all subpackages.
     * @param startIndex The starting index (0-based) for pagination.
     * @param pageSize The maximum number of results to return per page.
     * @return a paginated result of JavaType objects.
     */
    @AiTool("Finds all types within a given package, with an option for recursive search. Do not use for packages in open projects if the project's Structure context provider is 'providing' and already including the types of each package")
    public Page<JavaType> findTypesInPackage(
            @AiToolParam("The fully qualified name of the package to search (e.g., 'java.util').") String packageName,
            @AiToolParam(value = "Optional kind of type to search for.", required = false) ElementKind kindFilter,
            @AiToolParam("If true, the search will include all subpackages.") boolean recursive,
            @AiToolParam(value = "The starting index (0-based) for pagination.", required = false) Integer startIndex,
            @AiToolParam(value = "The maximum number of results to return per page.", required = false) Integer pageSize) {

        ClasspathInfo cpInfo = getGlobalClasspathInfo();

        Set<ElementHandle<javax.lang.model.element.TypeElement>> declaredTypes = cpInfo.getClassIndex().getDeclaredTypes(
                "", ClassIndex.NameKind.PREFIX, EnumSet.allOf(ClassIndex.SearchScope.class));

        List<JavaType> allResults = declaredTypes.stream()
                .filter(handle -> {
                    String fqn = handle.getQualifiedName();
                    int lastDot = fqn.lastIndexOf('.');
                    String pkg = lastDot > -1 ? fqn.substring(0, lastDot) : "";
                    if (recursive) {
                        return pkg.startsWith(packageName);
                    } else {
                        return pkg.equals(packageName);
                    }
                })
                .filter(handle -> kindFilter == null || handle.getKind() == kindFilter)
                .map(handle -> {
                    FileObject fo = SourceUtils.getFile(handle, cpInfo);
                    URL url = null;
                    try {
                        if (fo != null) url = fo.toURL();
                    } catch (Exception e) {
                        log.warn("Failed to resolve URL for handle: {}", handle.getQualifiedName());
                    }
                    return new JavaType(handle, url);
                })
                .sorted((t1, t2) -> t1.getFqn().compareTo(t2.getFqn()))
                .collect(Collectors.toList());

        int start = startIndex != null ? startIndex : 0;
        int size = pageSize != null ? pageSize : 108;

        return new Page<>(allResults, start, size);
    }

    /**
     * Recursively searches for all subtypes (implementations and subclasses) of a given JavaType.
     * 
     * @param javaType The starting type.
     * @param maxDepth The maximum depth to recurse. Defaults to 3 if null.
     * @return A recursive JavaHierarchyNode structure.
     * @throws Exception if the search fails.
     */
    @AiTool("Recursively searches for all subtypes (implementations and subclasses) of a given JavaType.")
    public JavaHierarchyNode getSubtypes(
            @AiToolParam("The keychain DTO for the starting type.") JavaType javaType,
            @AiToolParam(value = "The maximum depth to recurse. Defaults to 3.", required = false) Integer maxDepth) throws Exception {
        return new JavaSubtypeSearch(javaType, maxDepth != null ? maxDepth : 3).getRootNode();
    }

    /**
     * Recursively searches for all subtypes of a type specified by its fully qualified name.
     * 
     * @param fqn The fully qualified name of the type.
     * @param maxDepth The maximum depth to recurse. Defaults to 3 if null.
     * @return A recursive JavaHierarchyNode structure.
     * @throws Exception if the type is not found or ambiguous.
     */
    @AiTool("Recursively searches for all subtypes of a type specified by its fully qualified name. Fails if the FQN is ambiguous.")
    public JavaHierarchyNode getSubtypesByFqn(
            @AiToolParam("The fully qualified name of the type.") String fqn,
            @AiToolParam(value = "The maximum depth to recurse. Defaults to 3.", required = false) Integer maxDepth) throws Exception {
        return getSubtypes(resolveUniqueType(fqn), maxDepth);
    }

    /**
     * Recursively searches for all supertypes (base classes and interfaces) of a given JavaType.
     * 
     * @param javaType The starting type.
     * @param maxDepth The maximum depth to recurse up. Defaults to 3 if null.
     * @return A recursive JavaHierarchyNode structure.
     * @throws Exception if the search fails.
     */
    @AiTool("Recursively searches for all supertypes (base classes and interfaces) of a given JavaType.")
    public JavaHierarchyNode getSupertypes(
            @AiToolParam("The keychain DTO for the starting type.") JavaType javaType,
            @AiToolParam(value = "The maximum depth to recurse up. Defaults to 3.", required = false) Integer maxDepth) throws Exception {
        return new JavaSupertypeSearch(javaType, maxDepth != null ? maxDepth : 3).getRootNode();
    }

    /**
     * Recursively searches for all supertypes of a type specified by its fully qualified name.
     * 
     * @param fqn The fully qualified name of the type.
     * @param maxDepth The maximum depth to recurse up. Defaults to 3 if null.
     * @return A recursive JavaHierarchyNode structure.
     * @throws Exception if the type is not found or ambiguous.
     */
    @AiTool("Recursively searches for all supertypes of a type specified by its fully qualified name. Fails if the FQN is ambiguous.")
    public JavaHierarchyNode getSupertypesByFqn(
            @AiToolParam("The fully qualified name of the type.") String fqn,
            @AiToolParam(value = "The maximum depth to recurse up. Defaults to 3.", required = false) Integer maxDepth) throws Exception {
        return getSupertypes(resolveUniqueType(fqn), maxDepth);
    }

    /**
     * Builds a global ClasspathInfo of all SOURCE, COMPILE and BOOT classpaths of all open projects.
     * 
     * @return All classpaths of all open projects 
     */
    private static ClasspathInfo getGlobalClasspathInfo() {
        Set<ClassPath> sourcePaths = GlobalPathRegistry.getDefault().getPaths(ClassPath.SOURCE);
        Set<ClassPath> compilePaths = GlobalPathRegistry.getDefault().getPaths(ClassPath.COMPILE);
        Set<ClassPath> bootPaths = GlobalPathRegistry.getDefault().getPaths(ClassPath.BOOT);
        ClassPath sourceCp = ClassPathSupport.createProxyClassPath(sourcePaths.toArray(new ClassPath[0]));
        ClassPath compileCp = ClassPathSupport.createProxyClassPath(compilePaths.toArray(new ClassPath[0]));
        ClassPath bootCp = ClassPathSupport.createProxyClassPath(bootPaths.toArray(new ClassPath[0]));
        return ClasspathInfo.create(bootCp, compileCp, sourceCp);
    }

    /**
     * Resolves a fully qualified name to a unique JavaType.
     * @param fqn The fully qualified name.
     * @return the unique JavaType.
     * @throws AiToolException if the type is not found or ambiguous.
     */
    private JavaType resolveUniqueType(String fqn) throws AiToolException {
        log.info("Resolving unique type for FQN: {}", fqn);
        
        // 1. Try exact FQN search
        JavaTypeSearch search = new JavaTypeSearch(fqn, true, true);
        List<JavaType> results = search.getResults().stream()
                .filter(t -> fqn.equals(t.getFqn()))
                .collect(Collectors.toList());

        if (results.isEmpty()) {
            log.info("Exact FQN search failed for {}. Falling back to simple name search.", fqn);
            // 2. Fallback: Search by simple name and filter (TypeProvider is optimized for simple names)
            String simpleName = fqn.contains(".") ? fqn.substring(fqn.lastIndexOf('.') + 1) : fqn;
            search = new JavaTypeSearch(simpleName, true, true);
            results = search.getResults().stream()
                    .filter(t -> fqn.equals(t.getFqn()))
                    .collect(Collectors.toList());
            log.info("Simple name search for '{}' returned {} matches for FQN '{}'.", simpleName, results.size(), fqn);
        }

        if (results.isEmpty()) {
            throw new AiToolException("Type not found: " + fqn);
        }

        if (results.size() > 1) {
            log.warn("Ambiguous FQN: {}. Found {} matches.", fqn, results.size());
            throw new AiToolException("Multiple types found for FQN: " + fqn + ": " + results + ". Please use the tool that takes a JavaType as a parameter to specify or the Resources Toolkit if you now the url.");
        }

        log.info("Successfully resolved unique type: {} -> {}", fqn, results.get(0).getUrl());
        return results.get(0);
    }

    /**
     * Resolves a member FQN to a unique JavaMember.
     * @param memberFqn The member FQN (e.g., 'com.foo.Class.method').
     * @return the unique JavaMember.
     * @throws Exception if the member is not found or ambiguous.
     */
    private JavaMember resolveUniqueMember(String memberFqn) throws Exception {
        int lastDot = memberFqn.lastIndexOf('.');
        if (lastDot == -1) {
            throw new AiToolException("Invalid member FQN: " + memberFqn + ". Expected format: className.memberName");
        }
        String typeFqn = memberFqn.substring(0, lastDot);
        String memberName = memberFqn.substring(lastDot + 1);
        
        JavaType type = resolveUniqueType(typeFqn);
        List<JavaMember> matches = type.getMembers().stream()
                .filter(m -> memberName.equals(m.getName()))
                .collect(Collectors.toList());

        if (matches.isEmpty()) {
            throw new AiToolException("Member not found: " + memberName + " in type " + typeFqn);
        }

        if (matches.size() > 1) {
            throw new AiToolException("Multiple members found for name: " + memberName + " in type " + typeFqn + " (overloads). Please use getMembers to select the correct one.");
        }

        return matches.get(0);
    }
}
