/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi.nb.tools.java;

import java.net.URL;
import java.util.Collections;
import java.util.EnumSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import javax.lang.model.element.ElementKind;
import javax.lang.model.element.TypeElement;
import lombok.extern.slf4j.Slf4j;
import org.netbeans.api.java.classpath.ClassPath;
import org.netbeans.api.java.classpath.GlobalPathRegistry;
import org.netbeans.api.java.source.ClassIndex;
import org.netbeans.api.java.source.ClasspathInfo;
import org.netbeans.api.java.source.ElementHandle;
import org.netbeans.api.java.source.SourceUtils;
import org.netbeans.spi.java.classpath.support.ClassPathSupport;
import org.openide.filesystems.FileObject;
import uno.anahata.asi.model.Page;
import uno.anahata.asi.nb.tools.files.nb.NbFiles;
import uno.anahata.asi.tool.AiTool;
import uno.anahata.asi.tool.AiToolException;
import uno.anahata.asi.tool.AiToolParam;
import uno.anahata.asi.tool.AiToolkit;
import uno.anahata.asi.tool.AnahataToolkit;

/**
 * Provides tools for interacting with the Java code model in NetBeans.
 * This includes finding types, getting members, and retrieving source code.
 */
@Slf4j
@AiToolkit("A toolkit for browsing types, members, sources and javadocs.")
public class CodeModel extends AnahataToolkit {

    /** {@inheritDoc} */
    @Override
    public List<String> getSystemInstructions() throws Exception {
        String instructions = " CodeModel Toolkit Instructions:\n"
                + "- **Discovery**: Use `findTypes` to search for classes, interfaces, or enums. It returns `JavaType` objects which contain the full `ElementHandle` and `url` for precise identification.\n"
                + "- **Shortcuts (ByFqn)**: If you already know the fully qualified name (FQN) of a type or member, you can use the `ByFqn` methods to skip the discovery turn. These methods will fail if the FQN is ambiguous (e.g., exists in multiple open projects).\n"
                + "- **Member FQNs**: Members are identified by an FQN following the pattern `className.memberName` (e.g., `com.foo.MyClass.myMethod`).\n"
                + "- **Disambiguation**: If a `ByFqn` method fails due to ambiguity, use `findTypes` to get the explicit `JavaType` or `JavaMember` DTO and use the standard methods instead.\n";
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
    @AiTool("Finds multiple Java types matching a query within the aggregated classpath of all open projects and returns a paginated result of minimalist, machine-readable keys. It's the equivalent of a NetBeans Ctrl+O. Use only if\n a) you are researching/exploring the class path\n b) you dont know the exact fqn of a type c) you know for a fact or suspect there are multiple occurences of the same fqn in the classpath. If you know the fqn and you have no reason to believe there are multiple types with the same fqn, just go straight for the getXxxxByFqn")
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
     * Gets the source file for a given JavaType.
     * @param javaType The minimalist keychain DTO from a findTypes call.
     * @return the content of the source file.
     * @throws Exception if the source cannot be retrieved.
     */
    @AiTool("Gets the source file for a given JavaType.")
    public String getTypeSources(
            @AiToolParam("The minimalist keychain DTO from a findTypes call.") JavaType javaType) throws Exception {
        JavaTypeSource source = javaType.getSource();
        FileObject fo = source.getSourceFile();
        if (fo != null && "file".equals(fo.toURL().getProtocol())) {
            getToolkit(NbFiles.class).loadTextFileInternal(fo.getPath(), new uno.anahata.asi.toolkit.files.TextViewportSettings());
            return "requested type sources was a project file, resource added to context with LIVE refresh policy";
        }
        return source.getContent();
    }

    /**
     * Gets the source file for a type specified by its fully qualified name.
     * @param fqn The fully qualified name of the type.
     * @return the content of the source file.
     * @throws Exception if the type is not found or ambiguous.
     */
    @AiTool("Gets the source file for a type specified by its fully qualified name. Fails if the FQN is ambiguous.")
    public String getTypeSourcesByFqn(
            @AiToolParam("The fully qualified name of the type.") String fqn) throws Exception {
        return getTypeSources(resolveUniqueType(fqn));
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
     * @throws Exception if the type is not found or ambiguous.
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
     * @throws Exception if the member is not found or ambiguous.
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
        int size = pageSize != null ? pageSize : 100;

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
    @AiTool("Finds all types within a given package, with an option for recursive search.")
    public Page<JavaType> findTypesInPackage(
            @AiToolParam("The fully qualified name of the package to search (e.g., 'java.util').") String packageName,
            @AiToolParam(value = "Optional kind of type to search for.", required = false) ElementKind kindFilter,
            @AiToolParam("If true, the search will include all subpackages.") boolean recursive,
            @AiToolParam(value = "The starting index (0-based) for pagination.", required = false) Integer startIndex,
            @AiToolParam(value = "The maximum number of results to return per page.", required = false) Integer pageSize) {

        ClasspathInfo cpInfo = getClasspathInfo();

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
        int size = pageSize != null ? pageSize : 100;

        return new Page<>(allResults, start, size);
    }

    private static ClasspathInfo getClasspathInfo() {
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
            throw new AiToolException("Multiple types found for FQN: " + fqn + ". Please use findTypes to select the correct one.");
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
