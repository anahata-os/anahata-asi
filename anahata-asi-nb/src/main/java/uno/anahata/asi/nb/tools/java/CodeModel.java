/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi.nb.tools.java;

import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import javax.lang.model.element.ElementKind;
import uno.anahata.asi.model.Page;
import uno.anahata.asi.tool.AiTool;
import uno.anahata.asi.tool.AiToolException;
import uno.anahata.asi.tool.AiToolParam;
import uno.anahata.asi.tool.AiToolkit;
import uno.anahata.asi.tool.AnahataToolkit;

/**
 * Provides tools for interacting with the Java code model in NetBeans.
 * This includes finding types, getting members, and retrieving source code.
 */
@AiToolkit("A toolkit for browsing types, members, sources and javadocs.")
public class CodeModel extends AnahataToolkit {

    /** {@inheritDoc} */
    @Override
    public List<String> getSystemInstructions() throws Exception {
        String instructions = "### CodeModel Toolkit Instructions:\n"
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
    @AiTool("Finds multiple Java types matching a query and returns a paginated result of minimalist, machine-readable keys.")
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
        return javaType.getSource().getContent();
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
        return resolveUniqueType(fqn).getSource().getContent();
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
     * Resolves a fully qualified name to a unique JavaType.
     * @param fqn The fully qualified name.
     * @return the unique JavaType.
     * @throws AiToolException if the type is not found or ambiguous.
     */
    private JavaType resolveUniqueType(String fqn) throws AiToolException {
        JavaTypeSearch search = new JavaTypeSearch(fqn, true, true);
        List<JavaType> results = search.getResults().stream()
                .filter(t -> fqn.equals(t.getFqn()))
                .collect(Collectors.toList());

        if (results.isEmpty()) {
            throw new AiToolException("Type not found: " + fqn);
        }

        if (results.size() > 1) {
            throw new AiToolException("Multiple types found for FQN: " + fqn + ". Please use findTypes to select the correct one.");
        }

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
