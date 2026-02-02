/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi.nb.tools.java;

import java.util.List;
import java.util.stream.Collectors;
import javax.lang.model.element.ElementKind;
import uno.anahata.asi.model.Page;
import uno.anahata.asi.tool.AiTool;
import uno.anahata.asi.tool.AiToolParam;
import uno.anahata.asi.tool.AiToolkit;

/**
 * Provides tools for interacting with the Java code model in NetBeans.
 * This includes finding types, getting members, and retrieving source code.
 */
@AiToolkit("A toolkit for browsing types, members, sources and javadocs.")
public class CodeModel {

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
            @AiToolParam("The starting index (0-based) for pagination.") Integer startIndex,
            @AiToolParam("The maximum number of results to return per page.") Integer pageSize) {

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
}
