/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi.nb.tools.java;


import java.io.File;
import java.nio.file.Path;
import java.util.EnumSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;
import javax.lang.model.element.TypeElement;
import org.netbeans.api.java.project.JavaProjectConstants;
import org.netbeans.api.java.source.ClassIndex;
import org.netbeans.api.java.source.ClasspathInfo;
import org.netbeans.api.java.source.ElementHandle;
import org.netbeans.api.java.source.SourceUtils;
import org.netbeans.api.project.Project;
import org.netbeans.api.project.ProjectUtils;
import org.netbeans.api.project.SourceGroup;
import org.netbeans.api.project.Sources;
import org.netbeans.api.project.ui.OpenProjects;
import org.netbeans.modules.java.source.ui.JavaTypeDescription;
import org.netbeans.modules.web.common.spi.ImportantFilesImplementation.FileInfo;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import uno.anahata.asi.model.resource.TextFileResource;
import uno.anahata.asi.nb.tools.Page;
import uno.anahata.asi.tool.AiTool;
import uno.anahata.asi.tool.AiToolParam;

/**
 * Provides tools for interacting with the Java code model in NetBeans.
 * This includes finding types, getting members, and retrieving source code.
 */
public class CodeModel {

    /**
     * Private constructor to prevent instantiation of this utility class.
     */
    private CodeModel() {
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
            @AiToolParam("The starting index (0-based) for pagination.") Integer startIndex,
            @AiToolParam("The maximum number of results to return per page.") Integer pageSize) {

        JavaTypeSearch finder = new JavaTypeSearch(query, caseSensitive, preferOpenProjects);
        List<JavaType> allResults = finder.getResults();

        int start = startIndex != null ? startIndex : 0;
        int size = pageSize != null ? pageSize : 100;

        return new Page<>(allResults, start, size);
    }

    /**
     * Gets the source file for a given JavaType. This is the second step in the 'discovery' (Ctrl+O) workflow.
     * @param javaType The minimalist keychain DTO from a findTypes call.
     * @return the content of the source file.
     * @throws Exception if the source cannot be retrieved.
     */
    @AiTool("Gets the source file for a given JavaType. This is the second step in the 'discovery' (Ctrl+O) workflow.")
    public static String getSources(
            @AiToolParam("The minimalist keychain DTO from a findTypes call.") JavaType javaType) throws Exception {
        return javaType.getSource().getContent();
    }
    
    /**
     * Gets a list of all members (fields, constructors, methods) for a given type.
     * @param javaType The keychain DTO for the type to inspect.
     * @return a list of JavaMember objects.
     * @throws Exception if the members cannot be retrieved.
     */
    @AiTool("Gets a list of all members (fields, constructors, methods) for a given type.")
    public static List<JavaMember> getMembers(
            @AiToolParam("The keychain DTO for the type to inspect.") JavaType javaType) throws Exception {
        return javaType.getMembers();
    }

    
    
}