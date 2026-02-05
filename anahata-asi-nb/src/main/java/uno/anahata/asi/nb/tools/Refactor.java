/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.tools;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import javax.lang.model.element.Element;
import javax.lang.model.element.ExecutableElement;
import javax.lang.model.element.TypeElement;
import javax.lang.model.element.VariableElement;
import lombok.extern.slf4j.Slf4j;
import org.netbeans.api.java.source.CompilationController;
import org.netbeans.api.java.source.ElementHandle;
import org.netbeans.api.java.source.JavaSource;
import org.netbeans.api.java.source.Task;
import org.netbeans.api.java.source.TreePathHandle;
import org.netbeans.modules.refactoring.api.AbstractRefactoring;
import org.netbeans.modules.refactoring.api.CopyRefactoring;
import org.netbeans.modules.refactoring.api.MoveRefactoring;
import org.netbeans.modules.refactoring.api.Problem;
import org.netbeans.modules.refactoring.api.RefactoringElement;
import org.netbeans.modules.refactoring.api.RefactoringSession;
import org.netbeans.modules.refactoring.api.RenameRefactoring;
import org.netbeans.modules.refactoring.api.SafeDeleteRefactoring;
import org.netbeans.modules.refactoring.api.WhereUsedQuery;
import org.netbeans.modules.refactoring.java.api.EncapsulateFieldRefactoring;
import org.netbeans.modules.refactoring.java.api.ExtractInterfaceRefactoring;
import org.netbeans.modules.refactoring.java.api.InlineRefactoring;
import org.netbeans.modules.refactoring.java.api.InvertBooleanRefactoring;
import org.netbeans.modules.refactoring.java.api.MemberInfo;
import org.netbeans.modules.refactoring.java.api.PullUpRefactoring;
import org.netbeans.modules.refactoring.java.api.PushDownRefactoring;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import org.openide.util.Lookup;
import org.openide.util.lookup.Lookups;
import uno.anahata.asi.tool.AiTool;
import uno.anahata.asi.tool.AiToolParam;
import uno.anahata.asi.tool.AiToolkit;

/**
 * A toolkit for performing programmatic refactoring operations within the NetBeans IDE.
 * This toolkit leverages the NetBeans Refactoring API to ensure that changes are 
 * propagated correctly across the project (e.g., updating imports, references, etc.).
 * 
 * @author anahata-gemini-pro-2.5
 */
@Slf4j
@AiToolkit("Programmatic refactoring tools for NetBeans. Use these tools to safely rename, move, copy, or delete code elements while maintaining project integrity.")
public class Refactor {

    /**
     * Performs a programmatic rename refactoring of a file or class within the IDE.
     *
     * @param filePath The absolute path of the file to rename.
     * @param newName  The new name for the file or class (without the extension).
     * @return A detailed log of the refactoring process.
     * @throws Exception if there is an error invoking the operation.
     */
    @AiTool("Renames a file or class. This is a 'safe' rename that updates all references in the project.")
    public String rename(
            @AiToolParam(value = "The absolute path of the file to rename.", rendererId = "path") String filePath, 
            @AiToolParam("The new name (without extension).") String newName) throws Exception {
        FileObject fo = getFileObject(filePath);
        RenameRefactoring refactoring = new RenameRefactoring(getLookupForFile(fo));
        refactoring.setNewName(newName);
        
        return executeRefactoring(refactoring, "Rename " + fo.getName());
    }

    /**
     * Performs a programmatic move refactoring of a file or class to a new destination folder.
     *
     * @param filePath       The absolute path of the file to move.
     * @param targetFolderPath The absolute path of the destination folder.
     * @return A detailed log of the refactoring process.
     * @throws Exception if there is an error invoking the operation.
     */
    @AiTool("Moves a file or class to a different package or folder, updating all references.")
    public String move(
            @AiToolParam(value = "The absolute path of the file to move.", rendererId = "path") String filePath, 
            @AiToolParam(value = "The absolute path of the target folder.", rendererId = "path") String targetFolderPath) throws Exception {
        FileObject sourceFo = getFileObject(filePath);
        FileObject targetFo = getFileObject(targetFolderPath);
        
        if (!targetFo.isFolder()) {
            throw new IllegalArgumentException("Target path must be a folder: " + targetFolderPath);
        }

        MoveRefactoring refactoring = new MoveRefactoring(getLookupForFile(sourceFo));
        refactoring.setTarget(Lookups.singleton(targetFo));
        
        String result = executeRefactoring(refactoring, "Move " + sourceFo.getName());
        
        // Post-check for cross-project moves
        File targetFile = new File(FileUtil.toFile(targetFo), sourceFo.getNameExt());
        if (!targetFile.exists()) {
            result += "\nWARNING: Target file not found at " + targetFile.getPath() + ". The refactoring might have failed or is not supported across these locations.";
        }
        
        return result;
    }

    /**
     * Performs a programmatic copy refactoring of a file or class to a destination folder.
     *
     * @param filePath       The absolute path of the file to copy.
     * @param targetFolderPath The absolute path of the destination folder.
     * @return A detailed log of the refactoring process.
     * @throws Exception if there is an error invoking the operation.
     */
    @AiTool("Copies a file or class to a different package or folder.")
    public String copy(
            @AiToolParam(value = "The absolute path of the file to copy.", rendererId = "path") String filePath, 
            @AiToolParam(value = "The absolute path of the target folder.", rendererId = "path") String targetFolderPath) throws Exception {
        FileObject sourceFo = getFileObject(filePath);
        FileObject targetFo = getFileObject(targetFolderPath);
        
        if (!targetFo.isFolder()) {
            throw new IllegalArgumentException("Target path must be a folder: " + targetFolderPath);
        }

        CopyRefactoring refactoring = new CopyRefactoring(getLookupForFile(sourceFo));
        refactoring.setTarget(Lookups.singleton(targetFo));
        
        String result = executeRefactoring(refactoring, "Copy " + sourceFo.getName());

        // Post-check
        File targetFile = new File(FileUtil.toFile(targetFo), sourceFo.getNameExt());
        if (!targetFile.exists()) {
            result += "\nWARNING: Target file not found at " + targetFile.getPath() + ".";
        }

        return result;
    }

    /**
     * Performs a programmatic safe delete refactoring of a file or class.
     *
     * @param filePath        The absolute path of the file to delete.
     * @param checkInComments Whether to search for usages in comments and strings.
     * @return A detailed log of the refactoring process.
     * @throws Exception if there is an error invoking the operation.
     */
    @AiTool("Deletes a file or class only if it is safe to do so (i.e., no active usages).")
    public String safeDelete(
            @AiToolParam(value = "The absolute path of the file to delete.", rendererId = "path") String filePath,
            @AiToolParam("Whether to check for usages in comments.") boolean checkInComments) throws Exception {
        FileObject fo = getFileObject(filePath);
        SafeDeleteRefactoring refactoring = new SafeDeleteRefactoring(getLookupForFile(fo));
        refactoring.setCheckInComments(checkInComments);
        
        return executeRefactoring(refactoring, "Safe Delete " + fo.getName());
    }

    /**
     * Inlines a method, constant, or variable.
     * 
     * @param filePath The absolute path of the Java file.
     * @param memberName The name of the member to inline.
     * @param type The type of inlining to perform (METHOD, TEMP, CONSTANT).
     * @return A detailed log of the refactoring process.
     * @throws Exception if the operation fails.
     */
    @AiTool("Inlines a method, constant, or variable, replacing all usages with its body/value.")
    public String inline(
            @AiToolParam(value = "The absolute path of the Java file.", rendererId = "path") String filePath,
            @AiToolParam("The name of the member to inline.") String memberName,
            @AiToolParam("The type of inlining (METHOD, TEMP, CONSTANT).") InlineRefactoring.Type type) throws Exception {
        FileObject fo = getFileObject(filePath);
        TreePathHandle handle = getTreePathHandleForMember(fo, memberName);
        if (handle == null) {
            return "Member '" + memberName + "' not found in " + filePath;
        }

        InlineRefactoring refactoring = new InlineRefactoring(handle, type);
        return executeRefactoring(refactoring, "Inline " + memberName);
    }

    /**
     * Encapsulates a field by creating a getter and setter and updating all usages.
     * 
     * @param filePath The absolute path of the Java file.
     * @param fieldName The name of the field to encapsulate.
     * @return A detailed log of the refactoring process.
     * @throws Exception if the operation fails.
     */
    @AiTool("Encapsulates a field by creating a getter and setter and updating all references to use them.")
    public String encapsulateField(
            @AiToolParam(value = "The absolute path of the Java file.", rendererId = "path") String filePath,
            @AiToolParam("The name of the field to encapsulate.") String fieldName) throws Exception {
        FileObject fo = getFileObject(filePath);
        TreePathHandle handle = getTreePathHandleForMember(fo, fieldName);
        if (handle == null) {
            return "Field '" + fieldName + "' not found in " + filePath;
        }

        EncapsulateFieldRefactoring refactoring = new EncapsulateFieldRefactoring(handle);
        return executeRefactoring(refactoring, "Encapsulate Field " + fieldName);
    }

    /**
     * Inverts a boolean method or variable.
     * 
     * @param filePath The absolute path of the Java file.
     * @param memberName The name of the boolean member to invert.
     * @param newName The new name for the inverted member.
     * @return A detailed log of the refactoring process.
     * @throws Exception if the operation fails.
     */
    @AiTool("Inverts the logic of a boolean method or variable and updates all call sites accordingly.")
    public String invertBoolean(
            @AiToolParam(value = "The absolute path of the Java file.", rendererId = "path") String filePath,
            @AiToolParam("The name of the boolean member to invert.") String memberName,
            @AiToolParam("The new name for the inverted member.") String newName) throws Exception {
        FileObject fo = getFileObject(filePath);
        TreePathHandle handle = getTreePathHandleForMember(fo, memberName);
        if (handle == null) {
            return "Member '" + memberName + "' not found in " + filePath;
        }

        InvertBooleanRefactoring refactoring = new InvertBooleanRefactoring(handle);
        refactoring.setNewName(newName);
        return executeRefactoring(refactoring, "Invert Boolean " + memberName);
    }

    /**
     * Extracts an interface from a class.
     * 
     * @param filePath The absolute path of the Java file containing the class.
     * @param interfaceName The name of the new interface.
     * @param memberNames The names of the members (methods/fields) to include in the interface.
     * @return A detailed log of the refactoring process.
     * @throws Exception if the operation fails.
     */
    @AiTool("Extracts an interface from a class, moving selected members to the new interface.")
    public String extractInterface(
            @AiToolParam(value = "The absolute path of the Java file.", rendererId = "path") String filePath,
            @AiToolParam("The name of the new interface.") String interfaceName,
            @AiToolParam("The names of the members to extract.") List<String> memberNames) throws Exception {
        FileObject fo = getFileObject(filePath);
        TreePathHandle classHandle = getTreePathHandleForClass(fo);
        if (classHandle == null) {
            return "Class not found in " + filePath;
        }

        ExtractInterfaceRefactoring refactoring = new ExtractInterfaceRefactoring(classHandle);
        refactoring.setInterfaceName(interfaceName);
        
        List<ElementHandle<ExecutableElement>> methods = new ArrayList<>();
        List<ElementHandle<VariableElement>> fields = new ArrayList<>();
        
        resolveMembers(fo, memberNames, methods, fields);
        
        refactoring.setMethods(methods);
        refactoring.setFields(fields);
        
        return executeRefactoring(refactoring, "Extract Interface " + interfaceName);
    }

    /**
     * Pulls up members to a superclass.
     * 
     * @param filePath The absolute path of the Java file containing the subclass.
     * @param targetClassFqn The fully qualified name of the target superclass.
     * @param memberNames The names of the members to pull up.
     * @return A detailed log of the refactoring process.
     * @throws Exception if the operation fails.
     */
    @AiTool("Pulls up selected members from a class to one of its superclasses.")
    public String pullUp(
            @AiToolParam(value = "The absolute path of the Java file.", rendererId = "path") String filePath,
            @AiToolParam("The FQN of the target superclass.") String targetClassFqn,
            @AiToolParam("The names of the members to pull up.") List<String> memberNames) throws Exception {
        FileObject fo = getFileObject(filePath);
        TreePathHandle classHandle = getTreePathHandleForClass(fo);
        if (classHandle == null) {
            return "Class not found in " + filePath;
        }

        PullUpRefactoring refactoring = new PullUpRefactoring(classHandle);
        
        // Resolve target type
        ElementHandle<TypeElement> targetHandle = ElementHandle.createTypeElementHandle(javax.lang.model.element.ElementKind.CLASS, targetClassFqn);
        refactoring.setTargetType(targetHandle);
        
        // Resolve members to MemberInfo
        List<MemberInfo> members = new ArrayList<>();
        resolveMemberInfos(fo, memberNames, members);
        
        refactoring.setMembers(members.toArray(new MemberInfo[0]));
        
        return executeRefactoring(refactoring, "Pull Up to " + targetClassFqn);
    }

    /**
     * Pushes down members to subclasses.
     * 
     * @param filePath The absolute path of the Java file containing the superclass.
     * @param memberNames The names of the members to push down.
     * @return A detailed log of the refactoring process.
     * @throws Exception if the operation fails.
     */
    @AiTool("Pushes down selected members from a class to its subclasses.")
    public String pushDown(
            @AiToolParam(value = "The absolute path of the Java file.", rendererId = "path") String filePath,
            @AiToolParam("The names of the members to push down.") List<String> memberNames) throws Exception {
        FileObject fo = getFileObject(filePath);
        TreePathHandle classHandle = getTreePathHandleForClass(fo);
        if (classHandle == null) {
            return "Class not found in " + filePath;
        }

        PushDownRefactoring refactoring = new PushDownRefactoring(classHandle);
        
        // Resolve members to MemberInfo
        List<MemberInfo> members = new ArrayList<>();
        resolveMemberInfos(fo, memberNames, members);
        
        refactoring.setMembers(members.toArray(new MemberInfo[0]));
        
        return executeRefactoring(refactoring, "Push Down from " + fo.getName());
    }

    /**
     * Finds all usages of a file or class within the project.
     *
     * @param filePath        The absolute path of the file to search for.
     * @param searchInComments Whether to search for usages in comments and strings.
     * @return A formatted list of all found usages.
     * @throws Exception if there is an error invoking the query.
     */
    @AiTool("Finds all references/usages of a file or class in the project.")
    public String whereUsed(
            @AiToolParam(value = "The absolute path of the file to search for.", rendererId = "path") String filePath,
            @AiToolParam("Whether to search in comments.") boolean searchInComments) throws Exception {
        FileObject fo = getFileObject(filePath);
        WhereUsedQuery query = new WhereUsedQuery(getLookupForFile(fo));
        query.putValue(WhereUsedQuery.FIND_REFERENCES, true);
        query.putValue(WhereUsedQuery.SEARCH_IN_COMMENTS, searchInComments);

        RefactoringSession session = RefactoringSession.create("Where Used: " + fo.getName());
        Problem p = query.prepare(session);
        if (p != null && p.isFatal()) {
            return "Fatal error during query preparation: " + p.getMessage();
        }

        Collection<RefactoringElement> elements = session.getRefactoringElements();
        if (elements.isEmpty()) {
            return "No usages found for " + filePath;
        }

        StringBuilder sb = new StringBuilder("Found ").append(elements.size()).append(" usages:\n");
        for (RefactoringElement element : elements) {
            sb.append("- ").append(element.getDisplayText()).append(" (").append(element.getParentFile().getPath()).append(")\n");
        }
        return sb.toString();
    }

    /**
     * Finds all usages of a specific member (field or method) within a Java file.
     *
     * @param filePath        The absolute path of the Java file.
     * @param memberName      The name of the member to search for.
     * @param searchInComments Whether to search for usages in comments and strings.
     * @return A formatted list of all found usages.
     * @throws Exception if there is an error invoking the query.
     */
    @AiTool("Finds all references/usages of a specific class member (method or field) in the project.")
    public String whereUsedMember(
            @AiToolParam(value = "The absolute path of the Java file.", rendererId = "path") String filePath,
            @AiToolParam("The name of the member (method or field).") String memberName,
            @AiToolParam("Whether to search in comments.") boolean searchInComments) throws Exception {
        FileObject fo = getFileObject(filePath);
        if (!"java".equals(fo.getExt())) {
            throw new IllegalArgumentException("Member search is only supported for Java files.");
        }

        ElementHandle handle = getElementHandleForMember(fo, memberName);
        if (handle == null) {
            return "Member '" + memberName + "' not found in " + filePath;
        }

        // CRITICAL: For member search, the lookup should contain the ElementHandle
        WhereUsedQuery query = new WhereUsedQuery(Lookups.fixed(handle));
        query.putValue(WhereUsedQuery.FIND_REFERENCES, true);
        query.putValue(WhereUsedQuery.SEARCH_IN_COMMENTS, searchInComments);

        RefactoringSession session = RefactoringSession.create("Where Used Member: " + memberName);
        Problem p = query.prepare(session);
        if (p != null && p.isFatal()) {
            return "Fatal error during query preparation: " + p.getMessage();
        }

        Collection<RefactoringElement> elements = session.getRefactoringElements();
        if (elements.isEmpty()) {
            return "No usages found for member " + memberName + " in " + filePath;
        }

        StringBuilder sb = new StringBuilder("Found ").append(elements.size()).append(" usages of ").append(memberName).append(":\n");
        for (RefactoringElement element : elements) {
            sb.append("- ").append(element.getDisplayText()).append(" (").append(element.getParentFile().getPath()).append(")\n");
        }
        return sb.toString();
    }

    /**
     * Helper method to execute a refactoring operation through its standard lifecycle.
     * 
     * @param refactoring The refactoring operation to execute.
     * @param sessionName The name for the refactoring session.
     * @return A detailed feedback string.
     */
    private String executeRefactoring(AbstractRefactoring refactoring, String sessionName) {
        StringBuilder feedback = new StringBuilder();
        
        feedback.append("Executing: ").append(sessionName).append("\n");
        
        feedback.append("1. Pre-check... ");
        Problem p = refactoring.preCheck();
        if (p != null && p.isFatal()) {
            return feedback.append("FAILED: ").append(p.getMessage()).toString();
        }
        feedback.append("OK\n");

        feedback.append("2. Checking parameters... ");
        p = refactoring.checkParameters();
        if (p != null && p.isFatal()) {
            return feedback.append("FAILED: ").append(p.getMessage()).toString();
        }
        feedback.append("OK\n");

        RefactoringSession session = RefactoringSession.create(sessionName);
        feedback.append("3. Preparing session... ");
        p = refactoring.prepare(session);
        if (p != null && p.isFatal()) {
            return feedback.append("FAILED: ").append(p.getMessage()).toString();
        }
        
        Collection<RefactoringElement> elements = session.getRefactoringElements();
        if (elements.isEmpty()) {
            return feedback.append("FAILED: No refactoring elements found. The operation might not be applicable or the target is invalid.").toString();
        }
        feedback.append("OK (").append(elements.size()).append(" elements)\n");
        for (RefactoringElement re : elements) {
            feedback.append("   - ").append(re.getDisplayText()).append("\n");
        }

        feedback.append("4. Performing refactoring... ");
        p = session.doRefactoring(true);
        if (p != null && p.isFatal()) {
            return feedback.append("FAILED: ").append(p.getMessage()).toString();
        }
        feedback.append("SUCCESS\n");

        return feedback.toString();
    }

    /**
     * Helper method to retrieve a FileObject from an absolute path.
     * 
     * @param filePath The absolute path.
     * @return The FileObject.
     * @throws IllegalArgumentException if the file does not exist or cannot be resolved.
     */
    private FileObject getFileObject(String filePath) {
        File f = new File(filePath);
        if (!f.exists()) {
            throw new IllegalArgumentException("File does not exist: " + filePath);
        }
        FileObject fo = FileUtil.toFileObject(f);
        if (fo == null) {
            throw new IllegalArgumentException("Could not resolve FileObject for: " + filePath);
        }
        return fo;
    }

    /**
     * Helper method to create a Lookup for a file, including its primary Java element if applicable.
     * 
     * @param fo The FileObject.
     * @return A Lookup containing the FileObject and potentially an ElementHandle.
     */
    private Lookup getLookupForFile(FileObject fo) {
        if ("java".equals(fo.getExt())) {
            JavaSource js = JavaSource.forFileObject(fo);
            if (js != null) {
                final ElementHandle[] handle = new ElementHandle[1];
                try {
                    js.runUserActionTask(new Task<CompilationController>() {
                        @Override
                        public void run(CompilationController parameter) throws Exception {
                            parameter.toPhase(JavaSource.Phase.ELEMENTS_RESOLVED);
                            List<? extends TypeElement> topLevelElements = parameter.getTopLevelElements();
                            if (!topLevelElements.isEmpty()) {
                                handle[0] = ElementHandle.create(topLevelElements.get(0));
                            }
                        }
                    }, true);
                } catch (IOException ex) {
                    // Fallback to file object only
                }
                if (handle[0] != null) {
                    return Lookups.fixed(fo, handle[0]);
                }
            }
        }
        return Lookups.singleton(fo);
    }

    /**
     * Helper method to get an ElementHandle for a specific member in a Java file.
     * 
     * @param fo The FileObject.
     * @param memberName The member name.
     * @return The ElementHandle, or null if not found.
     * @throws IOException if an I/O error occurs.
     */
    private ElementHandle getElementHandleForMember(FileObject fo, String memberName) throws IOException {
        JavaSource js = JavaSource.forFileObject(fo);
        if (js == null) return null;

        final ElementHandle[] handle = new ElementHandle[1];
        js.runUserActionTask(new Task<CompilationController>() {
            @Override
            public void run(CompilationController parameter) throws Exception {
                parameter.toPhase(JavaSource.Phase.ELEMENTS_RESOLVED);
                TypeElement te = parameter.getTopLevelElements().isEmpty() ? null : parameter.getTopLevelElements().get(0);
                if (te != null) {
                    for (Element e : te.getEnclosedElements()) {
                        if (e.getSimpleName().contentEquals(memberName)) {
                            handle[0] = ElementHandle.create(e);
                            break;
                        }
                    }
                }
            }
        }, true);
        return handle[0];
    }

    /**
     * Helper method to get a TreePathHandle for a specific member in a Java file.
     * 
     * @param fo The FileObject.
     * @param memberName The member name.
     * @return The TreePathHandle, or null if not found.
     * @throws IOException if an I/O error occurs.
     */
    private TreePathHandle getTreePathHandleForMember(FileObject fo, String memberName) throws IOException {
        JavaSource js = JavaSource.forFileObject(fo);
        if (js == null) return null;

        final TreePathHandle[] handle = new TreePathHandle[1];
        js.runUserActionTask(new Task<CompilationController>() {
            @Override
            public void run(CompilationController parameter) throws Exception {
                parameter.toPhase(JavaSource.Phase.ELEMENTS_RESOLVED);
                TypeElement te = parameter.getTopLevelElements().isEmpty() ? null : parameter.getTopLevelElements().get(0);
                if (te != null) {
                    for (Element e : te.getEnclosedElements()) {
                        if (e.getSimpleName().contentEquals(memberName)) {
                            handle[0] = TreePathHandle.create(e, parameter);
                            break;
                        }
                    }
                }
            }
        }, true);
        return handle[0];
    }

    /**
     * Helper method to get a TreePathHandle for the primary class in a Java file.
     * 
     * @param fo The FileObject.
     * @return The TreePathHandle, or null if not found.
     * @throws IOException if an I/O error occurs.
     */
    private TreePathHandle getTreePathHandleForClass(FileObject fo) throws IOException {
        JavaSource js = JavaSource.forFileObject(fo);
        if (js == null) return null;

        final TreePathHandle[] handle = new TreePathHandle[1];
        js.runUserActionTask(new Task<CompilationController>() {
            @Override
            public void run(CompilationController parameter) throws Exception {
                parameter.toPhase(JavaSource.Phase.ELEMENTS_RESOLVED);
                TypeElement te = parameter.getTopLevelElements().isEmpty() ? null : parameter.getTopLevelElements().get(0);
                if (te != null) {
                    handle[0] = TreePathHandle.create(te, parameter);
                }
            }
        }, true);
        return handle[0];
    }

    /**
     * Resolves member names to ElementHandles.
     */
    private void resolveMembers(FileObject fo, List<String> memberNames, List<ElementHandle<ExecutableElement>> methods, List<ElementHandle<VariableElement>> fields) throws IOException {
        JavaSource js = JavaSource.forFileObject(fo);
        if (js == null) return;

        js.runUserActionTask(new Task<CompilationController>() {
            @Override
            public void run(CompilationController parameter) throws Exception {
                parameter.toPhase(JavaSource.Phase.ELEMENTS_RESOLVED);
                TypeElement te = parameter.getTopLevelElements().isEmpty() ? null : parameter.getTopLevelElements().get(0);
                if (te != null) {
                    for (Element e : te.getEnclosedElements()) {
                        if (memberNames.contains(e.getSimpleName().toString())) {
                            if (e instanceof ExecutableElement ee) {
                                methods.add(ElementHandle.create(ee));
                            } else if (e instanceof VariableElement ve) {
                                fields.add(ElementHandle.create(ve));
                            }
                        }
                    }
                }
            }
        }, true);
    }

    /**
     * Resolves member names to MemberInfo objects.
     */
    private void resolveMemberInfos(FileObject fo, List<String> memberNames, List<MemberInfo> members) throws IOException {
        JavaSource js = JavaSource.forFileObject(fo);
        if (js == null) return;

        js.runUserActionTask(new Task<CompilationController>() {
            @Override
            public void run(CompilationController parameter) throws Exception {
                parameter.toPhase(JavaSource.Phase.ELEMENTS_RESOLVED);
                TypeElement te = parameter.getTopLevelElements().isEmpty() ? null : parameter.getTopLevelElements().get(0);
                if (te != null) {
                    for (Element e : te.getEnclosedElements()) {
                        if (memberNames.contains(e.getSimpleName().toString())) {
                            members.add(MemberInfo.create(e, parameter));
                        }
                    }
                }
            }
        }, true);
    }
}
