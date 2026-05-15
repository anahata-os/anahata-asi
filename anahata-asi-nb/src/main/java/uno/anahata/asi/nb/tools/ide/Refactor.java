/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.tools.ide;

import com.sun.source.tree.Tree;
import com.sun.source.util.TreePath;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import javax.lang.model.element.Element;
import javax.lang.model.element.ExecutableElement;
import javax.lang.model.element.TypeElement;
import javax.lang.model.element.VariableElement;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
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
import org.netbeans.modules.refactoring.java.api.ChangeParametersRefactoring;
import org.netbeans.modules.refactoring.java.api.EncapsulateFieldRefactoring;
import org.netbeans.modules.refactoring.java.api.ExtractInterfaceRefactoring;
import org.netbeans.modules.refactoring.java.api.ExtractSuperclassRefactoring;
import org.netbeans.modules.refactoring.java.api.InlineRefactoring;
import org.netbeans.modules.refactoring.java.api.InnerToOuterRefactoring;
import org.netbeans.modules.refactoring.java.api.InvertBooleanRefactoring;
import org.netbeans.modules.refactoring.java.api.MemberInfo;
import org.netbeans.modules.refactoring.java.api.PullUpRefactoring;
import org.netbeans.modules.refactoring.java.api.PushDownRefactoring;
import org.netbeans.modules.refactoring.java.api.UseSuperTypeRefactoring;
import org.openide.filesystems.FileObject;
import org.openide.util.Lookup;
import org.openide.util.lookup.Lookups;
import uno.anahata.asi.agi.resource.Resource;
import uno.anahata.asi.agi.tool.AnahataToolkit;
import uno.anahata.asi.agi.tool.AgiToolkit;
import uno.anahata.asi.agi.tool.AgiToolParam;
import uno.anahata.asi.agi.tool.AgiTool;
import uno.anahata.asi.agi.tool.AgiToolException;
import uno.anahata.asi.nb.tools.java.JavaSourceUtils;
import static uno.anahata.asi.nb.tools.java.JavaSourceUtils.findMemberCandidates;
import static uno.anahata.asi.nb.tools.java.JavaSourceUtils.findTree;

/**
 * A toolkit for performing programmatic refactoring operations within the
 * NetBeans IDE.
 * <p>
 * This toolkit leverages the NetBeans Refactoring API to ensure that changes
 * are propagated correctly across the entire project (e.g., updating imports,
 * references, and string constants). It supports a wide range of standard Java
 * refactorings such as rename, move, copy, inline, and interface extraction.
 * </p>
 *
 * @author anahata
 */
@Slf4j
@AgiToolkit("Programmatic refactoring tools for NetBeans.")
public class Refactor extends AnahataToolkit {

    @Override
    public List<String> getSystemInstructions() throws Exception {
        return Collections.singletonList(JavaSourceUtils.CANONICAL_FQN_STANDARD
                + "\n"
                + "Refactor Toolkit Instructions:\n"
                + "- Use these tools to perform safe, project-wide refactorings.\n"
                + "- All member-based tools require the Anahata Canonical FQN.\n"
                + "- **Renaming Java Members**: If you need to rename a Java method or field that is used across multiple files, DO NOT use text replacement tools (like `Resources.findAndReplaceInTextResource`). Instead, use the `Refactor.renameMember` tool to prevent cascading compilation errors.\n"
        );
    }

    /**
     * DTO for specifying changes to method parameters.
     */
    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    public static class ParameterChange {

        /**
         * The 0-based index of the parameter in the original method. Use -1 for
         * new parameters.
         */
        private int originalIndex;
        /**
         * The new name for the parameter.
         */
        private String name;
        /**
         * The new type for the parameter (FQN or simple name).
         */
        private String type;
        /**
         * The default value to use at call sites for new parameters.
         */
        private String defaultValue;
    }

    /**
     * Performs a programmatic rename refactoring of a file or class within the
     * IDE.
     *
     * @param filePath The absolute path of the file to rename.
     * @param newName The new name for the file or class (without the
     * extension).
     * @return A detailed log of the refactoring process.
     * @throws Exception if there is an error invoking the operation.
     */
    @AgiTool("Renames a file or class. This is a 'safe' rename that updates all references in all open projects and the most efficient way of renaming a type as it updates the file's content, all its references and the file name on the file system in a single shot.")
    public String rename(
            @AgiToolParam(value = "The absolute path of the file to rename.", rendererId = "path") String filePath,
            @AgiToolParam("The new name (without extension).") String newName) throws Exception {
        FileObject fo = JavaSourceUtils.getFileObject(filePath);
        RenameRefactoring refactoring = new RenameRefactoring(getLookupForFile(fo));
        refactoring.setNewName(newName);

        String result = executeRefactoring(refactoring, "Rename " + fo.getName());
        return enrichWithContextInfo(filePath, result, "renamed");
    }

    /**
     * Renames a class member (method or field) across all open projects.
     *
     * @param filePath The absolute path of the Java file.
     * @param memberName The current name of the member.
     * @param newName The new name for the member.
     * @return A detailed log of the refactoring process.
     * @throws Exception if there is an error invoking the operation.
     */
    @AgiTool("Renames a class member (method or field) across all open projects. You must always Use this tool for renaming methods that you think could be called from outside the java file to save the user the cascade of compilation errors that could be derived from renaming a method using the Resources toolkit or any other text based rename.")
    public String renameMember(
            @AgiToolParam(value = "The absolute path of the Java file.", rendererId = "path") String filePath,
            @AgiToolParam("The ABSOLUTE FQN of the member (e.g. 'com.foo.Bar.myMethod(int)').") String memberFqn,
            @AgiToolParam("The new name for the member (simple name only).") String newName) throws Exception {
        FileObject fo = JavaSourceUtils.getFileObject(filePath);
        TreePathHandle handle = resolveMember(fo, memberFqn);

        RenameRefactoring refactoring = new RenameRefactoring(Lookups.fixed(handle));
        refactoring.setNewName(newName);

        return executeRefactoring(refactoring, "Rename Member " + memberFqn + " to " + newName);
    }

    /**
     * Moves a Java class to a new package or integrates it into another class.
     * <p>
     * For package moves, use a folder path as target. For member integration,
     * use the path of the target .java file.
     * </p>
     *
     * @param sourcePath The absolute path of the .java file to move.
     * @param targetPath The absolute path of the destination folder or .java
     * file.
     * @return A detailed log of the refactoring process.
     * @throws Exception if the operation fails.
     */
    @AgiTool("Moves a Java class to a new package or integrates it into another class. For inner classes, use moveInnerToTopLevel first.")
    public String moveClass(
            @AgiToolParam(value = "The absolute path of the .java file to move.", rendererId = "path") String sourcePath,
            @AgiToolParam(value = "The absolute path of the target folder or .java file.", rendererId = "path") String targetPath) throws Exception {
        FileObject sourceFo = JavaSourceUtils.getFileObject(sourcePath);
        if (!"java".equals(sourceFo.getExt())) {
            throw new IllegalArgumentException("Source must be a .java file. Use moveFile for other types.");
        }
        FileObject targetFo = JavaSourceUtils.getFileObject(targetPath);

        MoveRefactoring refactoring;
        if (targetFo.isData() && "java".equals(targetFo.getExt())) {
            TreePathHandle sourceHandle = getTreePathHandleForClass(sourceFo);
            TreePathHandle targetHandle = getTreePathHandleForClass(targetFo);
            refactoring = new MoveRefactoring(Lookups.fixed(sourceHandle));
            refactoring.setTarget(Lookups.fixed(targetHandle));
        } else if (targetFo.isFolder()) {
            refactoring = new MoveRefactoring(Lookups.fixed(sourceFo));
            java.net.URL targetUrl = org.openide.filesystems.URLMapper.findURL(targetFo, org.openide.filesystems.URLMapper.EXTERNAL);
            if (targetUrl == null) {
                targetUrl = targetFo.toURL();
            }
            refactoring.setTarget(Lookups.singleton(targetUrl));
        } else {
            throw new IllegalArgumentException("Target must be a folder or a .java file: " + targetPath);
        }

        String result = executeRefactoring(refactoring, "Move Class " + sourceFo.getName());
        return enrichWithContextInfo(sourcePath, result, "moved");
    }

    /**
     * Moves a non-Java file to a different folder.
     *
     * @param sourcePath The absolute path of the file to move.
     * @param targetFolderPath The absolute path of the target folder.
     * @return A detailed log of the refactoring process.
     * @throws Exception if the operation fails.
     */
    @AgiTool("Moves a non-Java file to a different folder.")
    public String moveFile(
            @AgiToolParam(value = "The absolute path of the file to move.", rendererId = "path") String sourcePath,
            @AgiToolParam(value = "The absolute path of the target folder.", rendererId = "path") String targetFolderPath) throws Exception {
        FileObject sourceFo = JavaSourceUtils.getFileObject(sourcePath);
        FileObject targetFo = JavaSourceUtils.getFileObject(targetFolderPath);

        if (!targetFo.isFolder()) {
            throw new IllegalArgumentException("Target path must be a folder: " + targetFolderPath);
        }

        MoveRefactoring refactoring = new MoveRefactoring(Lookups.fixed(sourceFo));
        java.net.URL targetUrl = org.openide.filesystems.URLMapper.findURL(targetFo, org.openide.filesystems.URLMapper.EXTERNAL);
        if (targetUrl == null) {
            targetUrl = targetFo.toURL();
        }
        refactoring.setTarget(Lookups.singleton(targetUrl));

        String result = executeRefactoring(refactoring, "Move File " + sourceFo.getName());
        return enrichWithContextInfo(sourcePath, result, "moved");
    }

    /**
     * Copies a Java class to a new package or folder.
     *
     * @param sourcePath The absolute path of the .java file to copy.
     * @param targetFolderPath The absolute path of the destination folder.
     * @return A detailed log of the refactoring process.
     * @throws Exception if the operation fails.
     */
    @AgiTool("Copies a Java class to a new package or folder.")
    public String copyClass(
            @AgiToolParam(value = "The absolute path of the .java file to copy.", rendererId = "path") String sourcePath,
            @AgiToolParam(value = "The absolute path of the target folder.", rendererId = "path") String targetFolderPath) throws Exception {
        FileObject sourceFo = JavaSourceUtils.getFileObject(sourcePath);
        if (!"java".equals(sourceFo.getExt())) {
            throw new IllegalArgumentException("Source must be a .java file. Use copyFile for other types.");
        }
        FileObject targetFo = JavaSourceUtils.getFileObject(targetFolderPath);

        if (!targetFo.isFolder()) {
            throw new IllegalArgumentException("Target path must be a folder: " + targetFolderPath);
        }

        CopyRefactoring refactoring = new CopyRefactoring(Lookups.fixed(sourceFo));
        java.net.URL targetUrl = org.openide.filesystems.URLMapper.findURL(targetFo, org.openide.filesystems.URLMapper.EXTERNAL);
        if (targetUrl == null) {
            targetUrl = targetFo.toURL();
        }
        refactoring.setTarget(Lookups.singleton(targetUrl));

        return executeRefactoring(refactoring, "Copy Class " + sourceFo.getName());
    }

    /**
     * Copies a non-Java file to a different folder.
     *
     * @param sourcePath The absolute path of the file to copy.
     * @param targetFolderPath The absolute path of the target folder.
     * @return A detailed log of the refactoring process.
     * @throws Exception if the operation fails.
     */
    @AgiTool("Copies a non-Java file to a different folder.")
    public String copyFile(
            @AgiToolParam(value = "The absolute path of the file to copy.", rendererId = "path") String sourcePath,
            @AgiToolParam(value = "The absolute path of the target folder.", rendererId = "path") String targetFolderPath) throws Exception {
        FileObject sourceFo = JavaSourceUtils.getFileObject(sourcePath);
        FileObject targetFo = JavaSourceUtils.getFileObject(targetFolderPath);

        if (!targetFo.isFolder()) {
            throw new IllegalArgumentException("Target path must be a folder: " + targetFolderPath);
        }

        CopyRefactoring refactoring = new CopyRefactoring(Lookups.fixed(sourceFo));
        java.net.URL targetUrl = org.openide.filesystems.URLMapper.findURL(targetFo, org.openide.filesystems.URLMapper.EXTERNAL);
        if (targetUrl == null) {
            targetUrl = targetFo.toURL();
        }
        refactoring.setTarget(Lookups.singleton(targetUrl));

        return executeRefactoring(refactoring, "Copy File " + sourceFo.getName());
    }

    /**
     * Performs a programmatic safe delete refactoring of a file or class.
     *
     * @param filePath The absolute path of the file to delete.
     * @param checkInComments Whether to search for usages in comments and
     * strings.
     * @return A detailed log of the refactoring process.
     * @throws Exception if there is an error invoking the operation.
     */
    @AgiTool("Deletes a file or class if it is safe to do so (i.e., no active usages) or returns a beautiful list of references to the file from any other files across all open projects (references that would need to be updated before deleting it)")
    public String safeDelete(
            @AgiToolParam(value = "The absolute path of the file to delete.", rendererId = "path") String filePath,
            @AgiToolParam("Whether to check for usages in comments.") boolean checkInComments) throws Exception {
        FileObject fo = JavaSourceUtils.getFileObject(filePath);
        SafeDeleteRefactoring refactoring = new SafeDeleteRefactoring(getLookupForFile(fo));
        refactoring.setCheckInComments(checkInComments);

        String result = executeRefactoring(refactoring, "Safe Delete " + fo.getName());
        return enrichWithContextInfo(filePath, result, "deleted");
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
    @AgiTool("Inlines a method, constant, or variable, replacing all usages with its body/value.")
    public String inline(
            @AgiToolParam(value = "The absolute path of the Java file.", rendererId = "path") String filePath,
            @AgiToolParam("The ABSOLUTE FQN of the member to inline.") String memberFqn,
            @AgiToolParam("The type of inlining (METHOD, TEMP, CONSTANT).") InlineRefactoring.Type type) throws Exception {
        FileObject fo = JavaSourceUtils.getFileObject(filePath);
        TreePathHandle handle = resolveMember(fo, memberFqn);

        InlineRefactoring refactoring = new InlineRefactoring(handle, type);
        return executeRefactoring(refactoring, "Inline " + memberFqn);
    }

    /**
     * Encapsulates a field by creating a getter and setter and updating all
     * usages.
     *
     * @param filePath The absolute path of the Java file.
     * @param fieldName The name of the field to encapsulate.
     * @param getterName The name for the getter method (optional, will be
     * generated if null).
     * @param setterName The name for the setter method (optional, will be
     * generated if null).
     * @return A detailed log of the refactoring process.
     * @throws Exception if the operation fails.
     */
    @AgiTool("Encapsulates a field by creating a getter and setter and updating all references to use them.")
    public String encapsulateField(
            @AgiToolParam(value = "The absolute path of the Java file.", rendererId = "path") String filePath,
            @AgiToolParam("The ABSOLUTE FQN of the field to encapsulate.") String fieldFqn,
            @AgiToolParam("The name for the getter method.") String getterName,
            @AgiToolParam("The name for the setter method.") String setterName) throws Exception {
        FileObject fo = JavaSourceUtils.getFileObject(filePath);
        TreePathHandle handle = resolveMember(fo, fieldFqn);

        EncapsulateFieldRefactoring refactoring = new EncapsulateFieldRefactoring(handle);

        String fieldName = JavaSourceUtils.getMemberSimpleName(fieldFqn);
        String gName = (getterName != null && !getterName.isEmpty()) ? getterName : "get" + StringUtils.capitalize(fieldName);
        String sName = (setterName != null && !setterName.isEmpty()) ? setterName : "set" + StringUtils.capitalize(fieldName);

        refactoring.setGetterName(gName);
        refactoring.setSetterName(sName);

        return executeRefactoring(refactoring, "Encapsulate Field " + fieldFqn);
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
    @AgiTool("Inverts the logic of a boolean method or variable and updates all call sites accordingly.")
    public String invertBoolean(
            @AgiToolParam(value = "The absolute path of the Java file.", rendererId = "path") String filePath,
            @AgiToolParam("The ABSOLUTE FQN of the boolean member to invert.") String memberFqn,
            @AgiToolParam("The new name for the inverted member.") String newName) throws Exception {
        FileObject fo = JavaSourceUtils.getFileObject(filePath);
        TreePathHandle handle = resolveMember(fo, memberFqn);

        InvertBooleanRefactoring refactoring = new InvertBooleanRefactoring(handle);
        refactoring.setNewName(newName);
        return executeRefactoring(refactoring, "Invert Boolean " + memberFqn);
    }

    /**
     * Extracts an interface from a class.
     *
     * @param filePath The absolute path of the Java file containing the class.
     * @param interfaceName The name of the new interface.
     * @param memberNames The names of the members (methods/fields) to include
     * in the interface.
     * @return A detailed log of the refactoring process.
     * @throws Exception if the operation fails.
     */
    @AgiTool("Extracts an interface from a class, moving selected members to the new interface.")
    public String extractInterface(
            @AgiToolParam(value = "The absolute path of the Java file.", rendererId = "path") String filePath,
            @AgiToolParam("The name of the new interface.") String interfaceName,
            @AgiToolParam("The ABSOLUTE FQNs of the members to extract.") List<String> memberFqns) throws Exception {
        FileObject fo = JavaSourceUtils.getFileObject(filePath);
        TreePathHandle classHandle = getTreePathHandleForClass(fo);
        if (classHandle == null) {
            return "Class not found in " + filePath;
        }

        ExtractInterfaceRefactoring refactoring = new ExtractInterfaceRefactoring(classHandle);
        refactoring.setInterfaceName(interfaceName);

        List<ElementHandle<ExecutableElement>> methods = new ArrayList<>();
        List<ElementHandle<VariableElement>> fields = new ArrayList<>();

        resolveMembers(fo, memberFqns, methods, fields);

        refactoring.setMethods(methods);
        refactoring.setFields(fields);

        return executeRefactoring(refactoring, "Extract Interface " + interfaceName);
    }

    /**
     * Pulls up members to a superclass.
     *
     * @param filePath The absolute path of the Java file containing the
     * subclass.
     * @param targetClassFqn The fully qualified name of the target superclass.
     * @param memberNames The names of the members to pull up.
     * @return A detailed log of the refactoring process.
     * @throws Exception if the operation fails.
     */
    @AgiTool("Pulls up selected members from a class to one of its superclasses.")
    public String pullUp(
            @AgiToolParam(value = "The absolute path of the Java file containing the subclass.", rendererId = "path") String filePath,
            @AgiToolParam("The FQN of the target superclass.") String targetClassFqn,
            @AgiToolParam("The ABSOLUTE FQNs of the members to pull up.") List<String> memberFqns) throws Exception {
        FileObject fo = JavaSourceUtils.getFileObject(filePath);
        TreePathHandle classHandle = getTreePathHandleForClass(fo);
        if (classHandle == null) {
            return "Class not found in " + filePath;
        }

        PullUpRefactoring refactoring = new PullUpRefactoring(classHandle);

        // Resolve target type
        ElementHandle<TypeElement> targetHandle = ElementHandle.createTypeElementHandle(javax.lang.model.element.ElementKind.CLASS, targetClassFqn);
        refactoring.setTargetType(targetHandle);

        // Resolve members to MemberInfo
        List<MemberInfo<ElementHandle<? extends Element>>> members = new ArrayList<>();
        resolveMemberInfos(fo, memberFqns, members);

        refactoring.setMembers(members.toArray(new MemberInfo[0]));

        return executeRefactoring(refactoring, "Pull Up to " + targetClassFqn);
    }

    /**
     * Pushes down members to subclasses.
     *
     * @param filePath The absolute path of the Java file containing the
     * superclass.
     * @param memberNames The names of the members to push down.
     * @return A detailed log of the refactoring process.
     * @throws Exception if the operation fails.
     */
    @AgiTool("Pushes down selected members from a class to its subclasses.")
    public String pushDown(
            @AgiToolParam(value = "The absolute path of the Java file containing the superclass.", rendererId = "path") String filePath,
            @AgiToolParam("The ABSOLUTE FQNs of the members to push down.") List<String> memberFqns) throws Exception {
        FileObject fo = JavaSourceUtils.getFileObject(filePath);
        TreePathHandle classHandle = getTreePathHandleForClass(fo);
        if (classHandle == null) {
            return "Class not found in " + filePath;
        }

        PushDownRefactoring refactoring = new PushDownRefactoring(classHandle);

        // Resolve members to MemberInfo
        List<MemberInfo<ElementHandle<? extends Element>>> members = new ArrayList<>();
        resolveMemberInfos(fo, memberFqns, members);

        refactoring.setMembers(members.toArray(new MemberInfo[0]));

        return executeRefactoring(refactoring, "Push Down from " + fo.getName());
    }

    /**
     * Changes the signature of a method, including its name, return type, and
     * parameters.
     *
     * @param filePath The absolute path of the Java file.
     * @param methodName The current name of the method.
     * @param newName The new name for the method.
     * @param newReturnType The new return type (FQN or simple name).
     * @param parameterChanges The new parameter configuration.
     * @return A detailed log of the refactoring process.
     * @throws Exception if the operation fails.
     */
    @AgiTool("Changes a method's signature (name, return type, parameters) and updates all call sites in all open projects.")
    public String changeMethodSignature(
            @AgiToolParam(value = "The absolute path of the Java file.", rendererId = "path") String filePath,
            @AgiToolParam("The ABSOLUTE FQN of the method (e.g. 'com.foo.Bar.myMethod(int)').") String memberFqn,
            @AgiToolParam("The new name for the method.") String newName,
            @AgiToolParam("The new return type.") String newReturnType,
            @AgiToolParam("The new parameter list.") List<ParameterChange> parameterChanges) throws Exception {
        FileObject fo = JavaSourceUtils.getFileObject(filePath);
        TreePathHandle handle = resolveMember(fo, memberFqn);

        ChangeParametersRefactoring refactoring = new ChangeParametersRefactoring(handle);
        refactoring.setMethodName(newName);
        refactoring.setReturnType(newReturnType);

        ChangeParametersRefactoring.ParameterInfo[] infos = new ChangeParametersRefactoring.ParameterInfo[parameterChanges.size()];
        for (int i = 0; i < parameterChanges.size(); i++) {
            ParameterChange pc = parameterChanges.get(i);
            infos[i] = new ChangeParametersRefactoring.ParameterInfo(pc.getOriginalIndex(), pc.getName(), pc.getType(), pc.getDefaultValue());
        }
        refactoring.setParameterInfo(infos);

        return executeRefactoring(refactoring, "Change Method Signature: " + memberFqn);
    }

    /**
     * Extracts a superclass from an existing class.
     *
     * @param filePath The absolute path of the Java file.
     * @param superclassName The name for the new superclass.
     * @param memberNames The names of the members to move to the superclass.
     * @return A detailed log of the refactoring process.
     * @throws Exception if the operation fails.
     */
    @AgiTool("Extracts a new superclass from a class, moving selected members to it.")
    public String extractSuperclass(
            @AgiToolParam(value = "The absolute path of the Java file.", rendererId = "path") String filePath,
            @AgiToolParam("The name for the new superclass.") String superclassName,
            @AgiToolParam("The ABSOLUTE FQNs of the members to extract.") List<String> memberFqns) throws Exception {
        FileObject fo = JavaSourceUtils.getFileObject(filePath);
        TreePathHandle classHandle = getTreePathHandleForClass(fo);
        if (classHandle == null) {
            return "Class not found in " + filePath;
        }

        ExtractSuperclassRefactoring refactoring = new ExtractSuperclassRefactoring(classHandle);
        refactoring.setSuperClassName(superclassName);

        List<MemberInfo<ElementHandle<? extends Element>>> members = new ArrayList<>();
        resolveMemberInfos(fo, memberFqns, members);
        refactoring.setMembers(members.toArray(new MemberInfo[0]));

        return executeRefactoring(refactoring, "Extract Superclass " + superclassName);
    }

    /**
     * Replaces usages of a class with a supertype where possible.
     *
     * @param filePath The absolute path of the Java file.
     * @param supertypeFqn The FQN of the supertype to use.
     * @return A detailed log of the refactoring process.
     * @throws Exception if the operation fails.
     */
    @AgiTool("Replaces usages of a class with a supertype (interface or superclass) throughout all open projects where possible.")
    public String useSupertype(
            @AgiToolParam(value = "The absolute path of the Java file.", rendererId = "path") String filePath,
            @AgiToolParam("The FQN of the supertype to use.") String supertypeFqn) throws Exception {
        FileObject fo = JavaSourceUtils.getFileObject(filePath);
        TreePathHandle classHandle = getTreePathHandleForClass(fo);
        if (classHandle == null) {
            return "Class not found in " + filePath;
        }

        UseSuperTypeRefactoring refactoring = new UseSuperTypeRefactoring(classHandle);
        ElementHandle<TypeElement> superHandle = ElementHandle.createTypeElementHandle(javax.lang.model.element.ElementKind.CLASS, supertypeFqn);
        refactoring.setTargetSuperType(superHandle);

        return executeRefactoring(refactoring, "Use Supertype " + supertypeFqn);
    }

    /**
     * Moves a nested/inner class to the top level.
     *
     * @param filePath The absolute path of the Java file containing the inner
     * class.
     * @param innerClassName The name of the inner class to move.
     * @return A detailed log of the refactoring process.
     * @throws Exception if the operation fails.
     */
    @AgiTool("Moves a nested (inner) class to its own top-level file, updating all references.")
    public String moveInnerToTopLevel(
            @AgiToolParam(value = "The absolute path of the Java file containing the inner class.", rendererId = "path") String filePath,
            @AgiToolParam("The ABSOLUTE FQN of the inner class to move.") String memberFqn) throws Exception {
        FileObject fo = JavaSourceUtils.getFileObject(filePath);
        TreePathHandle handle = resolveMember(fo, memberFqn);

        String innerClassName = JavaSourceUtils.getMemberSimpleName(memberFqn);

        InnerToOuterRefactoring refactoring = new InnerToOuterRefactoring(handle);
        refactoring.setClassName(innerClassName);
        return executeRefactoring(refactoring, "Move Inner to Top Level: " + memberFqn);
    }

    /**
     * Finds all usages of a file or class within all open projects.
     *
     * @param filePath The absolute path of the file to search for.
     * @param searchInComments Whether to search for usages in comments and
     * strings.
     * @return A formatted list of all found usages.
     * @throws Exception if there is an error invoking the query.
     */
    @AgiTool("Finds all references/usages of a file or type in all open projects.")
    public String whereUsed(
            @AgiToolParam(value = "The absolute path of the file to search for.", rendererId = "path") String filePath,
            @AgiToolParam("Whether to search in comments.") boolean searchInComments) throws Exception {
        FileObject fo = JavaSourceUtils.getFileObject(filePath);
        Lookup lookup = getLookupForFile(fo);
        // CRITICAL: For WhereUsedQuery on Java files, we must use ONLY the TreePathHandle
        // to ensure NetBeans performs a symbolic search instead of a generic file search.
        TreePathHandle handle = lookup.lookup(TreePathHandle.class);
        if (handle != null) {
            lookup = Lookups.fixed(handle);
        }

        WhereUsedQuery query = new WhereUsedQuery(lookup);
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
     * Finds all usages of a specific member (field or method) within a Java
     * file.
     *
     * @param filePath The absolute path of the Java file.
     * @param memberName The name of the member to search for.
     * @param searchInComments Whether to search for usages in comments and
     * strings.
     * @return A formatted list of all found usages.
     * @throws Exception if there is an error invoking the query.
     */
    @AgiTool("Finds all references/usages of a specific class member (method or field) in all open projects.")
    public String whereUsedMember(
            @AgiToolParam(value = "The absolute path of the Java file.", rendererId = "path") String filePath,
            @AgiToolParam("The ABSOLUTE FQN of the member (e.g. 'com.foo.Bar.myMethod(int)').") String memberFqn,
            @AgiToolParam("Whether to search in comments.") boolean searchInComments) throws Exception {
        FileObject fo = JavaSourceUtils.getFileObject(filePath);
        if (!"java".equals(fo.getExt())) {
            throw new IllegalArgumentException("Member search is only supported for Java files.");
        }

        TreePathHandle handle = resolveMember(fo, memberFqn);

        // CRITICAL: For Java elements, the query lookup MUST contain a TreePathHandle
        WhereUsedQuery query = new WhereUsedQuery(Lookups.fixed(handle));
        query.putValue(WhereUsedQuery.FIND_REFERENCES, true);
        query.putValue(WhereUsedQuery.SEARCH_IN_COMMENTS, searchInComments);

        RefactoringSession session = RefactoringSession.create("Where Used Member: " + memberFqn);
        Problem p = query.prepare(session);
        if (p != null && p.isFatal()) {
            return "Fatal error during query preparation: " + p.getMessage();
        }

        Collection<RefactoringElement> elements = session.getRefactoringElements();
        if (elements.isEmpty()) {
            return "No usages found for member " + memberFqn + " in " + filePath;
        }

        StringBuilder sb = new StringBuilder("Found ").append(elements.size()).append(" usages of ").append(memberFqn).append(":\n");
        for (RefactoringElement element : elements) {
            sb.append("- ").append(element.getDisplayText()).append(" (").append(element.getParentFile().getPath()).append(")\n");
        }
        return sb.toString();
    }

    /**
     * Helper method to execute a refactoring operation through its standard
     * lifecycle.
     * <p>
     * Performs pre-checks, parameter validation, session preparation, and
     * finally executes the refactoring while collecting elements and status
     * reports.
     * </p>
     *
     * @param refactoring The refactoring operation to execute.
     * @param sessionName The name for the refactoring session.
     * @return A detailed feedback string.
     */
    private String executeRefactoring(AbstractRefactoring refactoring, String sessionName) {
        log.info("Executing: {}", sessionName);

        // ATOMICITY FIX: For Java refactorings, we must provide ClasspathInfo in the context
        // to prevent the generic file plugin from 'stealing' the physical move/rename.
        Lookup sourceLookup = refactoring.getRefactoringSource();
        FileObject fo = sourceLookup.lookup(FileObject.class);
        if (fo != null && "java".equals(fo.getExt())) {
            refactoring.getContext().add(org.netbeans.modules.refactoring.java.api.JavaRefactoringUtils.getClasspathInfoFor(new FileObject[]{fo}));
        } else {
            TreePathHandle tph = sourceLookup.lookup(TreePathHandle.class);
            if (tph != null && tph.getFileObject() != null) {
                refactoring.getContext().add(org.netbeans.modules.refactoring.java.api.JavaRefactoringUtils.getClasspathInfoFor(new FileObject[]{tph.getFileObject()}));
            }
        }

        StringBuilder feedback = new StringBuilder();

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
     * Enriches the tool output with information about managed resources
     * affected by the refactoring.
     * <p>
     * It provides specific instructions on how to purge or recover deleted
     * resource content based on whether the action was a deletion or a move.
     * </p>
     *
     * @param path The path of the resource.
     * @param result The raw refactoring result.
     * @param action The type of refactoring action performed (e.g., 'deleted',
     * 'moved').
     * @return An enriched result string.
     */
    private String enrichWithContextInfo(String path, String result, String action) {
        Optional<Resource> res = getResourceManager().findByPath(path);
        if (res.isPresent()) {
            String uuid = res.get().getId();
            StringBuilder sb = new StringBuilder(result);
            sb.append("\n--- Context Awareness ---\n");
            if ("deleted".equals(action)) {
                sb.append("Resource [").append(uuid).append("] remains in context. Its last known content is cached. ")
                        .append("Use Resources.unloadResources to remove it from the RAG Message or ")
                        .append("Session.setContextProviderProviding(false, List.of(\"").append(uuid).append("\")) to free the cached content from the context window ")
                        .append("and Session.setContextProviderProviding(true, List.of(\"").append(uuid).append("\")) if you need to recover its contents.");
            } else {
                sb.append("NOTICE: Managed resource (ID: ").append(uuid).append(") has been ").append(action).append(". ")
                        .append("The internal resource path and name will update automatically to match the new location.");
            }
            return sb.toString();
        }
        return result;
    }

    /**
     * Helper method to create a Lookup for a file, prioritizing high-precision
     * Java elements.
     * <p>
     * CRITICAL FIX: For Java files, this method now returns ONLY the
     * {@link TreePathHandle}. Including the {@link FileObject} alongside the
     * handle causes a conflict between the generic file-move plugin and the
     * specialized Java plugin, leading to atomicity failures and 'stale
     * FileObject' crashes during the commit phase.
     * </p>
     *
     * @param fo The FileObject.
     * @return A Lookup containing the optimal identifiers for the refactoring
     * engine.
     */
    private Lookup getLookupForFile(FileObject fo) {
        if ("java".equals(fo.getExt())) {
            try {
                TreePathHandle handle = getTreePathHandleForClass(fo);
                if (handle != null) {
                    return Lookups.fixed(fo, handle);
                }
            } catch (IOException e) {
                log.error("Failed to resolve TreePathHandle for: " + fo.getPath(), e);
            }
        }
        return Lookups.singleton(fo);
    }

    /**
     * Resolves a set of member names into {@link ElementHandle}s.
     *
     * @param fo The FileObject.
     * @param memberNames The names to resolve.
     * @param methods Output list for resolved methods.
     * @param fields Output list for resolved fields.
     * @throws IOException If the source cannot be parsed.
     */
    private static void resolveMembers(FileObject fo, List<String> memberFqns, List<ElementHandle<ExecutableElement>> methods, List<ElementHandle<VariableElement>> fields) throws IOException {
        JavaSource js = JavaSource.forFileObject(fo);
        if (js == null) {
            return;
        }
        js.runUserActionTask(new Task<>() {
            @Override
            public void run(CompilationController parameter) throws Exception {
                parameter.toPhase(JavaSource.Phase.RESOLVED);
                for (String fqn : memberFqns) {
                    Tree tree = findTree(parameter, fqn);
                    if (tree != null) {
                        Element e = parameter.getTrees().getElement(TreePath.getPath(parameter.getCompilationUnit(), tree));
                        if (e instanceof ExecutableElement ee) {
                            methods.add(ElementHandle.create(ee));
                        } else if (e instanceof VariableElement ve) {
                            fields.add(ElementHandle.create(ve));
                        }
                    }
                }
            }
        }, true);
    }

    /**
     * Resolves member names into {@link MemberInfo} objects for refactoring.
     *
     * @param fo The FileObject.
     * @param memberFqns the fqns of the members to resolve
     * @param members Output list for MemberInfo objects.
     * @throws IOException If the source cannot be parsed.
     */
    private static void resolveMemberInfos(FileObject fo, List<String> memberFqns, List<MemberInfo<ElementHandle<? extends Element>>> members) throws IOException {
        JavaSource js = JavaSource.forFileObject(fo);
        if (js == null) {
            return;
        }

        js.runUserActionTask(new Task<>() {
            @Override
            public void run(CompilationController parameter) throws Exception {
                parameter.toPhase(JavaSource.Phase.RESOLVED);
                for (String fqn : memberFqns) {
                    Tree tree = findTree(parameter, fqn);
                    if (tree != null) {
                        Element e = parameter.getTrees().getElement(TreePath.getPath(parameter.getCompilationUnit(), tree));
                        if (e != null) {
                            members.add((MemberInfo) MemberInfo.create(e, parameter));
                        }
                    }
                }
            }
        }, true);
    }

    /**
     * Authoritatively resolves a member FQN to a TreePathHandle, throwing a
     * detailed exception with candidates if not found.
     *
     * @param fo The file object.
     * @param memberFqn The member FQN.
     * @return The handle.
     * @throws Exception if not found or parsing fails.
     */
    private static TreePathHandle resolveMember(FileObject fo, String memberFqn) throws Exception {
        JavaSource js = JavaSource.forFileObject(fo);
        if (js == null) {
            throw new AgiToolException("Could not get JavaSource for: " + fo.getPath());
        }
        final TreePathHandle[] result = new TreePathHandle[1];
        final String[] candidatesMsg = new String[1];

        js.runUserActionTask(info -> {
            info.toPhase(JavaSource.Phase.RESOLVED);
            Tree tree = findTree(info, memberFqn);
            if (tree != null) {
                result[0] = TreePathHandle.create(TreePath.getPath(info.getCompilationUnit(), tree), info);
            } else {
                List<String> candidates = findMemberCandidates(info, memberFqn);
                if (!candidates.isEmpty()) {
                    StringBuilder sb = new StringBuilder("Member not found: ").append(memberFqn);
                    sb.append("\nDid you mean one of these canonical identification FQNs?\n");
                    for (String c : candidates) {
                        sb.append("- ").append(c).append("\n");
                    }
                    candidatesMsg[0] = sb.toString();
                }
            }
        }, true);

        if (result[0] != null) {
            return result[0];
        }

        if (candidatesMsg[0] != null) {
            throw new AgiToolException(candidatesMsg[0]);
        }

        throw new AgiToolException("Member not found: " + memberFqn);
    }

    /**
     * Creates a {@link TreePathHandle} for the primary top-level class in a
     * file.
     *
     * @param fo The FileObject.
     * @return A TreePathHandle for the class or null.
     * @throws IOException If the source cannot be parsed.
     */
    public static TreePathHandle getTreePathHandleForClass(FileObject fo) throws IOException {
        JavaSource js = JavaSource.forFileObject(fo);
        if (js == null) {
            return null;
        }

        final TreePathHandle[] handle = new TreePathHandle[1];
        js.runUserActionTask(new Task<>() {
            @Override
            public void run(CompilationController parameter) throws Exception {
                parameter.toPhase(JavaSource.Phase.ELEMENTS_RESOLVED);
                for (TypeElement te : parameter.getTopLevelElements()) {
                    if (te.getSimpleName().contentEquals(fo.getName())) {
                        handle[0] = TreePathHandle.create(te, parameter);
                        return;
                    }
                }
                if (!parameter.getTopLevelElements().isEmpty()) {
                    handle[0] = TreePathHandle.create(parameter.getTopLevelElements().get(0), parameter);
                }
            }
        }, true);
        return handle[0];
    }

}
