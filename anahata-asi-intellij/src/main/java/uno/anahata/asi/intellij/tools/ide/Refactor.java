/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.intellij.tools.ide;

import com.intellij.ide.util.DirectoryUtil;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.application.ReadAction;
import com.intellij.openapi.command.WriteCommandAction;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.roots.ProjectFileIndex;
import com.intellij.openapi.vfs.VfsUtilCore;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.JavaPsiFacade;
import com.intellij.psi.PsiClass;
import com.intellij.psi.PsiDirectory;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementFactory;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiJavaFile;
import com.intellij.psi.PsiManager;
import com.intellij.psi.PsiMember;
import com.intellij.psi.PsiMethod;
import com.intellij.psi.PsiPackage;
import com.intellij.psi.PsiPackageStatement;
import com.intellij.psi.PsiParameter;
import com.intellij.psi.PsiParameterList;
import com.intellij.psi.PsiReference;
import com.intellij.psi.PsiType;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.psi.search.searches.ReferencesSearch;
import com.intellij.refactoring.JavaRefactoringFactory;
import com.intellij.refactoring.MoveClassesOrPackagesRefactoring;
import com.intellij.refactoring.MoveDestination;
import com.intellij.refactoring.MoveMembersRefactoring;
import com.intellij.refactoring.RefactoringFactory;
import com.intellij.refactoring.RenameRefactoring;
import com.intellij.refactoring.SafeDeleteRefactoring;
import com.intellij.refactoring.changeSignature.ChangeSignatureProcessor;
import com.intellij.refactoring.changeSignature.ParameterInfoImpl;
import com.intellij.refactoring.extractInterface.ExtractInterfaceProcessor;
import com.intellij.refactoring.extractSuperclass.ExtractSuperClassProcessor;
import com.intellij.refactoring.inline.InlineMethodProcessor;
import com.intellij.refactoring.memberPullUp.PullUpProcessor;
import com.intellij.refactoring.memberPushDown.PushDownProcessor;
import com.intellij.refactoring.util.DocCommentPolicy;
import com.intellij.refactoring.util.classMembers.MemberInfo;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.tool.AgiTool;
import uno.anahata.asi.agi.tool.AgiToolException;
import uno.anahata.asi.agi.tool.AgiToolParam;
import uno.anahata.asi.agi.tool.AgiToolkit;
import uno.anahata.asi.agi.tool.AnahataToolkit;
import uno.anahata.asi.agi.tool.ToolPermission;
import uno.anahata.asi.intellij.internal.JavaPsi;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

/**
 * Programmatic Java refactoring for IntelliJ IDEA: project-wide rename, safe delete,
 * and find-usages.
 * <p>
 * This is the IntelliJ port of the NetBeans {@code Refactor} toolkit. Where NetBeans
 * drives {@code org.netbeans.modules.refactoring.api.*}, IntelliJ uses
 * {@link RefactoringFactory} (rename / safe-delete) and {@link ReferencesSearch}
 * (find-usages), move and copy (class and static members), pull-up/push-down, inline-method,
 * change-signature, and extract superclass/interface — the full NetBeans {@code Refactor}
 * surface. Refactorings run non-interactively (no preview dialog) on the EDT and update every
 * reference across all open projects. The processor-based operations use
 * {@code java-impl-refactorings}; copy is a PSI file-copy + package fix-up + optional rename.
 * </p>
 *
 * @author anahata
 */
@Slf4j
@AgiToolkit("Programmatic Java refactoring tools (rename, safe-delete, find-usages) for IntelliJ IDEA.")
public class Refactor extends AnahataToolkit {

    /**
     * Constructs the Refactor toolkit (instantiated reflectively via its public no-arg constructor).
     */
    public Refactor() {
    }

    /**
     * {@inheritDoc}
     * <p>
     * Documents the FQN scheme and the project-wide, non-interactive nature of these
     * refactorings.
     * </p>
     */
    @Override
    public List<String> getSystemInstructions() throws Exception {
        return List.of(
                "The Refactor toolkit performs project-wide, non-interactive Java refactorings that update all references. "
                + "Member FQNs use the canonical scheme 'pkg.Type.name(erasedArgType,...)' for methods and 'pkg.Type.fieldName' for fields, "
                + "matching the CodeModel toolkit. Use whereUsed/whereUsedMember to preview impact before renaming or deleting.");
    }

    /**
     * Renames a top-level class (and its source file), updating all references across
     * every open project.
     *
     * @param filePath the absolute path of the {@code .java} file whose primary class to rename.
     * @param newName  the new simple class name.
     * @return a confirmation message.
     * @throws AgiToolException if the file or its primary class cannot be resolved.
     */
    @AgiTool("Renames a top-level class and its file, updating all references across all open projects.")
    public String rename(
            @AgiToolParam("The absolute path of the .java file to rename.") String filePath,
            @AgiToolParam("The new simple class name.") String newName) throws AgiToolException {

        Object[] resolved = resolvePrimaryClass(filePath);
        Project project = (Project) resolved[0];
        PsiClass target = (PsiClass) resolved[1];
        runRename(project, target, newName, false);
        return "Renamed class in " + filePath + " to " + newName;
    }

    /**
     * Renames a class member (method or field) identified by its canonical FQN,
     * updating all references across every open project.
     *
     * @param filePath  the absolute path of the declaring {@code .java} file.
     * @param memberFqn the canonical FQN of the method or field to rename.
     * @param newName   the new simple member name.
     * @return a confirmation message.
     * @throws AgiToolException if the file or member cannot be resolved.
     */
    @AgiTool("Renames a class member (method or field) across all open projects.")
    public String renameMember(
            @AgiToolParam("The absolute path of the declaring .java file.") String filePath,
            @AgiToolParam("The canonical FQN of the method or field to rename.") String memberFqn,
            @AgiToolParam("The new simple member name.") String newName) throws AgiToolException {

        Project project = resolveHostProject(filePath);
        PsiElement member = ReadAction.compute(() -> JavaPsi.findMember(project, memberFqn));
        if (member == null) {
            throw new AgiToolException("Member not found: " + memberFqn);
        }
        runRename(project, member, newName, false);
        return "Renamed member " + memberFqn + " to " + newName;
    }

    /**
     * Safely deletes a class if it has no remaining references, otherwise fails without
     * deleting. Updates the project on success.
     *
     * @param filePath      the absolute path of the {@code .java} file to delete.
     * @param searchComments whether to also search comments and strings for references.
     * @return a confirmation message.
     * @throws AgiToolException if the file or its primary class cannot be resolved.
     */
    @AgiTool("Safely deletes a class if it has no remaining references; otherwise fails without deleting.")
    public String safeDelete(
            @AgiToolParam("The absolute path of the .java file to delete.") String filePath,
            @AgiToolParam("Whether to also search comments and strings for references.") boolean searchComments) throws AgiToolException {

        Object[] resolved = resolvePrimaryClass(filePath);
        Project project = (Project) resolved[0];
        PsiClass target = (PsiClass) resolved[1];

        ApplicationManager.getApplication().invokeAndWait(() -> {
            SafeDeleteRefactoring refactoring = RefactoringFactory.getInstance(project)
                    .createSafeDelete(new PsiElement[]{target});
            refactoring.setSearchInComments(searchComments);
            refactoring.setSearchInNonJavaFiles(searchComments);
            refactoring.setPreviewUsages(false);
            refactoring.run();
        });
        return "Safe-deleted class in " + filePath;
    }

    /**
     * Finds all references to the primary class of a file, across every open project.
     *
     * @param filePath      the absolute path of the {@code .java} file.
     * @param searchComments whether to also search comments and strings.
     * @return a Markdown listing of usages (file:line), or a no-usages message.
     * @throws AgiToolException if the file or its primary class cannot be resolved.
     */
    @AgiTool(value = "Finds all references/usages of a class in all open projects.", permission = ToolPermission.APPROVE_ALWAYS)
    public String whereUsed(
            @AgiToolParam("The absolute path of the .java file.") String filePath,
            @AgiToolParam("Whether to also search comments and strings.") boolean searchComments) throws AgiToolException {

        Object[] resolved = resolvePrimaryClass(filePath);
        Project project = (Project) resolved[0];
        PsiClass target = (PsiClass) resolved[1];
        return renderUsages(project, target);
    }

    /**
     * Finds all references to a specific class member (method or field) across every
     * open project.
     *
     * @param filePath      the absolute path of the declaring {@code .java} file.
     * @param memberFqn     the canonical FQN of the method or field.
     * @param searchComments whether to also search comments and strings.
     * @return a Markdown listing of usages (file:line), or a no-usages message.
     * @throws AgiToolException if the file or member cannot be resolved.
     */
    @AgiTool(value = "Finds all references/usages of a specific class member in all open projects.", permission = ToolPermission.APPROVE_ALWAYS)
    public String whereUsedMember(
            @AgiToolParam("The absolute path of the declaring .java file.") String filePath,
            @AgiToolParam("The canonical FQN of the method or field.") String memberFqn,
            @AgiToolParam("Whether to also search comments and strings.") boolean searchComments) throws AgiToolException {

        Project project = resolveHostProject(filePath);
        PsiElement member = ReadAction.compute(() -> JavaPsi.findMember(project, memberFqn));
        if (member == null) {
            throw new AgiToolException("Member not found: " + memberFqn);
        }
        return renderUsages(project, member);
    }

    /**
     * Moves a top-level class to a different package, updating all references across every
     * open project.
     *
     * @param filePath      the absolute path of the {@code .java} file to move.
     * @param targetPackage the destination package FQN (e.g. {@code com.foo.bar}).
     * @return a confirmation message.
     * @throws AgiToolException if the file or its primary class cannot be resolved.
     */
    @AgiTool("Moves a top-level class to a different package, updating all references across all open projects.")
    public String moveClass(
            @AgiToolParam("The absolute path of the .java file to move.") String filePath,
            @AgiToolParam("The destination package FQN, e.g. 'com.foo.bar'.") String targetPackage) throws AgiToolException {

        Object[] resolved = resolvePrimaryClass(filePath);
        Project project = (Project) resolved[0];
        PsiClass target = (PsiClass) resolved[1];

        ApplicationManager.getApplication().invokeAndWait(() -> {
            JavaRefactoringFactory factory = JavaRefactoringFactory.getInstance(project);
            MoveDestination destination = factory.createSourceFolderPreservingMoveDestination(targetPackage);
            MoveClassesOrPackagesRefactoring refactoring =
                    factory.createMoveClassesOrPackages(new PsiElement[]{target}, destination);
            refactoring.setSearchInComments(true);
            refactoring.setSearchInNonJavaFiles(true);
            refactoring.setPreviewUsages(false);
            refactoring.run();
        });
        return "Moved class in " + filePath + " to package " + targetPackage;
    }

    /**
     * Moves static members (methods/fields) to another class, updating all references.
     * <p>
     * Mirrors IntelliJ's "Move Members" refactoring, which applies to static members.
     * </p>
     *
     * @param filePath       the absolute path of the declaring {@code .java} file.
     * @param memberFqns     the canonical FQNs of the static members to move.
     * @param targetClassFqn the FQN of the destination class.
     * @return a confirmation message.
     * @throws AgiToolException if the file or no members can be resolved.
     */
    @AgiTool("Moves static members (methods/fields) to another class, updating all references.")
    public String moveMembers(
            @AgiToolParam("The absolute path of the declaring .java file.") String filePath,
            @AgiToolParam("The canonical FQNs of the static members to move.") List<String> memberFqns,
            @AgiToolParam("The FQN of the destination class.") String targetClassFqn) throws AgiToolException {

        Project project = resolveHostProject(filePath);
        List<PsiMember> members = ReadAction.compute(() -> {
            List<PsiMember> found = new ArrayList<>();
            for (String fqn : memberFqns) {
                PsiElement member = JavaPsi.findMember(project, fqn);
                if (member instanceof PsiMember psiMember) {
                    found.add(psiMember);
                }
            }
            return found;
        });
        if (members.isEmpty()) {
            throw new AgiToolException("No members resolved from: " + memberFqns);
        }

        ApplicationManager.getApplication().invokeAndWait(() -> {
            MoveMembersRefactoring refactoring = JavaRefactoringFactory.getInstance(project)
                    .createMoveMembers(members.toArray(new PsiMember[0]), targetClassFqn, "");
            refactoring.setPreviewUsages(false);
            refactoring.run();
        });
        return "Moved " + members.size() + " member(s) to " + targetClassFqn;
    }

    /**
     * Copies a top-level class to another package, optionally renaming it.
     * <p>
     * The source file is copied into the target package directory (created under the source's
     * source root if it does not exist), the copy's package declaration is rewritten, and — if
     * a new name is given — the copied class (and its constructors and file) are renamed. The
     * original is left untouched.
     * </p>
     *
     * @param filePath      the absolute path of the {@code .java} file to copy.
     * @param targetPackage the destination package FQN.
     * @param newName       the new class name, or {@code null} to keep the original.
     * @return a confirmation message.
     * @throws AgiToolException if the source, target package, or copy operation fails.
     */
    @AgiTool("Copies a class to another package, optionally renaming it, fixing the copy's package declaration.")
    public String copyClass(
            @AgiToolParam("The absolute path of the .java file to copy.") String filePath,
            @AgiToolParam("The destination package FQN, e.g. 'com.foo.bar'.") String targetPackage,
            @AgiToolParam(value = "The new class name, or null to keep the original.", required = false) String newName) throws AgiToolException {

        Object[] resolved = resolvePrimaryClass(filePath);
        Project project = (Project) resolved[0];
        PsiClass source = (PsiClass) resolved[1];

        AgiToolException[] failure = new AgiToolException[1];
        PsiFile[] copyHolder = new PsiFile[1];
        ApplicationManager.getApplication().invokeAndWait(() ->
                WriteCommandAction.runWriteCommandAction(project, () -> {
                    try {
                        PsiJavaFile sourceFile = (PsiJavaFile) source.getContainingFile();
                        PsiDirectory targetDir = resolveOrCreateTargetDir(project, source, targetPackage);
                        if (targetDir == null) {
                            failure[0] = new AgiToolException("Cannot resolve or create target package: " + targetPackage);
                            return;
                        }
                        String copyName = sourceFile.getName();
                        if (newName == null && targetDir.findFile(copyName) != null) {
                            failure[0] = new AgiToolException("Target package already contains: " + copyName);
                            return;
                        }
                        PsiFile copy = targetDir.copyFileFrom(copyName, sourceFile);
                        if (copy instanceof PsiJavaFile javaCopy) {
                            fixPackage(javaCopy, targetPackage, JavaPsiFacade.getElementFactory(project));
                        }
                        copyHolder[0] = copy;
                    } catch (Exception e) {
                        failure[0] = new AgiToolException("Copy failed: " + e.getMessage());
                    }
                }));
        if (failure[0] != null) {
            throw failure[0];
        }

        if (newName != null && copyHolder[0] instanceof PsiJavaFile javaCopy) {
            PsiClass copiedClass = ReadAction.compute(() -> javaCopy.getClasses().length > 0 ? javaCopy.getClasses()[0] : null);
            if (copiedClass != null) {
                runRename(project, copiedClass, newName, false);
            }
        }
        return "Copied class from " + filePath + " to package " + targetPackage
                + (newName != null ? " as " + newName : "");
    }

    /**
     * Pulls members up from a class to one of its superclasses, updating references.
     *
     * @param filePath            the absolute path of the subclass {@code .java} file.
     * @param targetSuperclassFqn the FQN of the superclass to pull the members up to.
     * @param memberFqns          the canonical FQNs of the members to pull up.
     * @return a confirmation message.
     * @throws AgiToolException if the class, superclass, or members cannot be resolved.
     */
    @AgiTool("Pulls members up from a class to one of its superclasses, updating references.")
    public String pullUpMembers(
            @AgiToolParam("The absolute path of the subclass .java file.") String filePath,
            @AgiToolParam("The FQN of the superclass to pull members up to.") String targetSuperclassFqn,
            @AgiToolParam("The canonical FQNs of the members to pull up.") List<String> memberFqns) throws AgiToolException {

        Object[] resolved = resolvePrimaryClass(filePath);
        Project project = (Project) resolved[0];
        PsiClass source = (PsiClass) resolved[1];
        PsiMember[] members = resolveMembers(project, memberFqns);
        PsiClass target = ReadAction.compute(() -> JavaPsi.findClass(project, targetSuperclassFqn));
        if (target == null) {
            throw new AgiToolException("Superclass not found: " + targetSuperclassFqn);
        }

        ApplicationManager.getApplication().invokeAndWait(() ->
                new PullUpProcessor(source, target, toMemberInfos(members), new DocCommentPolicy(DocCommentPolicy.ASIS)).run());
        return "Pulled up " + members.length + " member(s) to " + targetSuperclassFqn;
    }

    /**
     * Pushes members down from a class to its subclasses, updating references.
     *
     * @param filePath   the absolute path of the superclass {@code .java} file.
     * @param memberFqns the canonical FQNs of the members to push down.
     * @return a confirmation message.
     * @throws AgiToolException if the class or members cannot be resolved.
     */
    @AgiTool("Pushes members down from a class to its subclasses, updating references.")
    public String pushDownMembers(
            @AgiToolParam("The absolute path of the superclass .java file.") String filePath,
            @AgiToolParam("The canonical FQNs of the members to push down.") List<String> memberFqns) throws AgiToolException {

        Object[] resolved = resolvePrimaryClass(filePath);
        Project project = (Project) resolved[0];
        PsiClass source = (PsiClass) resolved[1];
        PsiMember[] members = resolveMembers(project, memberFqns);

        ApplicationManager.getApplication().invokeAndWait(() ->
                new PushDownProcessor<>(source, List.of(toMemberInfos(members)), new DocCommentPolicy(DocCommentPolicy.ASIS)).run());
        return "Pushed down " + members.length + " member(s) from " + source.getQualifiedName();
    }

    /**
     * Inlines a method: replaces every call with the method body and removes the method.
     *
     * @param filePath  the absolute path of the declaring {@code .java} file.
     * @param memberFqn the canonical FQN of the method to inline.
     * @return a confirmation message.
     * @throws AgiToolException if the file or method cannot be resolved.
     */
    @AgiTool("Inlines a method: replaces all calls with the method body and removes the method.")
    public String inlineMethod(
            @AgiToolParam("The absolute path of the declaring .java file.") String filePath,
            @AgiToolParam("The canonical FQN of the method to inline.") String memberFqn) throws AgiToolException {

        Project project = resolveHostProject(filePath);
        PsiMethod method = resolveMethod(project, memberFqn);

        ApplicationManager.getApplication().invokeAndWait(() ->
                new InlineMethodProcessor(project, method, null, null, false).run());
        return "Inlined method " + memberFqn;
    }

    /**
     * Changes a method's name and/or return type, preserving its parameters, and updates all
     * call sites.
     *
     * @param filePath      the absolute path of the declaring {@code .java} file.
     * @param memberFqn     the canonical FQN of the method to change.
     * @param newName       the new method name, or {@code null} to keep the current one.
     * @param newReturnType the new return type (e.g. {@code java.util.List<String>}), or {@code null} to keep it.
     * @return a confirmation message.
     * @throws AgiToolException if the file or method cannot be resolved.
     */
    @AgiTool("Changes a method's name and/or return type (preserving parameters) and updates all call sites.")
    public String changeMethodSignature(
            @AgiToolParam("The absolute path of the declaring .java file.") String filePath,
            @AgiToolParam("The canonical FQN of the method to change.") String memberFqn,
            @AgiToolParam(value = "The new method name, or null to keep it.", required = false) String newName,
            @AgiToolParam(value = "The new return type, or null to keep it.", required = false) String newReturnType) throws AgiToolException {

        Project project = resolveHostProject(filePath);
        PsiMethod method = resolveMethod(project, memberFqn);

        ApplicationManager.getApplication().invokeAndWait(() -> {
            PsiElementFactory factory = JavaPsiFacade.getElementFactory(project);
            PsiType returnType = newReturnType != null ? factory.createTypeFromText(newReturnType, method) : method.getReturnType();
            PsiParameterList parameterList = method.getParameterList();
            ParameterInfoImpl[] params = new ParameterInfoImpl[parameterList.getParametersCount()];
            for (int i = 0; i < params.length; i++) {
                PsiParameter parameter = parameterList.getParameter(i);
                params[i] = ParameterInfoImpl.create(i).withName(parameter.getName()).withType(parameter.getType());
            }
            String name = newName != null ? newName : method.getName();
            new ChangeSignatureProcessor(project, method, false, null, name, returnType, params).run();
        });
        return "Changed signature of " + memberFqn;
    }

    /**
     * Extracts a new superclass from a class, moving the selected members up into it.
     *
     * @param filePath        the absolute path of the {@code .java} file.
     * @param newSuperclass   the simple name of the new superclass.
     * @param memberFqns      the canonical FQNs of the members to move into the superclass.
     * @return a confirmation message.
     * @throws AgiToolException if the class or members cannot be resolved.
     */
    @AgiTool("Extracts a new superclass from a class, moving the selected members up into it.")
    public String extractSuperclass(
            @AgiToolParam("The absolute path of the .java file.") String filePath,
            @AgiToolParam("The simple name of the new superclass.") String newSuperclass,
            @AgiToolParam("The canonical FQNs of the members to move into the superclass.") List<String> memberFqns) throws AgiToolException {

        Object[] resolved = resolvePrimaryClass(filePath);
        Project project = (Project) resolved[0];
        PsiClass source = (PsiClass) resolved[1];
        PsiMember[] members = resolveMembers(project, memberFqns);
        PsiDirectory directory = ReadAction.compute(() -> source.getContainingFile().getContainingDirectory());

        ApplicationManager.getApplication().invokeAndWait(() ->
                new ExtractSuperClassProcessor(project, directory, newSuperclass, source, toMemberInfos(members),
                        false, new DocCommentPolicy(DocCommentPolicy.ASIS)).run());
        return "Extracted superclass " + newSuperclass + " from " + filePath;
    }

    /**
     * Extracts a new interface from a class, declaring the selected members in it.
     *
     * @param filePath     the absolute path of the {@code .java} file.
     * @param newInterface the simple name of the new interface.
     * @param memberFqns   the canonical FQNs of the members to declare in the interface.
     * @return a confirmation message.
     * @throws AgiToolException if the class or members cannot be resolved.
     */
    @AgiTool("Extracts a new interface from a class, declaring the selected members in it.")
    public String extractInterface(
            @AgiToolParam("The absolute path of the .java file.") String filePath,
            @AgiToolParam("The simple name of the new interface.") String newInterface,
            @AgiToolParam("The canonical FQNs of the members to declare in the interface.") List<String> memberFqns) throws AgiToolException {

        Object[] resolved = resolvePrimaryClass(filePath);
        Project project = (Project) resolved[0];
        PsiClass source = (PsiClass) resolved[1];
        PsiMember[] members = resolveMembers(project, memberFqns);
        PsiDirectory directory = ReadAction.compute(() -> source.getContainingFile().getContainingDirectory());

        ApplicationManager.getApplication().invokeAndWait(() ->
                new ExtractInterfaceProcessor(project, false, directory, newInterface, source, toMemberInfos(members),
                        new DocCommentPolicy(DocCommentPolicy.ASIS)).run());
        return "Extracted interface " + newInterface + " from " + filePath;
    }

    //<editor-fold defaultstate="collapsed" desc="Refactoring execution">
    /**
     * Runs a non-interactive project-wide rename on the EDT.
     *
     * @param project        the host project.
     * @param element        the element to rename.
     * @param newName        the new name.
     * @param searchComments whether to search comments and strings for references.
     */
    private void runRename(Project project, PsiElement element, String newName, boolean searchComments) {
        ApplicationManager.getApplication().invokeAndWait(() -> {
            RenameRefactoring refactoring = RefactoringFactory.getInstance(project).createRename(element, newName);
            refactoring.setSearchInComments(searchComments);
            refactoring.setPreviewUsages(false);
            refactoring.run();
        });
    }

    /**
     * Searches for and renders all references to an element as a Markdown list.
     *
     * @param project the host project.
     * @param element the element whose usages to find.
     * @return a Markdown listing of usages, or a no-usages message.
     */
    private String renderUsages(Project project, PsiElement element) {
        List<String> usages = ReadAction.compute(() -> {
            List<String> found = new ArrayList<>();
            Collection<PsiReference> references = ReferencesSearch.search(element, GlobalSearchScope.allScope(project)).findAll();
            for (PsiReference reference : references) {
                PsiElement usage = reference.getElement();
                PsiFile file = usage.getContainingFile();
                if (file == null) {
                    continue;
                }
                VirtualFile vf = file.getVirtualFile();
                String path = vf != null ? vf.getPath() : file.getName();
                Document document = PsiDocumentManager.getInstance(project).getDocument(file);
                int line = document != null ? document.getLineNumber(usage.getTextOffset()) + 1 : -1;
                found.add("- `" + path + (line > 0 ? ":" + line : "") + "`");
            }
            return found;
        });

        if (usages.isEmpty()) {
            return "No usages found.";
        }
        return "Found " + usages.size() + " usage(s):\n" + String.join("\n", usages);
    }
    //</editor-fold>

    //<editor-fold defaultstate="collapsed" desc="PSI resolution helpers">
    /**
     * Resolves an absolute file path to {@code [Project, PsiClass]} for its primary
     * top-level class, failing fast if it is not resolvable Java source.
     *
     * @param filePath the absolute path.
     * @return a two-element array of the host project and its primary class.
     * @throws AgiToolException if the file or its primary class cannot be resolved.
     */
    private Object[] resolvePrimaryClass(String filePath) throws AgiToolException {
        Project project = resolveHostProject(filePath);
        VirtualFile vf = JavaPsi.findVirtualFile(filePath);
        PsiClass primary = ReadAction.compute(() -> JavaPsi.primaryClass(project, vf));
        if (primary == null) {
            throw new AgiToolException("No primary Java class in: " + filePath);
        }
        return new Object[]{project, primary};
    }

    /**
     * Resolves the open project hosting the given file path, failing fast if unresolved.
     *
     * @param filePath the absolute path.
     * @return the hosting project.
     * @throws AgiToolException if the path cannot be resolved or hosted.
     */
    private Project resolveHostProject(String filePath) throws AgiToolException {
        VirtualFile vf = JavaPsi.findVirtualFile(filePath);
        if (vf == null) {
            throw new AgiToolException("Could not resolve a VirtualFile for: " + filePath);
        }
        Project project = JavaPsi.findHostProject(vf);
        if (project == null) {
            throw new AgiToolException("No open project can host: " + filePath);
        }
        JavaPsi.requireSmart(project);
        return project;
    }

    /**
     * Resolves a list of canonical member FQNs to PSI members, failing fast if none resolve.
     *
     * @param project    the host project.
     * @param memberFqns the canonical member FQNs.
     * @return the resolved members.
     * @throws AgiToolException if no members resolve.
     */
    private PsiMember[] resolveMembers(Project project, List<String> memberFqns) throws AgiToolException {
        List<PsiMember> members = ReadAction.compute(() -> {
            List<PsiMember> found = new ArrayList<>();
            for (String fqn : memberFqns) {
                if (JavaPsi.findMember(project, fqn) instanceof PsiMember member) {
                    found.add(member);
                }
            }
            return found;
        });
        if (members.isEmpty()) {
            throw new AgiToolException("No members resolved from: " + memberFqns);
        }
        return members.toArray(new PsiMember[0]);
    }

    /**
     * Resolves a canonical method FQN to a PSI method, failing fast otherwise.
     *
     * @param project   the host project.
     * @param memberFqn the canonical method FQN.
     * @return the resolved method.
     * @throws AgiToolException if the FQN does not resolve to a method.
     */
    private PsiMethod resolveMethod(Project project, String memberFqn) throws AgiToolException {
        PsiMethod method = ReadAction.compute(() ->
                JavaPsi.findMember(project, memberFqn) instanceof PsiMethod psiMethod ? psiMethod : null);
        if (method == null) {
            throw new AgiToolException("Not a method: " + memberFqn);
        }
        return method;
    }

    /**
     * Resolves the target package's directory, preferring one under the source's own source
     * root, and creating the package directory chain if it does not yet exist. Must be called
     * inside a write action (it may create directories).
     *
     * @param project       the host project.
     * @param source        the class being copied (used to locate the source root).
     * @param targetPackage the destination package FQN.
     * @return the target directory, or {@code null} if it cannot be resolved or created.
     */
    private PsiDirectory resolveOrCreateTargetDir(Project project, PsiClass source, String targetPackage) {
        VirtualFile sourceRoot = ProjectFileIndex.getInstance(project)
                .getSourceRootForFile(source.getContainingFile().getVirtualFile());
        PsiPackage psiPackage = JavaPsiFacade.getInstance(project).findPackage(targetPackage);
        if (psiPackage != null) {
            PsiDirectory[] directories = psiPackage.getDirectories();
            for (PsiDirectory directory : directories) {
                if (sourceRoot != null && VfsUtilCore.isAncestor(sourceRoot, directory.getVirtualFile(), false)) {
                    return directory;
                }
            }
            if (directories.length > 0) {
                return directories[0];
            }
        }
        if (sourceRoot != null) {
            PsiDirectory rootDir = PsiManager.getInstance(project).findDirectory(sourceRoot);
            if (rootDir != null) {
                return DirectoryUtil.createSubdirectories(targetPackage, rootDir, ".");
            }
        }
        return null;
    }

    /**
     * Rewrites (or removes, for the default package) the package statement of a copied file.
     * Must be called inside a write action.
     *
     * @param javaFile      the copied Java file.
     * @param targetPackage the destination package FQN ({@code ""} for the default package).
     * @param factory       the element factory.
     */
    private void fixPackage(PsiJavaFile javaFile, String targetPackage, PsiElementFactory factory) {
        PsiPackageStatement existing = javaFile.getPackageStatement();
        if (targetPackage.isEmpty()) {
            if (existing != null) {
                existing.delete();
            }
            return;
        }
        PsiPackageStatement replacement = factory.createPackageStatement(targetPackage);
        if (existing != null) {
            existing.replace(replacement);
        } else {
            javaFile.addBefore(replacement, javaFile.getFirstChild());
        }
    }

    /**
     * Wraps PSI members as refactoring {@link MemberInfo}s. Must be called inside a read
     * action or on the EDT.
     *
     * @param members the members to wrap.
     * @return the member-info array.
     */
    private MemberInfo[] toMemberInfos(PsiMember[] members) {
        MemberInfo[] infos = new MemberInfo[members.length];
        for (int i = 0; i < members.length; i++) {
            infos[i] = new MemberInfo(members[i]);
        }
        return infos;
    }
    //</editor-fold>
}
