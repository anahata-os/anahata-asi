/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.intellij.tools.java;

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.application.ReadAction;
import com.intellij.openapi.command.WriteCommandAction;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.fileEditor.FileDocumentManager;
import com.intellij.openapi.project.Project;
import com.intellij.psi.JavaPsiFacade;
import com.intellij.psi.PsiAnnotation;
import com.intellij.psi.PsiClass;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiElementFactory;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiJavaFile;
import com.intellij.psi.PsiModifierList;
import com.intellij.psi.PsiModifierListOwner;
import com.intellij.psi.codeStyle.CodeStyleManager;
import com.intellij.psi.codeStyle.JavaCodeStyleManager;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.openapi.vfs.VirtualFile;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.tool.AgiTool;
import uno.anahata.asi.agi.tool.AgiToolException;
import uno.anahata.asi.agi.tool.AgiToolParam;
import uno.anahata.asi.agi.tool.AgiToolkit;
import uno.anahata.asi.agi.tool.AnahataToolkit;
import uno.anahata.asi.intellij.internal.JavaPsi;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

/**
 * AST-based, non-textual maintenance operations on Java source files: structural
 * import management, IDE-rule reformatting, and annotation insertion.
 * <p>
 * This is the IntelliJ port of the NetBeans V3 {@code CodeRefiner}. Where NetBeans
 * drives {@code WorkingCopy}/{@code TreeMaker}, IntelliJ mutates the live PSI tree
 * inside a {@link WriteCommandAction}. Because AI tool calls arrive on background
 * threads, every mutation is marshalled onto the EDT via
 * {@code ApplicationManager.getApplication().invokeAndWait}; PSI reads needed to resolve targets run
 * inside that same command.
 * </p>
 *
 * @author anahata
 */
@Slf4j
@AgiToolkit("AST-based updates of java code: structural imports, reformatting and annotations.")
public class CodeRefiner extends AnahataToolkit {

    /**
     * Constructs the CodeRefiner toolkit (instantiated reflectively via its public no-arg constructor).
     */
    public CodeRefiner() {
    }

    /**
     * Adds one or more imports to a Java file structurally (as real import
     * statements, resolving each fully-qualified name against the project).
     *
     * @param filePath the absolute path of the Java file.
     * @param imports  the fully-qualified type names to import.
     * @param save     whether to persist the file to disk afterwards.
     * @return a summary of which imports were added or skipped.
     * @throws AgiToolException if the file is not a resolvable Java source file.
     */
    @AgiTool("Adds one or more imports to a file structurally, resolving each FQN against the project classpath.")
    public String addImports(
            @AgiToolParam("The absolute path of the Java file.") String filePath,
            @AgiToolParam("The fully-qualified type names to import.") List<String> imports,
            @AgiToolParam("Whether to save the file to disk after editing.") boolean save) throws AgiToolException {

        PsiJavaFile javaFile = resolveJavaFile(filePath);
        Project project = javaFile.getProject();
        StringBuilder result = new StringBuilder();

        runWrite(project, () -> {
            JavaCodeStyleManager styleManager = JavaCodeStyleManager.getInstance(project);
            for (String fqn : imports) {
                PsiClass cls = JavaPsiFacade.getInstance(project).findClass(fqn, GlobalSearchScope.allScope(project));
                if (cls != null) {
                    styleManager.addImport(javaFile, cls);
                    result.append("Added import: ").append(fqn).append("\n");
                } else {
                    result.append("Skipped (type not found on classpath): ").append(fqn).append("\n");
                }
            }
            if (save) {
                saveFile(project, javaFile);
            }
        });

        log(result.toString());
        return result.toString();
    }

    /**
     * Optimizes the imports of a Java file: removes unused imports (when requested)
     * and shortens any fully-qualified references to simple names, adding imports as
     * needed. Delegates to IntelliJ's {@link JavaCodeStyleManager}.
     *
     * @param filePath     the absolute path of the Java file.
     * @param removeUnused whether unused imports should be removed.
     * @param save         whether to persist the file to disk afterwards.
     * @return a confirmation message.
     * @throws AgiToolException if the file is not a resolvable Java source file.
     */
    @AgiTool("Optimizes imports (shortens FQNs to simple names, adds missing, and optionally removes unused).")
    public String optimizeImports(
            @AgiToolParam("The absolute path of the Java file.") String filePath,
            @AgiToolParam("Whether unused imports should be removed.") boolean removeUnused,
            @AgiToolParam("Whether to save the file to disk after editing.") boolean save) throws AgiToolException {

        PsiJavaFile javaFile = resolveJavaFile(filePath);
        Project project = javaFile.getProject();

        runWrite(project, () -> {
            JavaCodeStyleManager styleManager = JavaCodeStyleManager.getInstance(project);
            styleManager.shortenClassReferences(javaFile);
            if (removeUnused) {
                styleManager.optimizeImports(javaFile);
            }
            if (save) {
                saveFile(project, javaFile);
            }
        });

        return "Optimized imports for " + filePath + (removeUnused ? " (removed unused)" : "");
    }

    /**
     * Reformats an entire Java file using the project's active IDE code-style rules.
     *
     * @param filePath the absolute path of the file to reformat.
     * @param save     whether to persist the file to disk afterwards.
     * @return a confirmation message.
     * @throws AgiToolException if the file cannot be resolved as a PSI file.
     */
    @AgiTool("Reformats a file using the project's IDE code-style rules.")
    public String reformat(
            @AgiToolParam("The absolute path of the file to reformat.") String filePath,
            @AgiToolParam("Whether to save the file to disk after formatting.") boolean save) throws AgiToolException {

        PsiFile psiFile = resolvePsiFile(filePath);
        Project project = psiFile.getProject();

        runWrite(project, () -> {
            CodeStyleManager.getInstance(project).reformat(psiFile);
            if (save) {
                saveFile(project, psiFile);
            }
        });

        return "Reformatted " + filePath;
    }

    /**
     * Adds an annotation to a class, method, or field identified by its canonical
     * fully-qualified name.
     * <p>
     * The annotation text may include arguments (e.g. {@code SuppressWarnings("unchecked")})
     * and an optional leading {@code @}. References inside the annotation are shortened
     * to simple names where an import is available.
     * </p>
     *
     * @param filePath   the absolute path of the Java file.
     * @param memberFqn  the canonical FQN of the target class/method/field.
     * @param annotation the annotation source (with or without a leading {@code @}).
     * @param save       whether to persist the file to disk afterwards.
     * @return a confirmation message.
     * @throws AgiToolException if the file or target member cannot be resolved.
     */
    @AgiTool("Adds an annotation to a class, method, or field identified by its fully-qualified name.")
    public String addAnnotation(
            @AgiToolParam("The absolute path of the Java file.") String filePath,
            @AgiToolParam("The fully-qualified name of the target class, method, or field.") String memberFqn,
            @AgiToolParam("The annotation source, e.g. 'Override' or 'SuppressWarnings(\"unchecked\")'.") String annotation,
            @AgiToolParam("Whether to save the file to disk after editing.") boolean save) throws AgiToolException {

        PsiJavaFile javaFile = resolveJavaFile(filePath);
        Project project = javaFile.getProject();
        String annotationText = annotation.startsWith("@") ? annotation : "@" + annotation;

        runWrite(project, () -> {
            PsiModifierListOwner owner = JavaPsi.findAnnotatable(project, memberFqn);
            if (owner == null) {
                throw new AgiToolException("Target not found for FQN: " + memberFqn);
            }
            PsiModifierList modifierList = owner.getModifierList();
            if (modifierList == null) {
                throw new AgiToolException("Target has no modifier list (cannot annotate): " + memberFqn);
            }
            PsiElementFactory factory = JavaPsiFacade.getElementFactory(project);
            PsiAnnotation psiAnnotation = factory.createAnnotationFromText(annotationText, owner);
            modifierList.addBefore(psiAnnotation, modifierList.getFirstChild());
            JavaCodeStyleManager.getInstance(project).shortenClassReferences(javaFile);
            if (save) {
                saveFile(project, javaFile);
            }
        });

        return "Added " + annotationText + " to " + memberFqn;
    }

    //<editor-fold defaultstate="collapsed" desc="PSI resolution helpers">
    /**
     * Resolves an absolute path to a {@link PsiJavaFile}, failing fast if the file is
     * missing or is not Java source.
     *
     * @param filePath the absolute path.
     * @return the resolved Java PSI file.
     * @throws AgiToolException if the file cannot be resolved to Java source.
     */
    private PsiJavaFile resolveJavaFile(String filePath) throws AgiToolException {
        PsiFile psiFile = resolvePsiFile(filePath);
        if (psiFile instanceof PsiJavaFile javaFile) {
            return javaFile;
        }
        throw new AgiToolException("Not a Java source file: " + filePath);
    }

    /**
     * Resolves an absolute path to a generic {@link PsiFile} within an open project.
     *
     * @param filePath the absolute path.
     * @return the resolved PSI file.
     * @throws AgiToolException if the path does not exist or cannot be hosted by an open project.
     */
    private PsiFile resolvePsiFile(String filePath) throws AgiToolException {
        Path path = Path.of(filePath);
        if (!Files.exists(path)) {
            throw new AgiToolException("File does not exist: " + filePath);
        }
        VirtualFile vf = JavaPsi.findVirtualFile(filePath);
        if (vf == null) {
            throw new AgiToolException("Could not resolve a VirtualFile for: " + filePath);
        }
        Project project = JavaPsi.findHostProject(vf);
        if (project == null) {
            throw new AgiToolException("No open project can host file: " + filePath);
        }
        JavaPsi.requireSmart(project);
        PsiFile psiFile = ReadAction.compute(() -> JavaPsi.findPsiFile(project, vf));
        if (psiFile == null) {
            throw new AgiToolException("Could not resolve a PSI file for: " + filePath);
        }
        return psiFile;
    }
    //</editor-fold>

    //<editor-fold defaultstate="collapsed" desc="Write / save plumbing">
    /**
     * Runs a mutating PSI operation as a single undoable write command on the EDT.
     * <p>
     * Marshals onto the EDT (AI tool calls arrive on background threads) and wraps the
     * body in a {@link WriteCommandAction}. Checked {@link AgiToolException}s thrown by
     * the body are unwrapped and rethrown so the model receives a clean error.
     * </p>
     *
     * @param project the project owning the document.
     * @param action  the mutation to perform.
     * @throws AgiToolException if the mutation reports a domain error.
     */
    private void runWrite(Project project, WriteBody action) throws AgiToolException {
        AgiToolException[] failure = new AgiToolException[1];
        ApplicationManager.getApplication().invokeAndWait(() ->
                WriteCommandAction.runWriteCommandAction(project, () -> {
                    try {
                        action.run();
                    } catch (AgiToolException e) {
                        failure[0] = e;
                    }
                }));
        if (failure[0] != null) {
            throw failure[0];
        }
    }

    /**
     * Commits pending PSI changes for the file's document and writes it to disk.
     * <p>
     * Must be called inside a write action on the EDT.
     * </p>
     *
     * @param project the host project.
     * @param psiFile the file to persist.
     */
    private void saveFile(Project project, PsiFile psiFile) {
        PsiDocumentManager documentManager = PsiDocumentManager.getInstance(project);
        Document document = documentManager.getDocument(psiFile);
        if (document != null) {
            documentManager.doPostponedOperationsAndUnblockDocument(document);
            FileDocumentManager.getInstance().saveDocument(document);
        }
    }

    /**
     * A mutating PSI operation that may raise a domain-level {@link AgiToolException}.
     */
    @FunctionalInterface
    private interface WriteBody {

        /**
         * Performs the mutation.
         *
         * @throws AgiToolException on a domain-level failure the model should see.
         */
        void run() throws AgiToolException;
    }
    //</editor-fold>
}
