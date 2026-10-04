/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.intellij.tools.java;

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.application.ReadAction;
import com.intellij.openapi.command.WriteCommandAction;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.fileEditor.FileDocumentManager;
import com.intellij.openapi.project.Project;
import com.intellij.psi.JavaPsiFacade;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiElementFactory;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiJavaFile;
import com.intellij.psi.codeStyle.CodeStyleManager;
import com.intellij.psi.codeStyle.JavaCodeStyleManager;
import com.intellij.openapi.vfs.VirtualFile;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.resource.Resource;
import uno.anahata.asi.agi.resource.handle.PathHandle;
import uno.anahata.asi.agi.tool.AgiTool;
import uno.anahata.asi.agi.tool.AgiToolException;
import uno.anahata.asi.agi.tool.AgiToolParam;
import uno.anahata.asi.agi.tool.AgiToolkit;
import uno.anahata.asi.agi.tool.AnahataToolkit;
import uno.anahata.asi.internal.AnahataDiffUtils;
import uno.anahata.asi.intellij.internal.JavaPsi;
import uno.anahata.asi.intellij.internal.ProjectUtils;
import uno.anahata.asi.intellij.tools.java.coderefiner.CodeRefinementBatch;
import uno.anahata.asi.intellij.tools.java.coderefiner.CodeRefinementIntent;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

/**
 * Structural, member-level Java refinement (the IntelliJ V4 AST-Guided Batch engine).
 * <p>
 * Applies a batch of {@link CodeRefinementIntent}s — insert / update / delete / move of
 * whole members — to a single Java file atomically, against the live PSI tree, then
 * optimizes imports and reformats, and returns a unified diff of the change.
 * </p>
 * <p>
 * This is the IntelliJ port of the NetBeans {@code BatchCodeRefiner}. Where NetBeans
 * splices text using {@code WorkingCopy}/{@code SourcePositions}, IntelliJ mutates the PSI
 * tree directly: member declarations are parsed via
 * {@link PsiElementFactory#createClassFromText} (which robustly handles methods, fields,
 * inner classes and initializers) and then added/replaced/deleted. The whole batch runs in
 * one {@link WriteCommandAction} on the EDT, so it is a single undoable transaction. Basic
 * text editing remains available via the core {@code Resources} toolkit and
 * {@code CodeRefiner}; this toolkit is the structure-aware, member-level path.
 * </p>
 *
 * @author anahata
 */
@Slf4j
@AgiToolkit("Advanced structural Java refinement (V4 AST-Guided Batch Mode): insert/update/delete/move whole members atomically.")
public class BatchCodeRefiner extends AnahataToolkit {

    /**
     * Constructs the BatchCodeRefiner toolkit (instantiated reflectively via its public no-arg constructor).
     */
    public BatchCodeRefiner() {
    }

    /**
     * {@inheritDoc}
     * <p>
     * Explains the batch model and the canonical FQN scheme the intents use.
     * </p>
     */
    @Override
    public List<String> getSystemInstructions() throws Exception {
        return List.of(
                "BatchCodeRefiner applies member-level structural edits to ONE Java file atomically and returns a unified diff. "
                + "Supply full member source in each intent's 'declaration' (Javadoc + annotations + modifiers + body). "
                + "INSERT needs classFqn + declaration (+ position/anchor); UPDATE needs memberFqn + declaration; DELETE needs memberFqn; "
                + "MOVE needs memberFqn + position/anchor. Member FQNs use 'pkg.Type.name(argType,...)' for methods and 'pkg.Type.field' for fields.");
    }

    /**
     * Applies a batch of structural member modifications to a single Java file atomically.
     *
     * @param batch the modification batch (file path, intents, import/save flags).
     * @return a unified diff of the applied change.
     * @throws AgiToolException if the file cannot be resolved or an intent is invalid.
     */
    @AgiTool("The definitive structural Java refiner: applies a batch of member-level modifications (insert/update/delete/move) to ONE Java file atomically and returns a unified diff.")
    public String refine(
            @AgiToolParam("The batch of member-level modifications to apply.") CodeRefinementBatch batch) throws AgiToolException {

        PsiJavaFile javaFile = resolveJavaFile(batch);
        Project project = javaFile.getProject();
        String fileName = javaFile.getName();

        String before = ReadAction.computeBlocking(javaFile::getText);

        if (batch.getManualOverride() != null && !batch.getManualOverride().isBlank()) {
            runWrite(project, () -> {
                Document doc = FileDocumentManager.getInstance().getDocument(javaFile.getVirtualFile());
                if (doc != null) {
                    doc.setText(batch.getManualOverride());
                }
                if (batch.isSave()) {
                    saveFile(project, javaFile);
                }
            });
        } else {
            runWrite(project, () -> {
                PsiElementFactory factory = JavaPsiFacade.getElementFactory(project);
                for (CodeRefinementIntent intent : batch.getIntents()) {
                    CodeRefinementBatch.applyIntentToPsi(project, factory, javaFile, intent);
                }
                JavaCodeStyleManager styleManager = JavaCodeStyleManager.getInstance(project);
                styleManager.shortenClassReferences(javaFile);
                if (batch.isOptimizeImports()) {
                    styleManager.optimizeImports(javaFile);
                }
                CodeStyleManager.getInstance(project).reformat(javaFile);
                if (batch.isSave()) {
                    saveFile(project, javaFile);
                }
            });
        }

        String after = ReadAction.computeBlocking(javaFile::getText);
        String diff = AnahataDiffUtils.generateUnifiedDiff(fileName, before, after);
        log(diff);
        return diff;
    }

    /**
     * Resolves the target Java file from either explicit filePath or the session's resource UUID.
     *
     * @param batch the refinement batch.
     * @return the resolved Java PSI file.
     * @throws AgiToolException if file cannot be resolved.
     */
    private PsiJavaFile resolveJavaFile(CodeRefinementBatch batch) throws AgiToolException {
        String filePath = batch.getFilePath();
        if (filePath == null && batch.getResourceUuid() != null) {
            Resource r = getToolContext().getResourceManager().get(batch.getResourceUuid());
            if (r != null && r.getHandle() instanceof PathHandle ph) {
                filePath = ph.getPath();
            }
        }
        if (filePath == null) {
            throw new AgiToolException("Neither filePath nor a valid resourceUuid was provided for refinement batch.");
        }
        return resolveJavaFile(filePath);
    }

    //<editor-fold defaultstate="collapsed" desc="Write / resolve plumbing">
    /**
     * Resolves an absolute path to a {@link PsiJavaFile}, failing fast on non-Java input.
     *
     * @param filePath the absolute path.
     * @return the Java PSI file.
     * @throws AgiToolException if the file cannot be resolved as Java source.
     */
    private PsiJavaFile resolveJavaFile(String filePath) throws AgiToolException {
        if (!Files.exists(Path.of(filePath))) {
            throw new AgiToolException("File does not exist: " + filePath);
        }
        VirtualFile vf = ProjectUtils.findVirtualFile(filePath);
        if (vf == null) {
            throw new AgiToolException("Could not resolve a VirtualFile for: " + filePath);
        }
        Project project = ProjectUtils.findHostProject(vf);
        if (project == null) {
            throw new AgiToolException("No open project can host file: " + filePath);
        }
        JavaPsi.requireSmart(project);
        PsiFile psiFile = ReadAction.computeBlocking(() -> JavaPsi.findPsiFile(project, vf));
        if (psiFile instanceof PsiJavaFile javaFile) {
            return javaFile;
        }
        throw new AgiToolException("Not a Java source file: " + filePath);
    }

    /**
     * Runs a mutating batch as a single undoable write command on the EDT, unwrapping any
     * domain {@link AgiToolException} thrown by the body.
     *
     * @param project the project owning the document.
     * @param action  the mutation body.
     * @throws AgiToolException if the body reports a domain error.
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
     * Commits pending PSI changes and writes the file to disk. Must run inside a write
     * action on the EDT.
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
     * A mutating batch body that may raise a domain-level {@link AgiToolException}.
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
