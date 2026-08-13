/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.intellij.tools.java;

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.application.ReadAction;
import com.intellij.openapi.command.WriteCommandAction;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.fileEditor.FileDocumentManager;
import com.intellij.openapi.project.Project;
import com.intellij.psi.JavaPsiFacade;
import com.intellij.psi.PsiClass;
import com.intellij.psi.PsiClassInitializer;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementFactory;
import com.intellij.psi.PsiField;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiJavaFile;
import com.intellij.psi.PsiMember;
import com.intellij.psi.PsiMethod;
import com.intellij.psi.codeStyle.CodeStyleManager;
import com.intellij.psi.codeStyle.JavaCodeStyleManager;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.openapi.vfs.VirtualFile;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.tool.AgiTool;
import uno.anahata.asi.agi.tool.AgiToolException;
import uno.anahata.asi.agi.tool.AgiToolParam;
import uno.anahata.asi.agi.tool.AgiToolkit;
import uno.anahata.asi.agi.tool.AnahataToolkit;
import uno.anahata.asi.internal.AnahataDiffUtils;
import uno.anahata.asi.intellij.internal.JavaPsi;
import uno.anahata.asi.intellij.tools.java.coderefiner.CodeRefinementBatch;
import uno.anahata.asi.intellij.tools.java.coderefiner.CodeRefinementIntent;
import uno.anahata.asi.intellij.tools.java.coderefiner.RelativePosition;

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

        PsiJavaFile javaFile = resolveJavaFile(batch.getFilePath());
        Project project = javaFile.getProject();
        String fileName = javaFile.getName();

        String before = ReadAction.compute(javaFile::getText);

        runWrite(project, () -> {
            PsiElementFactory factory = JavaPsiFacade.getElementFactory(project);
            for (CodeRefinementIntent intent : batch.getIntents()) {
                applyIntent(project, factory, intent);
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

        String after = ReadAction.compute(javaFile::getText);
        String diff = AnahataDiffUtils.generateUnifiedDiff(before, after, fileName);
        log(diff);
        return diff;
    }

    //<editor-fold defaultstate="collapsed" desc="Intent application">
    /**
     * Applies a single intent to the live PSI tree. Must run inside a write command.
     *
     * @param project the host project.
     * @param factory the element factory used to parse member declarations.
     * @param intent  the intent to apply.
     * @throws AgiToolException if the intent target cannot be resolved.
     */
    private void applyIntent(Project project, PsiElementFactory factory, CodeRefinementIntent intent) throws AgiToolException {
        switch (intent.getType()) {
            case INSERT -> {
                PsiClass target = JavaPsi.findClass(project, intent.getClassFqn());
                if (target == null) {
                    throw new AgiToolException("INSERT target class not found: " + intent.getClassFqn());
                }
                PsiMember member = parseMember(factory, intent.getDeclaration(), target);
                insertMember(target, member, intent.getPosition(), intent.getAnchorMemberName());
            }
            case UPDATE -> {
                PsiElement existing = requireMember(project, intent.getMemberFqn());
                PsiMember replacement = parseMember(factory, intent.getDeclaration(), existing);
                existing.replace(replacement);
            }
            case DELETE -> requireMember(project, intent.getMemberFqn()).delete();
            case MOVE -> {
                PsiElement existing = requireMember(project, intent.getMemberFqn());
                PsiClass parent = PsiTreeUtil.getParentOfType(existing, PsiClass.class);
                if (parent == null) {
                    throw new AgiToolException("MOVE member has no enclosing class: " + intent.getMemberFqn());
                }
                PsiElement copy = existing.copy();
                existing.delete();
                insertMember(parent, (PsiMember) copy, intent.getPosition(), intent.getAnchorMemberName());
            }
            default -> throw new AgiToolException("Unknown intent type: " + intent.getType());
        }
    }

    /**
     * Resolves a member FQN to its PSI element, failing fast if absent.
     *
     * @param project   the host project.
     * @param memberFqn the canonical member FQN.
     * @return the resolved member.
     * @throws AgiToolException if no member matches.
     */
    private PsiElement requireMember(Project project, String memberFqn) throws AgiToolException {
        PsiElement member = JavaPsi.findMember(project, memberFqn);
        if (member == null) {
            throw new AgiToolException("Member not found: " + memberFqn);
        }
        return member;
    }

    /**
     * Parses a verbatim member declaration into a detached PSI member by wrapping it in a
     * throwaway class, robustly handling methods, fields, inner classes and initializers.
     *
     * @param factory     the element factory.
     * @param declaration the member source.
     * @param context     a PSI context element (for resolution/scope).
     * @return the parsed member.
     * @throws AgiToolException if no member can be parsed from the declaration.
     */
    private PsiMember parseMember(PsiElementFactory factory, String declaration, PsiElement context) throws AgiToolException {
        PsiClass holder = factory.createClassFromText(declaration, context);
        if (holder.getMethods().length > 0) {
            return holder.getMethods()[0];
        }
        if (holder.getFields().length > 0) {
            return holder.getFields()[0];
        }
        if (holder.getInnerClasses().length > 0) {
            return holder.getInnerClasses()[0];
        }
        if (holder.getInitializers().length > 0) {
            return holder.getInitializers()[0];
        }
        throw new AgiToolException("Could not parse a member from declaration: " + declaration);
    }

    /**
     * Inserts a member into a class at the requested position (defaulting to END).
     *
     * @param target     the target class.
     * @param member     the member to insert.
     * @param position   the requested placement, or {@code null} for END.
     * @param anchorName the anchor member simple name for BEFORE/AFTER placement.
     * @throws AgiToolException if a BEFORE/AFTER anchor is required but not found.
     */
    private void insertMember(PsiClass target, PsiMember member, RelativePosition position, String anchorName) throws AgiToolException {
        RelativePosition pos = position != null ? position : RelativePosition.END;
        switch (pos) {
            case END -> target.add(member);
            case START -> {
                PsiElement lBrace = target.getLBrace();
                if (lBrace != null) {
                    target.addAfter(member, lBrace);
                } else {
                    target.add(member);
                }
            }
            case BEFORE -> target.addBefore(member, requireAnchor(target, anchorName));
            case AFTER -> target.addAfter(member, requireAnchor(target, anchorName));
        }
    }

    /**
     * Finds an anchor member by simple name within a class, failing fast if absent.
     *
     * @param target     the class to search.
     * @param anchorName the simple member name.
     * @return the anchor member.
     * @throws AgiToolException if no member with that name exists.
     */
    private PsiMember requireAnchor(PsiClass target, String anchorName) throws AgiToolException {
        for (PsiMethod method : target.getMethods()) {
            if (method.getName().equals(anchorName)) {
                return method;
            }
        }
        for (PsiField field : target.getFields()) {
            if (field.getName().equals(anchorName)) {
                return field;
            }
        }
        for (PsiClass inner : target.getInnerClasses()) {
            if (anchorName.equals(inner.getName())) {
                return inner;
            }
        }
        throw new AgiToolException("Anchor member not found in " + target.getQualifiedName() + ": " + anchorName);
    }
    //</editor-fold>

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
