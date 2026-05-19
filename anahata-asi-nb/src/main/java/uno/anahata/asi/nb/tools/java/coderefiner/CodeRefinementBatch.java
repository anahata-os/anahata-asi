/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.tools.java.coderefiner;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.sun.source.tree.CompilationUnitTree;
import java.util.ArrayList;
import java.util.List;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.netbeans.api.java.source.*;
import io.swagger.v3.oas.annotations.media.Schema;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.agi.tool.AgiToolException;
import uno.anahata.asi.toolkit.resources.text.AbstractTextResourceWrite;
import uno.anahata.asi.toolkit.resources.text.LineComment;
import uno.anahata.asi.nb.resources.handle.NbHandle;

/**
 * A robust, agent-friendly batch of structural AST modifications for a single
 * Java file.
 * <p>
 * This class extends {@link AbstractTextResourceWrite} to inherit optimistic
 * locking and path resolution, while providing the V4 AST-Guided text
 * replacement engine.
 * </p>
 */
@Data
@Slf4j
@NoArgsConstructor
@EqualsAndHashCode(callSuper = true)
@Schema(description = "A robust, agent-friendly batch of structural AST modifications for a single Java file.")
public class CodeRefinementBatch extends AbstractTextResourceWrite {

    /**
     * The linear list of structural changes to apply.
     */
    @Schema(description = "The linear list of structural changes to apply.", required = true)
    private List<CodeRefinementIntent> intents = new ArrayList<>();

    /**
     * Whether to optimize imports after applying all changes. Defaults to true.
     */
    @Schema(description = "Whether to optimize imports after applying all changes. Defaults to true.")
    private boolean optimize = true;

    /**
     * Whether to save the file to disk after refinement. Defaults to true.
     */
    @Schema(description = "Whether to save the file to disk after refinement. Defaults to true.")
    private boolean save = true;

    /**
     * List of FQNs to import.
     */
    @Schema(description = "List of FQNs to import.")
    private List<String> importsToAdd = new ArrayList<>();

    /**
     * List of FQNs to remove from imports.
     */
    @Schema(description = "List of FQNs to remove from imports.")
    private List<String> importsToRemove = new ArrayList<>();

    /**
     * The list of line-level comments calculated during the AST transformation process. 
     * These are intended for UI rendering of the changes.
     */
    @JsonIgnore
    @Schema(hidden = true)
    private List<LineComment> calculatedComments = new ArrayList<>();

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: This method executes a multi-stage replay of the modification intents. 
     * For each intent, it creates a transient virtual file in a {@code MemoryFileSystem}, 
     * initializes a {@code JavaSource} context, and applies AST-guided text replacements. 
     * Finally, it performs import management using {@link org.netbeans.api.java.source.GeneratorUtilities} 
     * to ensure the resulting code is semantically sound.
     * </p>
     */
    @Override
    protected String doCalculateResultingContent(Agi agi) throws Exception {
        if (originalContent == null) {
            captureOriginalContent(agi);
        }

        uno.anahata.asi.agi.resource.Resource res = agi.getResourceManager().get(resourceUuid);
        if (res == null) {
            throw new AgiToolException("Resource not found for uuid: " + resourceUuid);
        }

        if (!(res.getHandle() instanceof NbHandle nbh)) {
            throw new AgiToolException("Resource handle is not an IDE-capable NbHandle.");
        }

        FileObject originalFo = nbh.getFileObject();
        log.info("[V4-AST-TEXT] Replaying structural changes on: {}", originalFo.getNameExt());

        ClasspathInfo cpInfo = ClasspathInfo.create(originalFo);
        String currentContent = originalContent;

        int index = 0;
        for (CodeRefinementIntent intent : intents) {
            try {
                FileObject tempFo = FileUtil.createMemoryFileSystem().getRoot().createData("Temp_" + index, "java");
                try (java.io.OutputStream os = tempFo.getOutputStream()) {
                    os.write(currentContent.getBytes("UTF-8"));
                }

                JavaSource js = JavaSource.create(cpInfo, tempFo);
                String[] out = new String[]{currentContent};
                js.runUserActionTask(cc -> {
                    cc.toPhase(JavaSource.Phase.RESOLVED);
                    out[0] = intent.applyToText(cc, out[0]);
                }, true);
                currentContent = out[0];
                index++;
            } catch (Exception e) {
                throw new AgiToolException("Intent #" + index + " failed: " + e.getMessage() + "\n" + intent.toDiagnosticString(), e);
            }
        }

        if ((importsToAdd != null && !importsToAdd.isEmpty()) || (importsToRemove != null && !importsToRemove.isEmpty())) {
            FileObject tempFo = FileUtil.createMemoryFileSystem().getRoot().createData("Temp_Imports", "java");
            try (java.io.OutputStream os = tempFo.getOutputStream()) {
                os.write(currentContent.getBytes("UTF-8"));
            }
            JavaSource js = JavaSource.create(cpInfo, tempFo);
            ModificationResult mr = js.runModificationTask(wc -> {
                wc.toPhase(JavaSource.Phase.RESOLVED);
                CompilationUnitTree cut = wc.getCompilationUnit();
                CompilationUnitTree newCut = cut;

                if (importsToAdd != null && !importsToAdd.isEmpty()) {
                    java.util.Set<javax.lang.model.element.Element> toAdd = new java.util.HashSet<>();
                    for (String imp : importsToAdd) {
                        javax.lang.model.element.TypeElement te = wc.getElements().getTypeElement(imp);
                        if (te != null) {
                            toAdd.add(te);
                        }
                    }
                    if (!toAdd.isEmpty()) {
                        newCut = org.netbeans.api.java.source.GeneratorUtilities.get(wc).addImports(newCut, toAdd);
                    }
                }

                if (importsToRemove != null && !importsToRemove.isEmpty()) {
                    List<com.sun.source.tree.ImportTree> existingImports = new ArrayList<>(newCut.getImports());
                    boolean changed = false;
                    for (String imp : importsToRemove) {
                        for (int i = 0; i < existingImports.size(); i++) {
                            com.sun.source.tree.ImportTree it = existingImports.get(i);
                            String fqn = it.getQualifiedIdentifier().toString();
                            if (fqn.equals(imp)) {
                                existingImports.remove(i);
                                changed = true;
                                break;
                            }
                        }
                    }
                    if (changed) {
                        newCut = wc.getTreeMaker().CompilationUnit(newCut.getPackage(), existingImports, newCut.getTypeDecls(), newCut.getSourceFile());
                    }
                }

                if (newCut != cut) {
                    wc.rewrite(cut, newCut);
                }
            });
            if (mr.getModifiedFileObjects().contains(tempFo)) {
                String finalImports = mr.getResultingSource(tempFo);
                if (finalImports != null) {
                    currentContent = finalImports;
                }
            }
        }

        if (java.util.Objects.equals(originalContent, currentContent)) {
            throw new AgiToolException("Update rejected: AST rewrite produced no changes.");
        }

        List<LineComment> comments = new ArrayList<>();
        this.setCalculatedComments(comments);

        return currentContent;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Performs an early execution of the AST refinement pipeline 
     * to verify that the resulting content is not identical to the current file state. 
     * This prevents redundant disk writes and informs the AI if its proposed changes 
     * had no effect due to selector mismatches.
     * </p>
     */
    @Override
    public void validate(Agi agi) throws Exception {
        super.validate(agi);
        if (resourceUuid == null) {
            throw new AgiToolException("Resource uuid not provided");
        }
        captureOriginalContent(agi);

        if (intents == null || intents.isEmpty()) {
            throw new AgiToolException("Refinement batch must contain at least one intent.");
        }

        if (java.util.Objects.equals(originalContent, calculateResultingContent(agi))) {
            throw new AgiToolException("Update rejected: The resulting content is identical to the current file content on disk.");
        }
    }
}
