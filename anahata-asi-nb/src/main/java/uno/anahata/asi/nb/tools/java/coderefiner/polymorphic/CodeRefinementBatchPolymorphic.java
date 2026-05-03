/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.tools.java.coderefiner.polymorphic;

import com.sun.source.tree.*;
import io.swagger.v3.oas.annotations.media.Schema;
import java.util.*;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.netbeans.api.java.source.*;
import org.openide.filesystems.FileObject;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.agi.tool.AgiToolException;
import static uno.anahata.asi.nb.tools.java.BatchCodeRefiner.rebuildClassTree;
import uno.anahata.asi.toolkit.resources.text.AbstractTextResourceWrite;

/**
 * A container for a batch of structural Java refinement operations.
 * <p>
 * This class orchestrates multiple {@link CodeRefinementIntentPolymorphic}s targeted at a
 * single source file. It extends {@link AbstractTextResourceWrite} to leverage
 * the platform's high-fidelity diff rendering and resource-locking mechanisms.
 * </p>
 *
 * @author anahata
 */
@Data
@Slf4j
@NoArgsConstructor
@EqualsAndHashCode(callSuper = true)
@Schema(description = "A batch of structural AST modifications for a single Java file.")
public class CodeRefinementBatchPolymorphic extends AbstractTextResourceWrite {

    @Schema(description = "The list of structural changes to apply, in order. Note the intents will be mapped to one of the listed java types", required = true)
    private List<CodeRefinementIntentPolymorphic> intents = new ArrayList<>();

    @Schema(description = "Whether to optimize imports after applying all changes. Defaults to true.")
    private boolean optimize = true;

    @Schema(description = "Whether to save the file to disk after refinement. Defaults to true.")
    private boolean save = true;

    /**
     * {@inheritDoc}
     * <p>
     * Calculates the resulting source code by simulating the AST modifications
     * defined in the batch. This is used by the UI to show a unified
     * side-by-side diff before execution.</p>
     */
    @Override
    protected String doCalculateResultingContent(Agi agi) throws Exception {
        // 1. Authoritative state capture (if not already done by validate)
        if (originalContent == null) {
             captureOriginalContent(agi);
        }
        
        uno.anahata.asi.agi.resource.Resource res = agi.getResourceManager().get(resourceUuid);
        if (res == null) {
            throw new AgiToolException("no resource found for uuid " + resourceUuid);
        }
        
        if (!(res.getHandle() instanceof uno.anahata.asi.nb.resources.handle.NbHandle nbh)) {
            throw new AgiToolException("Resource handle is not a NbHandle " + res.getHandle());
        }

        FileObject realFo = nbh.getFileObject();
        log.info("Calculating resulting content for: {}", realFo);

        // 2. Perform sequential AST replay directly on the real FileObject to preserve Classpath/Project context.
        JavaSource js = JavaSource.forFileObject(realFo);
        ModificationResult mRes = js.runModificationTask(wc -> {
            wc.toPhase(JavaSource.Phase.RESOLVED);
            applyTo(wc);
        });

        // 3. Extract the resulting source string
        String ret = mRes.getResultingSource(realFo);
        return ret;
    }

    /**
     * Authoritatively applies all intents in this batch to the provided working
     * copy using a single-shot atomic rewrite strategy.
     *
     * @param wc The working copy to modify.
     * @throws Exception if any intent application fails.
     */
    public void applyTo(WorkingCopy wc) throws Exception {
        log.info("[V3-STRATEGY] Starting batch application. Intents: {}", intents.size());
        
        // Accumulate changes by parent type to ensure atomic rewrites
        Map<Tree, List<Tree>> modifiedMembers = new LinkedHashMap<>();

        for (CodeRefinementIntentPolymorphic intent : intents) {
            log.info("Processing intent: {}", intent.getClass().getSimpleName());
            intent.apply(wc, modifiedMembers, optimize);
        }

        // Perform single-shot atomic rewrites for each modified container
        TreeMaker make = wc.getTreeMaker();
        for (Map.Entry<Tree, List<Tree>> entry : modifiedMembers.entrySet()) {
            Tree parent = entry.getKey();
            List<Tree> members = entry.getValue();

            if (parent instanceof ClassTree ct) {
                log.info("Commiting rewrite for ClassTree: {} (Members: {})", ct.getSimpleName(), members.size());
                wc.rewrite(ct, rebuildClassTree(make, ct, members));
            } else if (parent instanceof CompilationUnitTree cut) {
                log.info("Commiting rewrite for CompilationUnitTree");
                CompilationUnitTree updated = make.CompilationUnit(cut.getPackage(), cut.getImports(), (List<? extends Tree>) members, cut.getSourceFile());
                wc.rewrite(cut, updated);
            }
        }
    }

    /**
     * {@inheritDoc}
     * <p>
     * Validates that the resource is in context and is a valid Java source.</p>
     */
    @Override
    public void validate(Agi agi) throws Exception {
        validateStructuralState(agi);
        if (intents == null || intents.isEmpty()) {
            throw new AgiToolException("Refinement batch must contain at least one intent.");
        }
        validateIdenticalContent(agi);
    }
}
