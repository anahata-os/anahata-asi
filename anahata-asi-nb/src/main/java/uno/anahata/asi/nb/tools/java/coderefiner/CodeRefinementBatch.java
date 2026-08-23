/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.tools.java.coderefiner;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.sun.source.tree.CompilationUnitTree;
import com.sun.source.tree.MemberSelectTree;
import com.sun.source.tree.MethodInvocationTree;
import com.sun.source.tree.Tree;
import com.sun.source.util.SourcePositions;
import com.sun.source.util.TreePath;
import com.sun.source.util.TreePathScanner;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import javax.lang.model.element.Element;
import javax.lang.model.element.PackageElement;
import javax.lang.model.element.TypeElement;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.netbeans.api.java.source.*;
import io.swagger.v3.oas.annotations.media.Schema;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import org.openide.loaders.DataObject;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.agi.resource.Resource;
import uno.anahata.asi.agi.tool.AgiToolException;
import uno.anahata.asi.toolkit.resources.text.AbstractTextResourceWrite;
import uno.anahata.asi.toolkit.resources.text.LineComment;
import uno.anahata.asi.nb.resources.handle.NbHandle;
import uno.anahata.asi.nb.tools.java.BatchCodeRefiner;
import uno.anahata.asi.nb.tools.java.JavaSourceUtils;
import uno.anahata.asi.nb.tools.java.CodeRefiner;

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
     * The NetBeans formatting mode to apply after refinement. Defaults to SELECTED_RANGES.
     */
    @Schema(description = "The NetBeans formatting mode to apply after refinement. Defaults to SELECTED_RANGES.")
    private FormatMode format = FormatMode.SELECTED_RANGES;
    /**
     * The list of line-level comments calculated during the AST transformation
     * process. These are intended for UI rendering of the changes.
     */
    @JsonIgnore
    @Schema(hidden = true)
    private List<LineComment> calculatedComments = new ArrayList<>();

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: This method executes a multi-stage replay of the
     * modification intents. For each intent, it creates a transient virtual
     * file in a {@code MemoryFileSystem}, initializes a {@code JavaSource}
     * context, and applies AST-guided text replacements. Finally, it performs
     * import management using
     * {@link org.netbeans.api.java.source.GeneratorUtilities} to ensure the
     * resulting code is semantically sound.
     * </p>
     */
    @Override protected String doCalculateResultingContent(Agi agi) throws Exception {
        if (originalContent == null) {
            captureOriginalContent(agi);
        }

        Resource res = agi.getResourceManager().get(resourceUuid);
        if (res == null) {
            throw new AgiToolException("Resource not found for uuid: " + resourceUuid);
        }

        if (!(res.getHandle() instanceof NbHandle nbh)) {
            throw new AgiToolException("Resource handle is not an IDE-capable NbHandle.");
        }

        FileObject originalFo = nbh.getFileObject();
        log.info("[V4-AST-TEXT] Replaying structural changes on: {}", originalFo.getNameExt());

        ClasspathInfo cpInfo = ClasspathInfo.create(originalFo);
        // CRITICAL FIX: Normalize CRLF to LF to prevent AST SourcePositions drift
        String currentContent = originalContent.replace("\r\n", "\n");

        List<int[]> modifiedRanges = new ArrayList<>();

        int index = 0;
        for (CodeRefinementIntent intent : intents) {
            try {
                FileObject tempFo = FileUtil.createMemoryFileSystem().getRoot().createData("Temp_" + index, "java");
                try (OutputStream os = tempFo.getOutputStream()) {
                    os.write(currentContent.getBytes("UTF-8"));
                }

                JavaSource js = JavaSource.create(cpInfo, tempFo);
                String[] out = new String[]{currentContent};
                js.runUserActionTask(cc -> {
                    cc.toPhase(JavaSource.Phase.RESOLVED);
                    out[0] = intent.applyToText(cc, out[0], modifiedRanges);
                }, true);
                currentContent = out[0];
                index++;
            } catch (Exception e) {
                throw new AgiToolException("Intent #" + index + " failed: " + e.getMessage() + "\n" + intent.toDiagnosticString(), e);
            }
        }

        if (this.optimize && modifiedRanges != null && !modifiedRanges.isEmpty()) {
            currentContent = shortenFqnsInModifiedRanges(cpInfo, currentContent, modifiedRanges, this.importsToAdd);
        }

        boolean hasExplicitImports = (importsToAdd != null && !importsToAdd.isEmpty()) || (importsToRemove != null && !importsToRemove.isEmpty());
        if (this.optimize || hasExplicitImports) {
            currentContent = CodeRefiner.optimizeImportsInMemory(cpInfo, currentContent, this.optimize, importsToAdd, importsToRemove);
        }

        currentContent = JavaSourceUtils.reformat(originalFo, currentContent, this.format, modifiedRanges);

        if (Objects.equals(originalContent, currentContent)) {
            throw new AgiToolException("Update rejected: AST rewrite produced no changes.");
        }

        List<LineComment> comments = new ArrayList<>();
        this.setCalculatedComments(comments);

        return currentContent;
    }

    /**
     * Shortens fully qualified type names in modified ranges to simple names using
     * real project ClasspathInfo in RAM, collecting all discovered FQNs for the import header.
     *
     * @param cpInfo The project ClasspathInfo.
     * @param content The current source code content.
     * @param modifiedRanges The list of [start, end] character ranges of modified regions.
     * @param importsToAddCollector Mutable list to collect discovered FQNs.
     * @return The updated source code content with simple names.
     */
    private static String shortenFqnsInModifiedRanges(ClasspathInfo cpInfo, String content, List<int[]> modifiedRanges, List<String> importsToAddCollector) {
        FileObject tempFo = null;
        DataObject tempDobj = null;
        try {
            tempFo = FileUtil.createMemoryFileSystem().getRoot().createData("Temp_FqnShorten_" + System.nanoTime(), "java");
            try (OutputStream os = tempFo.getOutputStream()) {
                os.write(content.getBytes(StandardCharsets.UTF_8));
            }

            tempDobj = DataObject.find(tempFo);
            JavaSource js = (cpInfo != null) ? JavaSource.create(cpInfo, tempFo) : JavaSource.forFileObject(tempFo);

            record Replacement(int start, int end, String simpleName) {}
            List<Replacement> replacements = new ArrayList<>();

            js.runUserActionTask(cc -> {
                cc.toPhase(JavaSource.Phase.RESOLVED);
                CompilationUnitTree cut = cc.getCompilationUnit();
                SourcePositions sp = cc.getTrees().getSourcePositions();

                new TreePathScanner<Void, Void>() {
                    @Override
                    public Void visitMemberSelect(MemberSelectTree node, Void p) {
                        long start = sp.getStartPosition(cut, node);
                        long end = sp.getEndPosition(cut, node);
                        if (start >= 0 && end > start) {
                            boolean inRange = false;
                            for (int[] r : modifiedRanges) {
                                if (start >= r[0] && end <= r[1]) {
                                    inRange = true;
                                    break;
                                }
                            }
                            if (inRange) {
                                TreePath path = getCurrentPath();
                                Element e = cc.getTrees().getElement(path);
                                if (e instanceof TypeElement te) {
                                    String fqn = te.getQualifiedName().toString();
                                    String rawText = content.substring((int) start, (int) end).replaceAll("\\s+", "");
                                    if (rawText.equals(fqn)) {
                                        PackageElement pkg = cc.getElements().getPackageOf(te);
                                        String pkgName = (pkg != null) ? pkg.getQualifiedName().toString() : "";
                                        if (!"java.lang".equals(pkgName)) {
                                            if (importsToAddCollector != null && !importsToAddCollector.contains(fqn)) {
                                                importsToAddCollector.add(fqn);
                                            }
                                        }
                                        replacements.add(new Replacement((int) start, (int) end, te.getSimpleName().toString()));
                                    }
                                }
                            }
                        }
                        return super.visitMemberSelect(node, p);
                    }
                }.scan(new TreePath(cut), null);
            }, true);

            if (!replacements.isEmpty()) {
                replacements.sort((a, b) -> Integer.compare(b.start, a.start));
                List<Replacement> nonOverlapping = new ArrayList<>();
                int lastStart = Integer.MAX_VALUE;
                for (Replacement r : replacements) {
                    if (r.end <= lastStart) {
                        nonOverlapping.add(r);
                        lastStart = r.start;
                    }
                }

                StringBuilder updated = new StringBuilder(content);
                for (Replacement r : nonOverlapping) {
                    updated.replace(r.start, r.end, r.simpleName);
                }
                return updated.toString();
            }
        } catch (Exception e) {
            log.warn("Failed to shorten FQNs in modified ranges: {}", e.getMessage(), e);
        } finally {
            if (tempDobj != null) {
                tempDobj.setModified(false);
            }
            if (tempFo != null && tempFo.isValid()) {
                try {
                    tempFo.delete();
                } catch (IOException e) {
                    // Ignore
                }
            }
        }
        return content;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Performs an early execution of the AST refinement
     * pipeline to verify that the resulting content is not identical to the
     * current file state. This prevents redundant disk writes and informs the
     * AI if its proposed changes had no effect due to selector mismatches.
     * </p>
     */
    @Override
    public void validate(Agi agi) throws Exception {
        super.validate(agi);
        if (resourceUuid == null) {
            throw new AgiToolException("Resource uuid not provided");
        }
        captureOriginalContent(agi);

        if (intents != null) {
            int idx = 0;
            for (CodeRefinementIntent intent : intents) {
                try {
                    intent.validate();
                } catch (Exception e) {
                    throw new AgiToolException("Intent #" + idx + " validation failed: " + e.getMessage() + "\n" + intent.toDiagnosticString(), e);
                }
                idx++;
            }
        }

        boolean hasIntents = intents != null && !intents.isEmpty();
        boolean hasImportsToAdd = importsToAdd != null && !importsToAdd.isEmpty();
        boolean hasImportsToRemove = importsToRemove != null && !importsToRemove.isEmpty();

        if (!hasIntents && !hasImportsToAdd && !hasImportsToRemove) {
            throw new AgiToolException("Refinement batch is empty. "
                    + "You must provide at least one structural member intent "
                    + "or an import modification (importsToAdd/importsToRemove).");
        }

        if (Objects.equals(originalContent, calculateResultingContent(agi))) {
            throw new AgiToolException("Update rejected: The resulting content is identical to the current file content on disk.");
        }

    }
}
