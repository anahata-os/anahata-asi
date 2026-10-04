/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.agi.render.flexmark;

import com.vladsch.flexmark.util.ast.DoNotDecorate;
import com.vladsch.flexmark.util.ast.Node;
import com.vladsch.flexmark.util.sequence.BasedSequence;
import org.jetbrains.annotations.NotNull;

/**
 * AST node representing an inlined mathematical symbol or directional arrow.
 * <p>
 * This node stores the underlying Unicode representation of the symbol while
 * preserving the original markdown source coordinates. It implements
 * {@link DoNotDecorate} to prevent subsequent AST post-processors from modifying
 * its contents.
 * </p>
 * 
 * @author anahata
 */
public class MathArrowNode extends Node implements DoNotDecorate {

    /**
     * The rendered Unicode character or string representing the symbol.
     */
    private final String unicodeSymbol;

    /**
     * Constructs a new {@code MathArrowNode}.
     * 
     * @param chars The source sequence characters representing this node.
     * @param unicodeSymbol The resolved Unicode symbol string.
     */
    public MathArrowNode(BasedSequence chars, String unicodeSymbol) {
        super(chars);
        this.unicodeSymbol = unicodeSymbol;
    }

    /**
     * Returns the resolved Unicode symbol for HTML rendering.
     * 
     * @return The Unicode symbol string.
     */
    public String getUnicodeSymbol() {
        return unicodeSymbol;
    }

    /**
     * {@inheritDoc}
     * <p>Returns an empty segment array since this node has no custom inner delimiters.</p>
     */
    @NotNull
    @Override
    public BasedSequence[] getSegments() {
        return EMPTY_SEGMENTS;
    }
}
