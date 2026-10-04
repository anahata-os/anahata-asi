/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.agi.render.flexmark;

import com.vladsch.flexmark.html.HtmlWriter;
import com.vladsch.flexmark.html.renderer.NodeRenderer;
import com.vladsch.flexmark.html.renderer.NodeRendererContext;
import com.vladsch.flexmark.html.renderer.NodeRendererFactory;
import com.vladsch.flexmark.html.renderer.NodeRenderingHandler;
import com.vladsch.flexmark.util.data.DataHolder;
import java.util.HashSet;
import java.util.Set;
import org.jetbrains.annotations.NotNull;

/**
 * Node renderer responsible for converting {@link MathArrowNode}s into raw Unicode HTML text.
 * 
 * @author anahata
 */
public class MathArrowNodeRenderer implements NodeRenderer {

    /**
     * Constructs a new {@code MathArrowNodeRenderer}.
     */
    public MathArrowNodeRenderer() {
    }

    /**
     * {@inheritDoc}
     * <p>Registers a handler for {@link MathArrowNode} rendering.</p>
     */
    @NotNull
    @Override
    public Set<NodeRenderingHandler<?>> getNodeRenderingHandlers() {
        Set<NodeRenderingHandler<?>> set = new HashSet<>();
        set.add(new NodeRenderingHandler<>(MathArrowNode.class, this::render));
        return set;
    }

    /**
     * Renders a {@link MathArrowNode} as clean Unicode text into the HTML writer.
     * 
     * @param node The math arrow node.
     * @param context The renderer context.
     * @param html The HTML writer.
     */
    private void render(MathArrowNode node, NodeRendererContext context, HtmlWriter html) {
        html.text(node.getUnicodeSymbol());
    }

    /**
     * Factory class for constructing {@link MathArrowNodeRenderer} instances.
     */
    public static class Factory implements NodeRendererFactory {

        /**
         * Constructs a new {@code Factory}.
         */
        public Factory() {
        }

        /**
         * {@inheritDoc}
         * <p>Instantiates a new {@link MathArrowNodeRenderer}.</p>
         */
        @NotNull
        @Override
        public NodeRenderer apply(@NotNull DataHolder options) {
            return new MathArrowNodeRenderer();
        }
    }
}
