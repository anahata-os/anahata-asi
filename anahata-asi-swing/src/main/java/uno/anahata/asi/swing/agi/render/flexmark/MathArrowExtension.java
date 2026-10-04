/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.agi.render.flexmark;

import com.vladsch.flexmark.html.HtmlRenderer;
import com.vladsch.flexmark.parser.Parser;
import com.vladsch.flexmark.util.data.MutableDataHolder;
import org.jetbrains.annotations.NotNull;

/**
 * Flexmark extension that enables zero-JavaScript rendering of LaTeX math arrows and symbols in Swing.
 * <p>
 * This extension registers {@link MathArrowPostProcessor} to parse LaTeX commands (such as {@code $\to$},
 * {@code $\rightarrow$}, {@code $\Rightarrow$}, and {@code $\leftrightarrow$}) into {@link MathArrowNode}s,
 * and {@link MathArrowNodeRenderer} to render them as standard Unicode characters in Swing HTML.
 * </p>
 * 
 * @author anahata
 */
public class MathArrowExtension implements Parser.ParserExtension, HtmlRenderer.HtmlRendererExtension {

    /**
     * Private constructor for singleton creation pattern.
     */
    private MathArrowExtension() {
    }

    /**
     * Creates a new instance of the {@code MathArrowExtension}.
     * 
     * @return A configured {@link MathArrowExtension} instance.
     */
    public static MathArrowExtension create() {
        return new MathArrowExtension();
    }

    /**
     * {@inheritDoc}
     * <p>Registers the {@link MathArrowPostProcessor.Factory} with the Flexmark parser builder.</p>
     */
    @Override
    public void extend(Parser.Builder parserBuilder) {
        parserBuilder.postProcessorFactory(new MathArrowPostProcessor.Factory());
    }

    /**
     * {@inheritDoc}
     * <p>Registers the {@link MathArrowNodeRenderer.Factory} with the Flexmark HTML renderer builder.</p>
     */
    @Override
    public void extend(@NotNull HtmlRenderer.Builder htmlRendererBuilder, @NotNull String rendererType) {
        if (htmlRendererBuilder.isRendererType("HTML")) {
            htmlRendererBuilder.nodeRendererFactory(new MathArrowNodeRenderer.Factory());
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void parserOptions(MutableDataHolder options) {
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void rendererOptions(@NotNull MutableDataHolder options) {
    }
}
