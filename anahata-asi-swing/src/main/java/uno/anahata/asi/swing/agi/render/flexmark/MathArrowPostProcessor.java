/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.agi.render.flexmark;

import com.vladsch.flexmark.ast.Text;
import com.vladsch.flexmark.ast.TextBase;
import com.vladsch.flexmark.parser.block.NodePostProcessor;
import com.vladsch.flexmark.parser.block.NodePostProcessorFactory;
import com.vladsch.flexmark.util.ast.DoNotDecorate;
import com.vladsch.flexmark.util.ast.Document;
import com.vladsch.flexmark.util.ast.Node;
import com.vladsch.flexmark.util.ast.NodeTracker;
import com.vladsch.flexmark.util.sequence.BasedSequence;
import java.util.HashMap;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.jetbrains.annotations.NotNull;

/**
 * AST post-processor that transforms LaTeX mathematical arrow expressions into {@link MathArrowNode}s.
 * <p>
 * This processor scans plain text nodes for LaTeX math sequences (e.g., {@code $\to$}, {@code $\rightarrow$},
 * {@code $\Rightarrow$}, {@code $\leftrightarrow$}, or unadorned {@code \to}) and translates recognized
 * commands into corresponding Unicode characters.
 * </p>
 * 
 * @author anahata
 */
public class MathArrowPostProcessor extends NodePostProcessor {

    /**
     * Map of supported LaTeX commands to their Unicode character equivalents.
     */
    private static final Map<String, String> SYMBOLS = new HashMap<>();

    static {
        // Directional Arrows
        SYMBOLS.put("to", "→");
        SYMBOLS.put("rightarrow", "→");
        SYMBOLS.put("Rightarrow", "⇒");
        SYMBOLS.put("leftarrow", "←");
        SYMBOLS.put("Leftarrow", "⇐");
        SYMBOLS.put("leftrightarrow", "↔");
        SYMBOLS.put("Leftrightarrow", "⇔");
        SYMBOLS.put("uparrow", "↑");
        SYMBOLS.put("Uparrow", "⇑");
        SYMBOLS.put("downarrow", "↓");
        SYMBOLS.put("Downarrow", "⇓");
        SYMBOLS.put("updownarrow", "↕");
        SYMBOLS.put("Updownarrow", "⇕");
        SYMBOLS.put("mapsto", "↦");
        SYMBOLS.put("implies", "⇒");
        SYMBOLS.put("iff", "⇔");
        SYMBOLS.put("gets", "←");
        SYMBOLS.put("rArr", "⇒");
        SYMBOLS.put("lArr", "⇐");
        SYMBOLS.put("hArr", "⇔");

        // Mathematical & Logical Symbols
        SYMBOLS.put("dots", "…");
        SYMBOLS.put("cdots", "⋯");
        SYMBOLS.put("ldots", "…");
        SYMBOLS.put("approx", "≈");
        SYMBOLS.put("neq", "≠");
        SYMBOLS.put("ne", "≠");
        SYMBOLS.put("leq", "≤");
        SYMBOLS.put("le", "≤");
        SYMBOLS.put("geq", "≥");
        SYMBOLS.put("ge", "≥");
        SYMBOLS.put("times", "×");
        SYMBOLS.put("pm", "±");
        SYMBOLS.put("mp", "∓");
        SYMBOLS.put("infty", "∞");
        SYMBOLS.put("forall", "∀");
        SYMBOLS.put("exists", "∃");
        SYMBOLS.put("in", "∈");
        SYMBOLS.put("notin", "∉");
    }

    /**
     * Regex matching LaTeX math expressions enclosed in dollars, GitLab backticks, or standalone backslash commands.
     */
    private static final Pattern PATTERN = Pattern.compile(
            "(?:\\$`?\\s*\\\\([a-zA-Z]+)\\s*`?\\$)|(?:\\\\([a-zA-Z]+)\\b)"
    );

    /**
     * Constructs a new {@code MathArrowPostProcessor}.
     */
    public MathArrowPostProcessor() {
    }

    /**
     * {@inheritDoc}
     * <p>Inspects the target text node for LaTeX math commands and replaces matches with {@link MathArrowNode}s.</p>
     */
    @Override
    public void process(@NotNull NodeTracker state, @NotNull Node node) {
        BasedSequence chars = node.getChars();
        String text = chars.toString();
        Matcher matcher = PATTERN.matcher(text);

        int lastEnd = 0;
        TextBase textBase = null;

        while (matcher.find()) {
            String cmd = matcher.group(1);
            if (cmd == null) {
                cmd = matcher.group(2);
            }
            String symbol = SYMBOLS.get(cmd);
            if (symbol == null) {
                continue;
            }

            if (textBase == null) {
                textBase = new TextBase(chars);
                node.insertBefore(textBase);
                state.nodeAdded(textBase);
            }

            int matchStart = matcher.start();
            int matchEnd = matcher.end();

            if (matchStart > lastEnd) {
                Text prefix = new Text(chars.subSequence(lastEnd, matchStart));
                textBase.appendChild(prefix);
                state.nodeAdded(prefix);
            }

            MathArrowNode arrowNode = new MathArrowNode(chars.subSequence(matchStart, matchEnd), symbol);
            textBase.appendChild(arrowNode);
            state.nodeAdded(arrowNode);

            lastEnd = matchEnd;
        }

        if (textBase != null) {
            if (lastEnd < chars.length()) {
                Text suffix = new Text(chars.subSequence(lastEnd, chars.length()));
                textBase.appendChild(suffix);
                state.nodeAdded(suffix);
            }
            node.unlink();
            state.nodeRemoved(node);
        }
    }

    /**
     * Factory class responsible for instantiating {@link MathArrowPostProcessor} instances.
     */
    public static class Factory extends NodePostProcessorFactory {

        /**
         * Constructs a new {@code Factory}, configuring exclusion of {@link DoNotDecorate} nodes.
         */
        public Factory() {
            super(false);
            addNodeWithExclusions(Text.class, DoNotDecorate.class);
        }

        /**
         * {@inheritDoc}
         * <p>Creates a new {@link MathArrowPostProcessor} for the document.</p>
         */
        @NotNull
        @Override
        public NodePostProcessor apply(@NotNull Document document) {
            return new MathArrowPostProcessor();
        }
    }
}
