/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.agi.render;

import com.vladsch.flexmark.html.HtmlRenderer;
import com.vladsch.flexmark.parser.Parser;
import com.vladsch.flexmark.util.ast.Node;
import com.vladsch.flexmark.util.data.MutableDataSet;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.util.Arrays;
import javax.swing.JComponent;
import javax.swing.JEditorPane;
import javax.swing.JPanel;
import javax.swing.text.html.HTMLEditorKit;
import javax.swing.text.html.StyleSheet;
import lombok.experimental.UtilityClass;
import uno.anahata.asi.swing.agi.SwingAgiConfig.UITheme;
import uno.anahata.asi.swing.components.WrappingEditorPane;
import com.vladsch.flexmark.ext.tables.TablesExtension; // Added import
import com.vladsch.flexmark.util.misc.Extension; // Corrected import for Extension

/**
 * Centralized utility for rendering Markdown and HTML content within the ASI Swing UI.
 * <p>
 * This class leverages the Flexmark parser to transform agentic output into high-fidelity 
 * Swing components. It ensures visual consistency across all conversation parts by 
 * managing CSS rules, line-wrapping constraints, and theme-aware color mapping.
 * </p>
 * 
 * @author anahata
 */
@UtilityClass
public class HtmlRendererUtils {

    /** The Flexmark parser used to transform Markdown into an Abstract Syntax Tree (AST). */
    private final Parser markdownParser;
    /** The Flexmark renderer used to transform the AST into HTML. */
    private final HtmlRenderer htmlRenderer;

    static {
        MutableDataSet options = new MutableDataSet();
        options.set(HtmlRenderer.SOFT_BREAK, "<br />");
        
        // Correct way to add extensions as seen in TextPartRenderer (v1)
        options.set(Parser.EXTENSIONS, Arrays.asList(TablesExtension.create()));

        markdownParser = Parser.builder(options).build(); // Pass options to builder
        htmlRenderer = HtmlRenderer.builder(options).build(); // Pass options to builder
    }

    /**
     * Orchestrates the creation of a theme-aware HTML rendering component.
     * <p>
     * This method parses raw markdown, applies a "Diff-and-Update" friendly word-wrapping 
     * strategy via {@link WrappingEditorPane}, and injects specialized CSS rules for 
     * model thoughts and data tables. The resulting component is wrapped in a 
     * BorderLayout JPanel to force strict width adherence.
     * </p>
     *
     * @param markdown The raw markdown content from the agi session.
     * @param isThought Whether to apply italicized, low-contrast "thought" styling.
     * @param theme The current UI theme for coordinate-accurate color rendering.
     * @return A ready-to-display JComponent containing the rendered content.
     */
    public static JComponent createHtmlPane(String markdown, boolean isThought, UITheme theme) {
        // Parse markdown to HTML
        Node document = markdownParser.parse(markdown);
        String html = htmlRenderer.render(document);

        // Use the V2 WrappingEditorPane for correct line wrapping
        JEditorPane editorPane = new WrappingEditorPane();
        editorPane.setEditable(false);
        editorPane.setContentType("text/html");
        editorPane.setOpaque(false);

        HTMLEditorKit kit = new HTMLEditorKit();
        editorPane.setEditorKit(kit);

        // Apply custom CSS for styling and word wrapping
        StyleSheet sheet = kit.getStyleSheet();

        String fontStyle = isThought ? "font-style: italic; color: #888888;" : "color: " + toHtmlColor(theme.getFontColor()) + ";";
        String fontWeight = isThought ? "font-weight: normal;" : "font-weight: normal;";

        sheet.addRule("body { word-wrap: break-word; font-family: sans-serif; font-size: 14px; background-color: transparent; " + fontStyle + fontWeight + "}");
        sheet.addRule("table { border-collapse: collapse; width: 100%; }");
        sheet.addRule("th, td { border: 1px solid #dddddd; text-align: left; padding: 8px; }");
        sheet.addRule("th { background-color: #f2f2f2; }");

        editorPane.setText("<html><body>" + html + "</body></html>");
        editorPane.setAlignmentX(Component.LEFT_ALIGNMENT);

        // Wrap the editor pane in a JPanel with BorderLayout to force width constraint
        JPanel wrapper = new JPanel(new BorderLayout());
        wrapper.setOpaque(false);
        wrapper.add(editorPane, BorderLayout.CENTER);
        return wrapper;
    }

    /**
     * Converts a Java Color object to its hexadecimal HTML color string.
     * @param color The Java Color.
     * @return The HTML color string (e.g., "#FF0000").
     */
    public static String toHtmlColor(Color color) {
        return String.format("#%02x%02x%02x", color.getRed(), color.getGreen(), color.getBlue());
    }
}