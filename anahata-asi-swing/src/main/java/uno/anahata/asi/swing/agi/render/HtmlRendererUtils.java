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
// import java.util.List; // Removed as not directly used for List<Extension>
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
 * Utility class for rendering Markdown to HTML and creating the corresponding
 * Swing JEditorPane components.
 *
 * @author anahata
 */
@UtilityClass
public class HtmlRendererUtils {

    private final Parser markdownParser;
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
     * Creates a JComponent (a JPanel wrapper containing a WrappingEditorPane)
     * that renders the given markdown text as HTML.
     *
     * @param markdown The markdown text to render.
     * @param isThought If true, applies thought-specific styling (italic, gray).
     * @param theme The UI theme to use for colors.
     * @return A JComponent containing the rendered HTML.
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