/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.ai.swing.chat.render;

import com.vladsch.flexmark.ext.media.tags.MediaTagsExtension;
import com.vladsch.flexmark.ext.tables.TablesExtension;
import com.vladsch.flexmark.ext.youtube.embedded.YouTubeLinkExtension;
import com.vladsch.flexmark.html.HtmlRenderer;
import com.vladsch.flexmark.parser.Parser;
import com.vladsch.flexmark.util.ast.Node;
import com.vladsch.flexmark.util.data.MutableDataSet;
import java.awt.Component;
import java.util.Arrays;
import javax.swing.BorderFactory;
import javax.swing.JComponent;
import javax.swing.JScrollPane;
import javax.swing.text.html.HTMLEditorKit;
import javax.swing.text.html.StyleSheet;
import uno.anahata.ai.swing.chat.ChatPanel;
import uno.anahata.ai.swing.chat.SwingChatConfig.UITheme;
import uno.anahata.ai.swing.components.WrappingEditorPane;
import uno.anahata.ai.swing.internal.SwingUtils;

/**
 * Renders a markdown text segment into a {@link WrappingEditorPane}.
 * This class extends {@link AbstractTextSegmentRenderer} and handles the
 * conversion of markdown to HTML and applying appropriate styling.
 *
 * @author anahata
 */
public class MarkupTextSegmentRenderer extends AbstractTextSegmentRenderer {

    /** The Flexmark markdown parser. */
    private final Parser markdownParser;
    /** The Flexmark HTML renderer. */
    private final HtmlRenderer htmlRenderer;
    /** Whether this segment represents a model thought. */
    private final boolean isThought;
    
    /** The inner text component. */
    private WrappingEditorPane innerComponent;

    /**
     * Constructs a new MarkupTextSegmentRenderer.
     *
     * @param chatPanel The chat panel instance.
     * @param initialContent The initial markdown content for this segment.
     * @param isThought True if the text represents a model thought, false otherwise.
     */
    public MarkupTextSegmentRenderer(ChatPanel chatPanel, String initialContent, boolean isThought) {
        super(chatPanel, initialContent);
        MutableDataSet options = new MutableDataSet();
        options.set(Parser.EXTENSIONS, Arrays.asList(
                TablesExtension.create(),
                MediaTagsExtension.create(),
                YouTubeLinkExtension.create()
        ));
        options.set(HtmlRenderer.SOFT_BREAK, "<br />");
        this.markdownParser = Parser.builder(options).build();
        this.htmlRenderer = HtmlRenderer.builder(options).build();
        this.isThought = isThought;
    }

    /**
     * {@inheritDoc}
     * It reuses the existing {@link WrappingEditorPane} if available and updates its content
     * only if the markdown text has changed.
     */
    @Override
    public boolean render() {
        boolean changed = hasContentChanged();

        if (component == null) {
            // Initial render: create the WrappingEditorPane
            innerComponent = new WrappingEditorPane();
            innerComponent.setEditable(false);
            innerComponent.setContentType("text/html");
            innerComponent.setOpaque(false);

            HTMLEditorKit kit = new HTMLEditorKit();
            innerComponent.setEditorKit(kit);

            // Apply custom CSS for styling and word wrapping
            StyleSheet sheet = kit.getStyleSheet();
            sheet.addRule("body { word-wrap: break-word; font-family: sans-serif; font-size: 14px; background-color: transparent;}");
            sheet.addRule("table { border-collapse: collapse; width: 100%; }");
            sheet.addRule("th, td { border: 1px solid #dddddd; text-align: left; padding: 8px; }");
            sheet.addRule("th { background-color: #f2f2f2; }");

            // Wrap in a scroll pane for horizontal scrolling of giant lines
            JScrollPane scrollPane = new JScrollPane(innerComponent);
            scrollPane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_NEVER);
            scrollPane.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
            scrollPane.setBorder(BorderFactory.createEmptyBorder());
            scrollPane.setOpaque(false);
            scrollPane.getViewport().setOpaque(false);
            
            // Redispatch mouse wheel events to the parent scroll pane
            scrollPane.addMouseWheelListener(e -> SwingUtils.redispatchMouseWheelEvent(scrollPane, e));
            
            this.component = scrollPane;
            changed = true;
        }

        // Update content only if it has changed
        if (changed) {
            UITheme theme = chatConfig.getTheme();
            Node document = markdownParser.parse(currentContent);
            String html = htmlRenderer.render(document);

            String color = isThought ? SwingUtils.toHtmlColor(theme.getThoughtFg()) : SwingUtils.toHtmlColor(theme.getFontColor());
            String fontStyle = isThought ? "italic" : "normal";

            // Wrap the content in a styled div to ensure the style is applied correctly
            String styledHtml = String.format(
                "<html><body><div style='color: %s; font-style: %s;'>%s</div></body></html>",
                color, fontStyle, html
            );

            innerComponent.setText(styledHtml);
            contentRendered(); // Mark content as rendered
        }

        return changed;
    }

    /**
     * {@inheritDoc}
     * A {@code MarkupTextSegmentRenderer} handles {@link TextSegmentType#TEXT} descriptors.
     */
    @Override
    public boolean matches(TextSegmentDescriptor descriptor) {
        return descriptor.type() == TextSegmentType.TEXT;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public JComponent getComponent() {
        return component;
    }
}
