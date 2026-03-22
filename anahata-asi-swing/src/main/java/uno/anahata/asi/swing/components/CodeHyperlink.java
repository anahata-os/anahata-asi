/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.components;

import java.awt.Color;
import java.awt.Cursor;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.function.Supplier;
import javax.swing.JLabel;
import lombok.Getter;
import lombok.NonNull;
import lombok.Setter;
import uno.anahata.asi.internal.JacksonUtils;
import uno.anahata.asi.swing.internal.SwingUtils;

/**
 * A specialized JLabel that acts as a clickable hyperlink for displaying
 * code or JSON content in a popup dialog.
 * 
 * @author anahata
 */
@Getter @Setter
public class CodeHyperlink extends JLabel {

    /** 
     * A supplier for the title of the popup dialog. 
     * <p>
     * This title is displayed at the top of the dialog window when the hyperlink 
     * is clicked. It can be dynamically generated based on the current state.
     * </p>
     */
    private Supplier<String> titleSupplier;

    /** 
     * A supplier for the content to display, allowing for lazy loading and formatting. 
     * <p>
     * The content is fetched only when the user clicks the hyperlink. This is 
     * ideal for large JSON payloads or code blocks that require expensive processing.
     * </p>
     */
    private Supplier<String> contentSupplier;

    /** 
     * The language for syntax highlighting in the popup dialog. 
     * <p>
     * Supports standard RSyntaxTextArea constants like "json", "java", "xml", etc. 
     * If set to "json", the content will be automatically pretty-printed.
     * </p>
     */
    private String language;

    /**
     * Constructs a new CodeHyperlink with default text language.
     * 
     * @param labelText The text to display for the hyperlink.
     * @param titleSupplier The supplier for the dialog title.
     * @param contentSupplier The supplier for the content.
     */
    public CodeHyperlink(@NonNull String labelText, @NonNull Supplier<String> titleSupplier, @NonNull Supplier<String> contentSupplier) {
        this(labelText, titleSupplier, contentSupplier, "text");
    }

    /**
     * Constructs a new CodeHyperlink with a specific language for syntax highlighting.
     * 
     * @param labelText The text to display for the hyperlink.
     * @param titleSupplier The supplier for the dialog title.
     * @param contentSupplier The supplier for the content.
     * @param language The language for syntax highlighting (e.g., "json", "java").
     */
    public CodeHyperlink(@NonNull String labelText, @NonNull Supplier<String> titleSupplier, @NonNull Supplier<String> contentSupplier, String language) {
        super("<html><u>" + labelText + "</u></html>");
        this.titleSupplier = titleSupplier;
        this.contentSupplier = contentSupplier;
        this.language = language;
        
        setForeground(Color.BLUE);
        setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
        
        addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                if (CodeHyperlink.this.contentSupplier != null) {
                    String content = CodeHyperlink.this.contentSupplier.get();
                    String title = CodeHyperlink.this.titleSupplier != null ? CodeHyperlink.this.titleSupplier.get() : "Content";
                    
                    if (content != null && !content.isEmpty()) {
                        // Automatically pretty-print if it's JSON
                        if ("json".equalsIgnoreCase(CodeHyperlink.this.language)) {
                            String pretty = JacksonUtils.prettyPrintJsonString(content);
                            // Fallback to original content if pretty-printing fails (e.g. it's already an error message)
                            if (!pretty.startsWith("Error:")) {
                                content = pretty;
                            }
                        }
                        SwingUtils.showCodeBlockDialog(CodeHyperlink.this, title, content, CodeHyperlink.this.language);
                    }
                }
            }
        });
    }
}
