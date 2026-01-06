/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.ai.swing.components;

import java.awt.Color;
import java.awt.Cursor;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.function.Supplier;
import javax.swing.JLabel;
import lombok.Getter;
import lombok.NonNull;
import lombok.Setter;
import uno.anahata.ai.internal.JacksonUtils;
import uno.anahata.ai.swing.internal.SwingUtils;

/**
 * A specialized JLabel that acts as a clickable hyperlink for displaying
 * code or JSON content in a popup dialog.
 * 
 * @author gemini-3-flash-preview
 */
@Getter @Setter
public class CodeHyperlink extends JLabel {

    /** A supplier for the title of the popup dialog. */
    private Supplier<String> titleSupplier;
    /** A supplier for the content to display, allowing for lazy loading/formatting. */
    private Supplier<String> contentSupplier;
    /** The language for syntax highlighting in the popup dialog. */
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
                            content = JacksonUtils.prettyPrintJsonString(content);
                        }
                        SwingUtils.showCodeBlockDialog(CodeHyperlink.this, title, content, CodeHyperlink.this.language);
                    }
                }
            }
        });
    }
}
