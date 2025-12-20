/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.ai.swing.components;

import java.awt.Color;
import java.awt.Cursor;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import javax.swing.JLabel;
import lombok.Getter;
import lombok.NonNull;
import lombok.Setter;
import uno.anahata.ai.swing.internal.SwingUtils;

/**
 * A specialized JLabel that acts as a clickable hyperlink for displaying
 * code or JSON content in a popup dialog.
 * 
 * @author gemini-3-flash-preview
 */
@Getter @Setter
public class CodeHyperlink extends JLabel {

    /** The title of the popup dialog. */
    private String title;
    /** The content to display in the popup dialog. */
    private String content;
    /** The language for syntax highlighting in the popup dialog. */
    private String language;

    /**
     * Constructs a new CodeHyperlink with default text language.
     * 
     * @param labelText The text to display for the hyperlink.
     * @param title The title of the popup dialog.
     * @param content The initial content to display.
     */
    public CodeHyperlink(@NonNull String labelText, @NonNull String title, @NonNull String content) {
        this(labelText, title, content, "text");
    }

    /**
     * Constructs a new CodeHyperlink with a specific language for syntax highlighting.
     * 
     * @param labelText The text to display for the hyperlink.
     * @param title The title of the popup dialog.
     * @param content The initial content to display.
     * @param language The language for syntax highlighting (e.g., "json", "java").
     */
    public CodeHyperlink(@NonNull String labelText, @NonNull String title, @NonNull String content, String language) {
        super("<html><u>" + labelText + "</u></html>");
        this.title = title;
        this.content = content;
        this.language = language;
        
        setForeground(Color.BLUE);
        setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
        setToolTipText("Click to view " + title);
        
        addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                if (CodeHyperlink.this.content != null && !CodeHyperlink.this.content.isEmpty()) {
                    SwingUtils.showCodeBlockDialog(CodeHyperlink.this, CodeHyperlink.this.title, CodeHyperlink.this.content, CodeHyperlink.this.language);
                }
            }
        });
    }
}
