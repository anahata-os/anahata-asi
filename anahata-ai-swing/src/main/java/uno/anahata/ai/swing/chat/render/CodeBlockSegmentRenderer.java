/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.ai.swing.chat.render;

import java.awt.BorderLayout;
import java.awt.Dialog;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Window;
import java.util.Objects;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JEditorPane;
import javax.swing.JFrame;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.SwingUtilities;
import javax.swing.text.EditorKit;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.ai.swing.chat.ChatPanel;
import uno.anahata.ai.swing.chat.render.editorkit.EditorKitProvider;

/**
 * Renders a code block segment into a syntax-highlighted {@link JEditorPane}
 * or a plain {@link JTextArea} as a fallback. This class extends
 * {@link AbstractTextSegmentRenderer} and manages the component and content
 * for diff-based updates.
 *
 * @author anahata
 */
@Slf4j
@Getter
public class CodeBlockSegmentRenderer extends AbstractTextSegmentRenderer {

    /**
     * The programming language for syntax highlighting.
     */
    private final String language;

    /**
     * Constructs a new CodeBlockSegmentRenderer.
     *
     * @param chatPanel The chat panel instance.
     * @param initialContent The initial code content for this segment.
     * @param language The programming language for syntax highlighting.
     */
    public CodeBlockSegmentRenderer(ChatPanel chatPanel, String initialContent, String language) {
        super(chatPanel, initialContent);
        this.language = language;
    }

    /**
     * Renders or updates the code block segment.
     * It reuses the existing {@link JEditorPane} or {@link JTextArea} if available
     * and updates its content only if the code has changed.
     *
     * @return True if a visual update occurred, false otherwise.
     */
    @Override
    public boolean render() {
        boolean changed = hasContentChanged();

        if (component == null) {
            // Initial render: create the component
            EditorKitProvider editorKitProvider = chatPanel.getChatConfig().getEditorKitProvider();

            JComponent innerComponent;
            if (editorKitProvider == null) {
                log.warn("EditorKitProvider is null. Cannot render code block.");
                innerComponent = createFallbackTextArea(currentContent);
            } else {
                JEditorPane codeEditor = new JEditorPane();
                codeEditor.setEditable(false);

                try {
                    EditorKit kit = editorKitProvider.getEditorKitForLanguage(language);
                    if (kit == null) {
                        log.warn("No EditorKit found for language '" + language + "'. Falling back to plain text for code.");
                        innerComponent = createFallbackTextArea(currentContent);
                    } else {
                        codeEditor.setEditorKit(kit);
                        codeEditor.getDocument().putProperty("mimeType", kit.getContentType());
                        log.info("codeEditor.getDocument().putProperty(mimeType, {}); for '{}'.", kit.getContentType(), language);
                        innerComponent = codeEditor;
                    }
                } catch (Exception e) {
                    log.warn("Failed to render code block for language '" + language + "'.", e);
                    innerComponent = createFallbackTextArea(currentContent);
                }
            }
            // Always wrap the inner component in a JScrollPane
            JScrollPane scrollPane = new JScrollPane(innerComponent);
            scrollPane.setPreferredSize(new java.awt.Dimension(0, 200)); // Default height
            scrollPane.setBorder(javax.swing.BorderFactory.createEmptyBorder());
            this.component = scrollPane;
            changed = true; // Component was created, so a change occurred
        }

        // Update content only if it has changed
        if (changed) {
            JComponent innerComponent = (JComponent) ((JScrollPane) this.component).getViewport().getView();
            if (innerComponent instanceof JEditorPane) {
                ((JEditorPane) innerComponent).setText(currentContent);
            } else if (innerComponent instanceof JTextArea) {
                ((JTextArea) innerComponent).setText(currentContent);
            }
            contentRendered(); // Mark content as rendered
        }
        return changed;
    }

    /**
     * Displays the content of this code block renderer in a non-modal popup dialog.
     *
     * @param title The title of the dialog.
     */
    public void showInPopup(String title) {
        Window ancestorWindow = SwingUtilities.getWindowAncestor(chatPanel);
        JDialog dialog;
        if (ancestorWindow instanceof JDialog) {
            dialog = new JDialog((JDialog) ancestorWindow, title, Dialog.ModalityType.MODELESS);
        } else if (ancestorWindow instanceof JFrame) {
            dialog = new JDialog((JFrame) ancestorWindow, title, Dialog.ModalityType.MODELESS);
        } else {
            dialog = new JDialog((JFrame) null, title, Dialog.ModalityType.MODELESS); // Fallback to null parent
        }
        
        dialog.setLayout(new BorderLayout());
        dialog.setPreferredSize(new Dimension(800, 600));

        // CRITICAL: Ensure the component is rendered before adding it to the dialog
        render();
        JComponent contentComponent = getComponent();
        
        // The render method already wraps the inner component in a JScrollPane, so we just add it.
        dialog.add(contentComponent, BorderLayout.CENTER);

        dialog.pack();
        dialog.setLocationRelativeTo(chatPanel);
        dialog.setVisible(true);
    }

    /**
     * Creates a fallback JTextArea for displaying code when syntax highlighting is not available.
     *
     * @param code The code to display.
     * @return A JTextArea.
     */
    private JComponent createFallbackTextArea(String code) {
        JTextArea fallbackEditor = new JTextArea(code, 10, 80);
        fallbackEditor.setEditable(false);
        fallbackEditor.setLineWrap(true);
        fallbackEditor.setWrapStyleWord(true);
        fallbackEditor.setBackground(new java.awt.Color(240, 240, 240));
        fallbackEditor.setFont(new java.awt.Font(Font.MONOSPACED, java.awt.Font.PLAIN, 12));

        return fallbackEditor;
    }

    /**
     * Determines if this renderer can handle the given segment descriptor.
     * A {@code CodeBlockSegmentRenderer} handles {@link TextSegmentType#CODE} descriptors
     * with a matching language.
     *
     * @param descriptor The {@link TextSegmentDescriptor} to check.
     * @return True if the descriptor's type is {@link TextSegmentType#CODE} and the language matches, false otherwise.
     */
    @Override
    public boolean matches(TextSegmentDescriptor descriptor) {
        return descriptor.type() == TextSegmentType.CODE && Objects.equals(language, descriptor.language());
    }

    /**
     * Returns the JComponent managed by this renderer.
     *
     * @return The JComponent.
     */
    @Override
    public JComponent getComponent() {
        return component;
    }
}
