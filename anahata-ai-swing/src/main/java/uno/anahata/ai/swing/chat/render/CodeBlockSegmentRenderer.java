/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.ai.swing.chat.render;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dialog;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Window;
import java.util.Objects;
import javax.swing.BorderFactory;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JEditorPane;
import javax.swing.JFrame;
import javax.swing.JScrollPane;
import javax.swing.SwingUtilities;
import javax.swing.text.EditorKit;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import org.fife.ui.rsyntaxtextarea.RSyntaxTextArea;
import org.fife.ui.rsyntaxtextarea.SyntaxConstants;
import uno.anahata.ai.swing.chat.ChatPanel;
import uno.anahata.ai.swing.chat.render.editorkit.EditorKitProvider;
import uno.anahata.ai.swing.internal.SwingUtils;

/**
 * Renders a code block segment into a syntax-highlighted {@link JEditorPane}
 * or an {@link RSyntaxTextArea} as a fallback. This class extends
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
    
    /** The inner code component (RSyntaxTextArea or JEditorPane). */
    private JComponent innerComponent;

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
     * It reuses the existing {@link JEditorPane} or {@link RSyntaxTextArea} if available
     * and updates its content only if the code has changed.
     *
     * @return True if a visual update occurred, false otherwise.
     */
    @Override
    public boolean render() {
        boolean changed = hasContentChanged();

        if (component == null) {
            // Initial render: create the inner component
            EditorKitProvider editorKitProvider = chatPanel.getChatConfig().getEditorKitProvider();

            if (editorKitProvider != null) {
                try {
                    EditorKit kit = editorKitProvider.getEditorKitForLanguage(language);
                    if (kit != null) {
                        JEditorPane codeEditor = new JEditorPane();
                        codeEditor.setEditable(true);
                        codeEditor.setEditorKit(kit);
                        codeEditor.getDocument().putProperty("mimeType", kit.getContentType());
                        log.info("codeEditor.getDocument().putProperty(mimeType, {}); for '{}'.", kit.getContentType(), language);
                        innerComponent = codeEditor;
                    }
                } catch (Exception e) {
                    log.warn("Failed to get EditorKit for language '" + language + "'.", e);
                }
            }

            if (innerComponent == null) {
                log.info("Using RSyntaxTextArea for language '{}'.", language);
                innerComponent = createRSyntaxTextArea(currentContent, language);
            }

            // Wrap in a scroll pane for horizontal scrolling
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
            if (innerComponent instanceof JEditorPane editorPane) {
                editorPane.setText(currentContent);
            } else if (innerComponent instanceof RSyntaxTextArea syntaxTextArea) {
                syntaxTextArea.setText(currentContent);
            }
            contentRendered(); // Mark content as rendered
        }
        return changed;
    }

    /**
     * Creates an RSyntaxTextArea for displaying code with syntax highlighting.
     *
     * @param code The code to display.
     * @param language The language for syntax highlighting.
     * @return An RSyntaxTextArea.
     */
    private JComponent createRSyntaxTextArea(String code, String language) {
        RSyntaxTextArea textArea = new RSyntaxTextArea(code);
        textArea.setSyntaxEditingStyle(mapLanguageToSyntaxStyle(language));
        textArea.setEditable(true);
        textArea.setLineWrap(false); // Disable line wrap to allow horizontal scrolling
        textArea.setCodeFoldingEnabled(true);
        textArea.setAntiAliasingEnabled(true);
        
        // Use theme colors if possible, otherwise defaults
        textArea.setBackground(new java.awt.Color(245, 245, 245));
        textArea.setFont(new java.awt.Font(Font.MONOSPACED, java.awt.Font.PLAIN, 13));

        return textArea;
    }

    private String mapLanguageToSyntaxStyle(String language) {
        if (language == null) return SyntaxConstants.SYNTAX_STYLE_NONE;
        return switch (language.toLowerCase()) {
            case "java" -> SyntaxConstants.SYNTAX_STYLE_JAVA;
            case "python" -> SyntaxConstants.SYNTAX_STYLE_PYTHON;
            case "xml" -> SyntaxConstants.SYNTAX_STYLE_XML;
            case "html" -> SyntaxConstants.SYNTAX_STYLE_HTML;
            case "javascript", "js" -> SyntaxConstants.SYNTAX_STYLE_JAVASCRIPT;
            case "typescript", "ts" -> SyntaxConstants.SYNTAX_STYLE_TYPESCRIPT;
            case "json" -> SyntaxConstants.SYNTAX_STYLE_JSON;
            case "sql" -> SyntaxConstants.SYNTAX_STYLE_SQL;
            case "markdown", "md" -> SyntaxConstants.SYNTAX_STYLE_MARKDOWN;
            case "yaml", "yml" -> SyntaxConstants.SYNTAX_STYLE_YAML;
            case "css" -> SyntaxConstants.SYNTAX_STYLE_CSS;
            case "c" -> SyntaxConstants.SYNTAX_STYLE_C;
            case "cpp", "c++" -> SyntaxConstants.SYNTAX_STYLE_CPLUSPLUS;
            case "csharp", "c#" -> SyntaxConstants.SYNTAX_STYLE_CSHARP;
            case "php" -> SyntaxConstants.SYNTAX_STYLE_PHP;
            case "ruby" -> SyntaxConstants.SYNTAX_STYLE_RUBY;
            case "shell", "sh", "bash" -> SyntaxConstants.SYNTAX_STYLE_UNIX_SHELL;
            case "properties" -> SyntaxConstants.SYNTAX_STYLE_PROPERTIES_FILE;
            default -> SyntaxConstants.SYNTAX_STYLE_NONE;
        };
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
     * @return The JComponent (the JScrollPane).
     */
    @Override
    public JComponent getComponent() {
        return component;
    }
}
