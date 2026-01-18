/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.chat.render;

import java.awt.Color;
import java.awt.Font;
import javax.swing.JComponent;
import org.fife.ui.rsyntaxtextarea.RSyntaxTextArea;
import org.fife.ui.rsyntaxtextarea.SyntaxConstants;
import uno.anahata.asi.swing.chat.ChatPanel;

/**
 * RSyntaxTextArea implementation of a code block segment renderer.
 *
 * @author anahata
 */
public class RSyntaxTextAreaCodeBlockSegmentRenderer extends AbstractCodeBlockSegmentRenderer {

    /**
     * Constructs a new RSyntaxTextAreaCodeBlockSegmentRenderer.
     *
     * @param chatPanel The chat panel instance.
     * @param initialContent The initial code content.
     * @param language The programming language.
     */
    public RSyntaxTextAreaCodeBlockSegmentRenderer(ChatPanel chatPanel, String initialContent, String language) {
        super(chatPanel, initialContent, language);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected JComponent createInnerComponent() {
        RSyntaxTextArea textArea = new RSyntaxTextArea(currentContent);
        textArea.setSyntaxEditingStyle(mapLanguageToSyntaxStyle(language));
        textArea.setEditable(false); 
        textArea.setLineWrap(false); 
        textArea.setCodeFoldingEnabled(true);
        textArea.setAntiAliasingEnabled(true);
        textArea.setTabSize(4); 
        textArea.setHighlightCurrentLine(false); 
        
        textArea.setOpaque(false);
        textArea.setBackground(new Color(0, 0, 0, 0));
        textArea.setFont(new java.awt.Font(Font.MONOSPACED, java.awt.Font.PLAIN, 13));
        
        return textArea;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void updateComponentContent(String content) {
        ((RSyntaxTextArea) innerComponent).setText(content);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected String getCurrentContentFromComponent() {
        return ((RSyntaxTextArea) innerComponent).getText();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void setComponentEditable(boolean editable) {
        ((RSyntaxTextArea) innerComponent).setEditable(editable);
    }

    /**
     * Maps a language string to an RSyntaxTextArea syntax style constant.
     * 
     * @param language The language string.
     * @return The corresponding syntax style constant.
     */
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
}
