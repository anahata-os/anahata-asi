/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.agi.render;

import java.awt.Color;
import java.awt.Font;
import javax.swing.JComponent;
import javax.swing.JScrollPane;
import javax.swing.SwingUtilities;
import org.fife.ui.rsyntaxtextarea.RSyntaxTextArea;
import org.fife.ui.rsyntaxtextarea.SyntaxConstants;
import org.fife.ui.rtextarea.RTextScrollPane;
import uno.anahata.asi.swing.agi.AgiPanel;

/**
 * Renders code blocks using RSyntaxTextArea for rich syntax highlighting in standalone/generic Swing environments.
 */
public class RSyntaxTextAreaCodeBlockSegmentRenderer extends AbstractCodeBlockSegmentRenderer {

    /** The actual text area component. */
    private RSyntaxTextArea textArea;

    /**
     * Constructs a new RSyntaxTextAreaCodeBlockSegmentRenderer.
     *
     * @param agiPanel The agi panel instance.
     * @param initialContent The initial code content.
     * @param language The programming language.
     */
    public RSyntaxTextAreaCodeBlockSegmentRenderer(AgiPanel agiPanel, String initialContent, String language) {
        super(agiPanel, initialContent, language);
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details:
     * Simply creates the RSyntaxTextArea. The scroll pane and line numbers 
     * are handled by {@link #createScrollPane(JComponent)}.
     * </p>
     */
    @Override
    protected JComponent createInnerComponent() {
        this.textArea = new RSyntaxTextArea(currentContent);
        textArea.setEditable(false);
        textArea.setLineWrap(false);
        textArea.setTabSize(4);
        textArea.setCodeFoldingEnabled(true);
        textArea.setAntiAliasingEnabled(true);
        textArea.setHighlightCurrentLine(false);
        
        // Use a solid background to prevent rendering artifacts in some look-and-feels
        textArea.setOpaque(true);
        textArea.setBackground(Color.WHITE);
        textArea.setFont(new Font(Font.MONOSPACED, Font.PLAIN, 13));
        
        // Map language string to RSyntaxTextArea constants
        textArea.setSyntaxEditingStyle(mapLanguageToSyntax(language));

        // Ensure a repaint is triggered after initialization
        SwingUtilities.invokeLater(() -> textArea.repaint());
        
        return textArea;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details:
     * Wraps the text area in an RTextScrollPane to provide native line numbers 
     * and folding support in standalone mode.
     * </p>
     */
    @Override
    protected JScrollPane createScrollPane(JComponent inner) {
        RTextScrollPane scrollPane = new RTextScrollPane(inner);
        scrollPane.setLineNumbersEnabled(true);
        scrollPane.setFoldIndicatorEnabled(true);
        return scrollPane;
    }

    /** {@inheritDoc} */
    @Override
    protected void updateComponentContent(String content) {
        if (textArea != null) {
            textArea.setText(content);
            textArea.setCaretPosition(0);
        }
    }

    /** {@inheritDoc} */
    @Override
    protected String getCurrentContentFromComponent() {
        return (textArea != null) ? textArea.getText() : currentContent;
    }

    /** {@inheritDoc} */
    @Override
    protected void setComponentEditable(boolean editable) {
        if (textArea != null) {
            textArea.setEditable(editable);
        }
    }

    /**
     * Maps a language string to an RSyntaxTextArea syntax style constant.
     * 
     * @param language The language string.
     * @return The corresponding syntax style constant.
     */
    private String mapLanguageToSyntax(String language) {
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
