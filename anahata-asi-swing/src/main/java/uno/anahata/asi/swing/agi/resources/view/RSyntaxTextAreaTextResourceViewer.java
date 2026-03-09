/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.agi.resources.view;

import java.awt.Color;
import java.awt.Component;
import java.awt.Font;
import java.io.IOException;
import javax.swing.JComponent;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import lombok.extern.slf4j.Slf4j;
import org.fife.ui.rsyntaxtextarea.RSyntaxTextArea;
import org.fife.ui.rsyntaxtextarea.SyntaxConstants;
import org.fife.ui.rtextarea.RTextScrollPane;
import uno.anahata.asi.agi.resource.Resource;
import uno.anahata.asi.agi.resource.view.TextView;
import uno.anahata.asi.swing.agi.AgiPanel;
import uno.anahata.asi.swing.internal.SwingUtils;

/**
 * A specialized viewer for textual resources using RSyntaxTextArea.
 * <p>
 * This viewer implements the {@link AbstractTextResourceViewer} contract, 
 * providing high-fidelity syntax highlighting and a professional Edit/Save 
 * experience for standalone environments.
 * </p>
 * <p>
 * <b>Fidelity Singularity:</b> For virtual resources (snippets), this viewer 
 * uses a single RSyntax component pair for both View and Edit modes to prevent 
 * library-level gutter duplication and layout desyncs.
 * </p>
 * 
 * @author anahata
 */
@Slf4j
public class RSyntaxTextAreaTextResourceViewer extends AbstractTextResourceViewer {

    /** The RSyntax area used for the preview/viewport card. */
    private RSyntaxTextArea previewArea;
    /** The RSyntax area used for the editor card. */
    private RSyntaxTextArea editorArea;

    /**
     * Constructs a new RSyntaxTextAreaTextResourceViewer.
     * 
     * @param agiPanel The parent panel.
     * @param resource The resource to render.
     */
    public RSyntaxTextAreaTextResourceViewer(AgiPanel agiPanel, Resource resource) {
        super(agiPanel, resource);
    }

    /** 
     * {@inheritDoc}
     * <p>
     * Implementation details:
     * Creates a read-only RSyntaxTextArea wrapped in an RTextScrollPane. 
     * If {@code previewAsEditor} is true, this method returns a dummy panel 
     * as the editor component will handle both modes to prevent gutter duplication.
     * </p>
     */
    @Override
    protected JComponent createPreviewComponent() {
        if (previewAsEditor) {
            return new JPanel();
        }
        this.previewArea = createRSyntaxArea();
        previewArea.setEditable(false);
        return new RTextScrollPane(previewArea);
    }

    /** 
     * {@inheritDoc}
     * <p>
     * Implementation details:
     * Creates an editable RSyntaxTextArea wrapped in an RTextScrollPane 
     * for full-file modifications. In snippet mode, this component is used 
     * for both View and Edit states.
     * </p>
     */
    @Override
    protected JComponent createEditorComponent() {
        this.editorArea = createRSyntaxArea();
        editorArea.setEditable(true);
        return new RTextScrollPane(editorArea);
    }

    /**
     * Internal factory for RSyntaxTextArea with standard ASI styling.
     * @return A styled RSyntaxTextArea instance.
     */
    private RSyntaxTextArea createRSyntaxArea() {
        RSyntaxTextArea area = new RSyntaxTextArea();
        area.setLineWrap(false);
        area.setTabSize(4);
        area.setCodeFoldingEnabled(true);
        area.setAntiAliasingEnabled(true);
        area.setFont(new Font(Font.MONOSPACED, Font.PLAIN, 12));
        area.setSyntaxEditingStyle(mapLanguageToSyntax(resource.getName()));
        return area;
    }

    /** 
     * {@inheritDoc}
     * <p>
     * Implementation details:
     * Authoritatively toggles the editability of the internal components. 
     * If snippets are using the single-component strategy, it only affects the 
     * editorArea.
     * </p>
     */
    @Override
    protected void setComponentEditable(boolean editable) {
        if (editorArea != null) {
            editorArea.setEditable(editable);
        }
        // If not using previewAsEditor, also toggle preview area to be safe
        if (!previewAsEditor && previewArea != null) {
            previewArea.setEditable(false);
        }
    }

    /** 
     * {@inheritDoc}
     * <p>
     * Implementation details:
     * Loads the full text content from the resource handle into the editor area 
     * and resets the caret position to ensure a clean editing start.
     * </p>
     */
    @Override
    protected void onEditorActivated() {
        if (editorArea == null) {
            return;
        }
        try {
            // Load full content for editing
            editorArea.setText(resource.asText());
            editorArea.setCaretPosition(0);
        } catch (Exception e) {
            editorArea.setText("Error loading content: " + e.getMessage());
        }
    }

    /** 
     * {@inheritDoc}
     * <p>
     * Implementation details:
     * Disables the editor's editability and triggers a synchronization 
     * with the resource's latest viewport data.
     * </p>
     */
    @Override
    protected void onPreviewActivated() {
        if (editorArea != null && previewAsEditor) {
            editorArea.setEditable(false);
        }
        syncWithResource();
    }

    /** 
     * {@inheritDoc}
     * <p>
     * Implementation details:
     * Returns the current text from the underlying RSyntaxTextArea.
     * </p>
     */
    @Override
    public String getEditorContent() {
        return (editorArea != null) ? editorArea.getText() : null;
    }

    /** 
     * {@inheritDoc}
     * <p>
     * Implementation details:
     * Authoritative streaming ingestion point. Updates the active RSyntax area 
     * with incremental content, ensuring bit-by-bit generation fidelity.
     * </p>
     */
    @Override
    protected void updatePreviewContent(String content) {
        String text = content != null ? content : "";
        
        // Snippets stay on the editor area even in preview mode
        if (previewAsEditor && editorArea != null) {
            editorArea.setText(text);
            editorArea.setCaretPosition(0);
        } else if (previewArea != null) {
            previewArea.setText(text);
            previewArea.setCaretPosition(0);
        }
    }

    /** 
     * {@inheritDoc}
     * <p>
     * Implementation details:
     * Performs a complete UI synchronization with the resource state. 
     * Ensures snippets are visible immediately by falling back to the 
     * handle's text if the viewport cache is not yet populated.
     * </p>
     */
    @Override
    protected void syncWithResource() {
        super.syncWithResource();
    }

    /** 
     * {@inheritDoc}
     * <p>
     * Implementation details:
     * Recursively finds the internal RSyntaxTextArea and installs the 
     * boundary-aware mouse wheel redispatcher to enable scroll passthrough.
     * </p>
     */
    @Override
    protected void configureScrollBehavior() {
        super.configureScrollBehavior();
        
        if (!verticalScrollEnabled) {
            // RSyntax components consume events, so we must install forwarders 
            // on the areas themselves rather than the scroll pane wrapper.
            if (previewArea != null) {
                installWheelForwarder(previewArea);
            }
            if (editorArea != null) {
                installWheelForwarder(editorArea);
            }
        }
    }
    
    /**
     * Installs the boundary-aware wheel redispatcher on an RSyntaxTextArea.
     * @param area The text area component.
     */
    private void installWheelForwarder(RSyntaxTextArea area) {
        // Safe addition: ensure we don't double-install but don't strip internal library logic
        area.addMouseWheelListener(e -> {
            SwingUtils.redispatchMouseWheelEvent(area, e);
            e.consume();
        });
    }

    /**
     * Maps a language string or file name to an RSyntaxTextArea syntax style constant.
     * 
     * @param language The language string or file name.
     * @return The corresponding syntax style constant.
     */
    public static String mapLanguageToSyntax(String language) {
        if (language == null) {
            return SyntaxConstants.SYNTAX_STYLE_NONE;
        }
        
        if (language.contains(".")) {
            language = language.substring(language.lastIndexOf(".") + 1);
        }
        
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
