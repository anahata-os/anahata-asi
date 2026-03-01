/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.agi.render;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Cursor;
import java.awt.FlowLayout;
import java.io.File;
import java.util.Objects;
import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextArea;
import javax.swing.UIManager;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.model.tool.AbstractToolCall;
import uno.anahata.asi.model.tool.ToolExecutionStatus;
import uno.anahata.asi.swing.agi.AgiPanel;
import uno.anahata.asi.swing.agi.render.editorkit.EditorKitProvider;
import uno.anahata.asi.toolkit.files.Files;
import uno.anahata.asi.toolkit.files.FullTextFileCreate;

/**
 * A unified, environment-aware renderer for file creation operations.
 * <p>
 * This renderer manages the entire UI for a {@link FullTextFileCreate} parameter,
 * including the header with a file link, the syntax-highlighted editor, and 
 * authoritative pre-flight validation.
 * </p>
 * 
 * @author anahata
 */
@Slf4j
public class FullTextFileCreateRenderer implements ParameterRenderer<FullTextFileCreate> {

    /** The agi panel hosting this renderer. */
    private AgiPanel agiPanel;
    /** The tool call containing the parameter. */
    private AbstractToolCall<?, ?> call;
    /** The name of the parameter. */
    private String paramName;
    /** The current creation DTO. */
    private FullTextFileCreate value;
    
    /** The environment-appropriate code block renderer used as the editor. */
    private AbstractCodeBlockSegmentRenderer editor;
    /** The main container panel. */
    private final JPanel container = new JPanel(new BorderLayout());
    
    /** Cache of the last rendered state to prevent redundant UI rebuilds. */
    private FullTextFileCreate lastRenderedValue;
    /** Cache of the last rendered status. */
    private ToolExecutionStatus lastRenderedStatus;

    /** {@inheritDoc} */
    @Override
    public void init(AgiPanel agiPanel, AbstractToolCall<?, ?> call, String paramName, FullTextFileCreate value) {
        this.agiPanel = agiPanel;
        this.call = call;
        this.paramName = paramName;
        this.value = value;
    }

    /** {@inheritDoc} */
    @Override
    public JComponent getComponent() {
        return container;
    }

    /** {@inheritDoc} */
    @Override
    public void updateContent(FullTextFileCreate value) {
        this.value = value;
        if (editor != null) {
            editor.updateContent(value.getContent());
        }
    }

    /**
     * Performs an immediate validation of the file creation using the 
     * authoritative logic in the Files toolkit. If validation fails, the 
     * tool call is rejected.
     * 
     * @return true if valid, false if rejected.
     */
    private boolean validatePreFlight() {
        if (call.getResponse().getStatus() != ToolExecutionStatus.PENDING) {
            return true; 
        }

        try {
            Files files = agiPanel.getAgi().getToolkit(Files.class)
                    .orElseThrow(() -> new IllegalStateException("Files toolkit not found."));
            files.validateCreate(value);
            return true;
        } catch (Exception e) {
            log.warn("Pre-flight validation failed for {}: {}", value.getPath(), e.getMessage());
            call.getResponse().reject(e.getMessage());
            return false;
        }
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details:
     * Orchestrates the assembly of the header and the environment-appropriate 
     * editor. Uses the {@link EditorKitProvider} from the agi config to delegate 
     * language detection and renderer creation.
     * </p>
     */
    @Override
    public boolean render() {
        if (value == null) {
            return false;
        }

        // 1. Validation check
        if (!validatePreFlight()) {
            renderError(call.getResponse().getErrors());
            return true;
        }

        ToolExecutionStatus status = call.getResponse().getStatus();

        // 2. Stability check
        if (Objects.equals(value, lastRenderedValue) && status == lastRenderedStatus) {
            return true;
        }

        boolean isPending = status == ToolExecutionStatus.PENDING;
        EditorKitProvider provider = agiPanel.getAgiConfig().getEditorKitProvider();
        String language = provider.getLanguageForFile(value.getPath());

        // 3. Editor Rebuild check: if language or status changed, we need a fresh editor
        if (editor == null || status != lastRenderedStatus) {
            this.editor = provider.createRenderer(agiPanel, value.getContent(), language);
            editor.setEditable(isPending);
            editor.setOnSave(newContent -> {
                FullTextFileCreate updated = FullTextFileCreate.builder()
                        .path(value.getPath())
                        .content(newContent)
                        .build();
                this.value = updated;
                call.setModifiedArgument(paramName, updated);
            });
            
            // Trigger initial editor render
            editor.render();

            container.removeAll();
            container.add(createHeaderPanel(status), BorderLayout.NORTH);
            container.add(editor.getComponent(), BorderLayout.CENTER);
            container.revalidate();
            container.repaint();
        } else {
            // Incremental update of existing editor
            editor.updateContent(value.getContent());
            editor.render();
        }

        lastRenderedValue = value;
        lastRenderedStatus = status;
        return true;
    }

    /**
     * Creates the header panel with status label and filename link.
     * 
     * @param status The current execution status.
     * @return The populated header panel.
     */
    private JPanel createHeaderPanel(ToolExecutionStatus status) {
        JPanel header = new JPanel(new FlowLayout(FlowLayout.LEFT, 10, 5));
        
        String labelText = (status == ToolExecutionStatus.PENDING) ? "Proposed New File:" : "Created File:";
        JLabel label = new JLabel(labelText);
        label.setFont(label.getFont().deriveFont(java.awt.Font.BOLD));
        header.add(label);

        File f = new File(value.getPath());
        JButton link = new JButton("<html><a href='#'>" + f.getName() + "</a></html>");
        link.setToolTipText(value.getPath());
        link.setBorderPainted(false);
        link.setOpaque(false);
        link.setBackground(new Color(0, 0, 0, 0));
        
        // Link is only enabled if the file exists
        boolean exists = f.exists();
        link.setEnabled(exists);
        if (exists) {
            link.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
            link.addActionListener(e -> {
                 agiPanel.getAgi().getResourceManager().findByPath(value.getPath()).ifPresent(res -> {
                     agiPanel.getAgi().getConfig().getContainer().openResource(res);
                 });
            });
        } else {
            link.setToolTipText("File not yet created: " + value.getPath());
        }
        
        header.add(link);
        return header;
    }

    /**
     * Renders an error message in place of the editor.
     * 
     * @param message The error message to display.
     */
    private void renderError(String message) {
        JTextArea errorArea = new JTextArea(message);
        errorArea.setEditable(false);
        errorArea.setLineWrap(true);
        errorArea.setWrapStyleWord(true);
        errorArea.setBackground(UIManager.getColor("Panel.background"));
        errorArea.setForeground(Color.RED);
        errorArea.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));

        container.removeAll();
        container.add(errorArea, BorderLayout.CENTER);
        container.revalidate();
        container.repaint();
    }
}
