/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.agi.message.part.tool.param;

import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.Font;
import java.io.File;
import java.util.Objects;
import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextArea;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.model.tool.AbstractToolCall;
import uno.anahata.asi.model.tool.ToolExecutionStatus;
import uno.anahata.asi.resource.v2.Resource;
import uno.anahata.asi.resource.v2.handle.StringHandle;
import uno.anahata.asi.swing.agi.AgiPanel;
import uno.anahata.asi.swing.agi.resources.view.AbstractTextResourceViewer;
import uno.anahata.asi.swing.agi.resources.ResourceUI;
import uno.anahata.asi.swing.agi.resources.ResourceUiRegistry;
import uno.anahata.asi.toolkit.files.FullTextFileCreate;

/**
 * A unified, strategy-driven renderer for file creation operations.
 * <p>
 * This renderer manages the entire UI for a {@link FullTextFileCreate} parameter.
 * It leverages the V2 {@link ResourceUI} strategy to provide high-fidelity 
 * editing by wrapping the proposed file in a virtual {@link StringHandle}.
 * </p>
 * <p>
 * <b>Authoritative Navigation:</b> Once a file is created, the header provides 
 * a clickable hyperlink to the real resource and an 'Edit' toggle to refine 
 * the proposal.
 * </p>
 * 
 * @author anahata
 */
@Slf4j
public class FullTextFileCreateRenderer implements ParameterRenderer<FullTextFileCreate> {

    /** The agi panel instance. */
    private AgiPanel agiPanel;
    /** The tool call containing the parameter. */
    private AbstractToolCall<?, ?> call;
    /** The name of the parameter. */
    private String paramName;
    /** The current creation DTO. */
    private FullTextFileCreate value;
    
    /** The main container panel. */
    private final JPanel container = new JPanel(new BorderLayout());
    /** The toolbar for post-execution actions. */
    private final JPanel actionPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT, 10, 0));
    
    /** The high-fidelity viewer component. */
    private AbstractTextResourceViewer viewer;
    /** The virtual handle used for the proposal phase. */
    private StringHandle ephemeralHandle;

    /** Cache of the last rendered state. */
    private FullTextFileCreate lastRenderedValue;
    /** Cache of the last rendered status. */
    private ToolExecutionStatus lastRenderedStatus;

    /** 
     * {@inheritDoc} 
     * <p>
     * Implementation details:
     * Initializes the renderer with the necessary session and tool call context. 
     * Configures the container panels for role-neutral rendering.
     * </p>
     */
    @Override
    public void init(AgiPanel agiPanel, AbstractToolCall<?, ?> call, String paramName, FullTextFileCreate value) {
        this.agiPanel = agiPanel;
        this.call = call;
        this.paramName = paramName;
        this.value = value;
        container.setOpaque(false);
        actionPanel.setOpaque(false);
    }

    /** 
     * {@inheritDoc} 
     * <p>
     * Implementation details:
     * Returns the main container panel which will host either the high-fidelity 
     * editor or a validation error message.
     * </p>
     */
    @Override
    public JComponent getComponent() {
        return container;
    }

    /** 
     * {@inheritDoc} 
     * <p>
     * Implementation details:
     * Updates the underlying DTO and pushes the new content into the ephemeral 
     * virtual resource to support bit-by-bit streaming in the UI.
     * </p>
     */
    @Override
    public void updateContent(FullTextFileCreate value) {
        this.value = value;
        
        // STREAMING BRIDGE: Push new content into the ephemeral handle
        if (ephemeralHandle != null && value != null) {
            try {
                ephemeralHandle.write(value.getContent());
                viewer.getResource().reloadIfNeeded();
            } catch (Exception e) {
                log.error("Failed to update streaming content for proposed file: {}", value.getPath(), e);
            }
        }
    }

    /**
     * Performs authoritative pre-flight validation using the DTO's internal logic.
     * @return true if valid.
     */
    private boolean validatePreFlight() {
        if (call.getResponse().getStatus() != ToolExecutionStatus.PENDING) {
            return true; 
        }
        try {
            value.validate(agiPanel.getAgi());
            return true;
        } catch (Exception e) {
            call.getResponse().reject("Validation failed, no file got created: " + e.getMessage());
            return false;
        }
    }

    /** 
     * {@inheritDoc} 
     * <p>
     * Implementation details:
     * Executes the main render logic. It performs pre-flight validation, 
     * initializes the high-fidelity viewer if necessary, and assembles the 
     * final UI with host-specific actions and clickable file hyperlinks.
     * </p>
     */
    @Override
    public boolean render() {
        if (value == null) {
            return false;
        }

        if (!validatePreFlight()) {
            renderError(call.getResponse().getErrors());
            return true;
        }

        ToolExecutionStatus status = call.getResponse().getStatus();

        if (Objects.equals(value, lastRenderedValue) && status == lastRenderedStatus) {
            return true;
        }

        // Initialize Virtual High-Fidelity Pipeline
        if (viewer == null) {
            initViewer();
        }

        if (viewer != null) {
            container.removeAll();
            container.add(createHeaderPanel(status, ephemeralHandle.getOwner()), BorderLayout.NORTH);
            container.add(viewer, BorderLayout.CENTER);
        } else {
            renderError("No ResourceUI strategy registered for this host.");
        }

        lastRenderedValue = value;
        lastRenderedStatus = status;
        
        container.revalidate();
        container.repaint();
        return true;
    }

    /**
     * Initializes the host-native high-fidelity viewer via a virtual StringHandle.
     */
    private void initViewer() {
        File f = new File(value.getPath());
        String lang = agiPanel.getAgiConfig().getEditorKitProvider().getLanguageForFile(f.getName());
        
        // We use a StringHandle to support streaming and safe editing without disk side-effects
        this.ephemeralHandle = new StringHandle(f.getName(), "text/x-" + lang, value.getContent());
        Resource ephemeral = new Resource(ephemeralHandle);
        
        ResourceUI strategy = ResourceUiRegistry.getInstance().getResourceUI();
        if (strategy != null) {
            JComponent content = strategy.createContent(ephemeral, agiPanel);
            if (content instanceof AbstractTextResourceViewer atv) {
                this.viewer = atv;
                viewer.setToolbarVisible(false);
                viewer.setVerticalScrollEnabled(false);
                viewer.setEditing(true); // Always show editor for proposed files
                
                // WIRE PERSISTENCE: Refinements in the UI update the DTO
                viewer.setSaveAction(contentStr -> {
                    value.setContent(contentStr);
                    log.info("Proposed file content updated via UI editor: {}", value.getPath());
                });
            }
        }
    }

    /**
     * Creates the header panel including status label and post-execution actions.
     * @param status The current execution status.
     * @param resource The ephemeral resource instance.
     * @return The configured header JPanel.
     */
    private JPanel createHeaderPanel(ToolExecutionStatus status, Resource resource) {
        JPanel header = new JPanel(new BorderLayout());
        header.setOpaque(false);
        
        JPanel labelPanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 10, 5));
        labelPanel.setOpaque(false);
        
        String labelText = (status == ToolExecutionStatus.EXECUTED) ? "Created File:" : "Proposed File:";
        JLabel label = new JLabel(labelText);
        label.setFont(label.getFont().deriveFont(Font.BOLD));
        labelPanel.add(label);

        File f = new File(value.getPath());
        
        // NAVIGATION FIX: Always show file name and path in the header
        header.add(new JLabel(f.getName() + " (" + value.getPath() + ")"));
        
        header.add(labelPanel, BorderLayout.WEST);
        
        // Add navigation actions (Hyperlink / Edit)
        actionPanel.removeAll();
        ResourceUI strategy = ResourceUiRegistry.getInstance().getResourceUI();
        if (strategy != null) {
            // For both Proposed and Created, we want header actions available 
            // since the viewer's own toolbar is hidden.
            strategy.populateActions(actionPanel, resource, agiPanel);
            header.add(actionPanel, BorderLayout.EAST);
        }
        
        return header;
    }

    /**
     * Renders an error message in the center of the container.
     * @param message The error text.
     */
    private void renderError(String message) {
        container.removeAll();
        JTextArea errorArea = new JTextArea(message);
        errorArea.setForeground(java.awt.Color.RED);
        errorArea.setEditable(false);
        errorArea.setOpaque(false);
        errorArea.setLineWrap(true);
        errorArea.setWrapStyleWord(true);
        errorArea.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
        container.add(errorArea, BorderLayout.CENTER);
        container.revalidate();
        container.repaint();
    }
}
