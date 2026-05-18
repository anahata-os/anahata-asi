/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.agi.message.part.tool.param;

import java.awt.BorderLayout;
import java.awt.Color;
import javax.swing.BorderFactory;
import javax.swing.JComponent;
import javax.swing.JPanel;
import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.internal.TextUtils;
import uno.anahata.asi.agi.tool.spi.AbstractToolCall;
import uno.anahata.asi.agi.resource.Resource;
import uno.anahata.asi.agi.resource.handle.StringHandle;
import uno.anahata.asi.swing.agi.AgiPanel;
import uno.anahata.asi.swing.agi.resources.view.AbstractTextResourceViewer;
import uno.anahata.asi.swing.agi.resources.ResourceUI;
import uno.anahata.asi.swing.agi.resources.ResourceUiRegistry;

/**
 * A specialized parameter renderer that converts any Java object to a string 
 * and displays it using a high-fidelity IDE viewer.
 * <p>
 * This implementation authoritatively leverages the {@link AbstractTextResourceViewer} 
 * directly, bypassing the markdown segment middle-man to provide a professional, 
 * host-aware editor for tool parameters.
 * </p>
 * <p>
 * <b>Representational Fidelity:</b> Uses the polymorphic {@link TextUtils#resolveContentString} 
 * to handle Strings, Enums, Collections, and Arrays with 100% technical purity.
 * </p>
 * 
 * @author anahata
 */
@Slf4j
public class ObjectToStringParameterRenderer implements ParameterRenderer<Object> {
    
    /** The parent panel. */
    protected AgiPanel agiPanel;
    /** The main container panel. */
    private final JPanel container = new JPanel(new BorderLayout());
    /** The high-fidelity viewer. */
    @Getter
    private AbstractTextResourceViewer viewer;

    /** The programming language for syntax highlighting. */
    @Getter @Setter
    private String language = "text";
    /** Whether the user is allowed to edit this parameter. */
    @Getter @Setter
    private boolean editable = false;

    /** No-arg constructor for factory instantiation. */
    public ObjectToStringParameterRenderer() {
        container.setOpaque(false);
        container.setBorder(BorderFactory.createLineBorder(new Color(200, 200, 200), 1, true));
    }

    /**
     * Creates a String handle calling the file "Anahata" if the extension is "java".
     * 
     * @param content the content of the handle
     * @param call the tool call that is creating it
     * @param paramName the parameter name in question
     * @return 
     */
    protected StringHandle createHandle(String content, AbstractToolCall<?, ?> call, String paramName) {
        String fileName = "java".equalsIgnoreCase(language) ? "Anahata.java" : "param." + language;
        return new StringHandle(fileName, content);
    }

    /** {@inheritDoc} 
     * <p>Implementation details: Initializes the high-fidelity pipeline by resolving the 
     * object to a string, wrapping it in a virtual resource, and requesting a viewer.</p>
     */
    @Override
    public void init(AgiPanel agiPanel, AbstractToolCall<?, ?> call, String paramName, Object value) {
        this.agiPanel = agiPanel;
        
        // 1. Resolve Purity: Convert object to string using authoritative utility
        String content = TextUtils.resolveContentString(value);
        
        // 2. Initialize High-Fidelity Sense (Cero Hardcoding: names trigger host-specific detection)
        StringHandle handle = createHandle(content, call, paramName);
        Resource ephemeral = new Resource(handle);
        try {
            ephemeral.reloadIfNeeded();
        } catch (Exception e) {
            log.error("Failed to perform initial reload on ephemeral resource", e);
        }
        
        ResourceUI strategy = ResourceUiRegistry.getInstance().getResourceUI();
        if (strategy != null) {
            JComponent contentComp = strategy.createContent(ephemeral, agiPanel);
            if (contentComp instanceof AbstractTextResourceViewer atv) {
                this.viewer = atv;
                viewer.setVerticalScrollEnabled(false); // Conversation passthrough
                viewer.setPreviewAsEditor(true); // Constant IDE fidelity
                viewer.setReadOnly(!editable); // Start read-only
                
                // WIRE PERSISTENCE: Save refinements back to the tool call
                viewer.setSaveAction(contentStr -> {
                    call.setModifiedArgument(paramName, contentStr);
                    viewer.setEditing(false);
                });
            }
        }

        // 3. Assemble UI
        container.removeAll();
        if (viewer != null) {
            container.add(viewer, BorderLayout.CENTER);
        }
    }

    /** 
     * {@inheritDoc} 
     * <p>Implementation details: Returns the central container which encapsulates 
     * the action strip and the high-fidelity viewer.</p>
     */
    @Override
    public JComponent getComponent() {
        return container;
    }

    /** {@inheritDoc} 
     * <p>Implementation details: Updates the underlying virtual resource handle 
     * with the new object's string representation.</p>
     */
    @Override
    public void updateContent(Object value) {
        if (viewer != null && viewer.getResource().getHandle() instanceof StringHandle sh) {
            try {
                String content = TextUtils.resolveContentString(value);
                sh.write(content);
                viewer.getResource().reloadIfNeeded();
            } catch (Exception e) {
                log.error("Failed to update parameter content", e);
            }
        }
    }

    /** 
     * {@inheritDoc} 
     * <p>Implementation details: Re-validates the container to ensure height 
     * adjustments are propagated through the conversation layout.</p>
     */
    @Override
    public boolean render() {
        container.revalidate();
        container.repaint();
        return true;
    }


}
