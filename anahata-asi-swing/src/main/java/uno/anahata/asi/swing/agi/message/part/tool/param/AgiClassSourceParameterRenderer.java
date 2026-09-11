/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.agi.message.part.tool.param;

import java.awt.BorderLayout;
import javax.swing.JComponent;
import javax.swing.JPanel;
import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.resource.Resource;
import uno.anahata.asi.agi.resource.handle.StringHandle;
import uno.anahata.asi.agi.tool.ToolExecutionStatus;
import uno.anahata.asi.agi.tool.spi.AbstractToolCall;
import uno.anahata.asi.swing.agi.AgiPanel;
import uno.anahata.asi.swing.agi.resources.ResourceUI;
import uno.anahata.asi.swing.agi.resources.ResourceUiRegistry;
import uno.anahata.asi.swing.agi.resources.view.AbstractTextResourceViewer;
import uno.anahata.asi.toolkit.java.AgiClassSource;

/**
 * A specialized parameter renderer for individual {@link AgiClassSource} descriptors.
 * <p>
 * This renderer wraps the Java class source in a virtual {@link StringHandle} and leverages
 * the host-specific {@link ResourceUI} strategy (NetBeans editor or RSyntaxTextArea)
 * to provide 100% IDE fidelity including syntax highlighting, line numbers, and inline editing.
 * </p>
 * <p>
 * <b>Hierarchical Value Propagation:</b> When the user modifies the source code in the UI,
 * saving triggers {@link #valueChanged(Object)}, cleanly emitting a new immutable
 * {@link AgiClassSource} instance up to its parent list container or directly to the tool call.
 * </p>
 *
 * @author anahata
 */
@Slf4j
@Getter
@Setter
public class AgiClassSourceParameterRenderer extends AbstractParameterRenderer<AgiClassSource> {

    /** The central container hosting the high-fidelity text viewer. */
    private final JPanel container = new JPanel(new BorderLayout());

    /** The high-fidelity text viewer component. */
    private AbstractTextResourceViewer viewer;

    /** The virtual handle simulating the Java source file. */
    private StringHandle ephemeralHandle;

    /**
     * Constructs a new uninitialized AgiClassSourceParameterRenderer.
     */
    public AgiClassSourceParameterRenderer() {
        container.setOpaque(false);
    }

    /**
     * {@inheritDoc}
     * <p>Initializes the renderer with its context and sets up the container.</p>
     */
    @Override
    public void init(AgiPanel agiPanel, AbstractToolCall<?, ?> call, String paramName, AgiClassSource value) {
        super.init(agiPanel, call, paramName, value);
        container.setOpaque(false);
    }

    /**
     * {@inheritDoc}
     * <p>Returns the primary container component.</p>
     */
    @Override
    public JComponent getComponent() {
        return container;
    }

    /**
     * {@inheritDoc}
     * <p>Pushes streaming source code updates into the virtual handle.</p>
     */
    @Override
    public void updateContent(AgiClassSource value) {
        this.value = value;
        if (ephemeralHandle != null && value != null) {
            try {
                ephemeralHandle.write(value.sourceCode());
                if (viewer != null && viewer.getResource() != null) {
                    viewer.getResource().reloadIfNeeded();
                }
            } catch (Exception e) {
                log.error("Failed to update streaming content for AgiClassSource: {}", value.fqn(), e);
            }
        }
    }

    /**
     * {@inheritDoc}
     * <p>Initializes the virtual editor viewer and mounts it into the container.</p>
     */
    @Override
    public boolean render() {
        if (value == null) {
            return false;
        }

        if (viewer == null) {
            initViewer();
        }

        container.removeAll();
        if (viewer != null) {
            container.add(viewer, BorderLayout.CENTER);
        }
        container.revalidate();
        container.repaint();
        return true;
    }

    /**
     * Initializes the host-native high-fidelity text viewer via a virtual {@link StringHandle}.
     */
    private void initViewer() {
        String simpleName = value.getDisplayValue();
        String fileName = simpleName + ".java";

        this.ephemeralHandle = new StringHandle(fileName, value.sourceCode());
        this.ephemeralHandle.setContextPath(value.fqn().replace('.', '/') + ".java");

        // Inject custom classpath if available on the tool call
        if (call != null) {
            Object extraCp = call.getEffectiveArgs().get("extraClassPath");
            if (extraCp instanceof String s && !s.isBlank()) {
                ephemeralHandle.setAttribute("anahata.customClasspath", s);
            }
        }

        Resource ephemeral = new Resource(ephemeralHandle);
        try {
            ephemeral.reloadIfNeeded();
        } catch (Exception e) {
            log.error("Failed to reload ephemeral resource for {}", value.fqn(), e);
        }

        ResourceUI strategy = ResourceUiRegistry.getInstance().getResourceUI();
        if (strategy != null) {
            JComponent content = strategy.createContent(ephemeral, agiPanel);
            if (content instanceof AbstractTextResourceViewer atv) {
                this.viewer = atv;
                viewer.setVerticalScrollEnabled(false);
                viewer.setPreviewAsEditor(true);

                boolean isPending = call != null && call.getResponse().getStatus() == ToolExecutionStatus.PENDING;
                viewer.setReadOnly(!isPending);

                // WIRE PERSISTENCE: Propagates edited source code back up the hierarchy
                viewer.setSaveAction(newSourceCode -> {
                    AgiClassSource updated = new AgiClassSource(value.fqn(), newSourceCode);
                    valueChanged(updated);
                    viewer.setEditing(false);
                });
            }
        }
    }
}
