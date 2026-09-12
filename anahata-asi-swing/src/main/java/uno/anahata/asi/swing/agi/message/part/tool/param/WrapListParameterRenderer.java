/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.agi.message.part.tool.param;

import java.awt.FlowLayout;
import java.util.List;
import javax.swing.JComponent;
import javax.swing.JPanel;
import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.swing.components.WrapLayout;

/**
 * A generic composite parameter renderer that arranges a list of child item renderers
 * flowing horizontally and wrapping vertically in a {@link WrapLayout}.
 * <p>
 * This is the natural container layout for lightweight inline items such as chips,
 * badges, pills, and tiles (e.g. URIs, file paths, resource UUIDs).
 * </p>
 * <p>
 * <b>Hierarchical Child-to-Parent Propagation:</b> When any child renderer modifies its value,
 * it notifies {@link #childValueChanged}, updating the corresponding element in the collection
 * and propagating the change up the composite hierarchy.
 * </p>
 *
 * @author anahata
 */
@Slf4j
@Getter
@Setter
public class WrapListParameterRenderer extends AbstractListParameterRenderer<Object> {

    /** The container panel hosting the child components in a WrapLayout. */
    private final JPanel container = new JPanel(new WrapLayout(FlowLayout.LEFT, 5, 5));

    /**
     * Constructs a new WrapListParameterRenderer.
     */
    public WrapListParameterRenderer() {
        container.setOpaque(false);
    }

    /**
     * {@inheritDoc}
     * <p>Returns the main container component.</p>
     */
    @Override
    public JComponent getComponent() {
        return container;
    }

    /**
     * {@inheritDoc}
     * <p>Builds the child item renderers and adds them to the flowing WrapLayout.</p>
     */
    @Override
    public boolean render() {
        container.removeAll();
        childRenderers.clear();

        if (value == null || value.isEmpty()) {
            return false;
        }

        for (int i = 0; i < value.size(); i++) {
            Object item = value.get(i);
            ParameterRenderer<Object> child = createChildRenderer(item, i);
            childRenderers.add(child);
            child.render();
            container.add(child.getComponent());
        }

        container.revalidate();
        container.repaint();
        return true;
    }
}
