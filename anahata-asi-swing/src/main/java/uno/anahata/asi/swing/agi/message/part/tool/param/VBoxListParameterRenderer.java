/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.agi.message.part.tool.param;

import java.util.ArrayList;
import java.util.List;
import javax.swing.JComponent;
import javax.swing.JPanel;
import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import net.miginfocom.swing.MigLayout;
import uno.anahata.asi.agi.tool.spi.AbstractToolCall;
import uno.anahata.asi.swing.agi.AgiPanel;

/**
 * A generic composite parameter renderer that arranges a list of items vertically in a column (VBox).
 * <p>
 * This is the default layout strategy for list parameters that do not specify a specialized layout
 * or for simple lists of strings, numbers, or records.
 * </p>
 * <p>
 * <b>Hierarchical Child-to-Parent Propagation:</b> When any child renderer notifies {@link #childValueChanged},
 * this compositor updates the corresponding item in the list and invokes {@link #valueChanged(Object)},
 * propagating the updated collection to the parent renderer or directly to the tool call's modified arguments.
 * </p>
 *
 * @author anahata
 */
@Slf4j
@Getter
@Setter
public class VBoxListParameterRenderer extends AbstractListParameterRenderer<Object> {

    /** The central panel hosting child components vertically. */
    private final JPanel container = new JPanel(new MigLayout("fillx, insets 0", "[grow,fill]", "[]2[]"));

    /**
     * Constructs a new VBoxListParameterRenderer.
     */
    public VBoxListParameterRenderer() {
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
     * <p>Lays out child renderers vertically in a column.</p>
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
            container.add(child.getComponent(), "growx, wrap");
        }

        container.revalidate();
        container.repaint();
        return true;
    }
}
