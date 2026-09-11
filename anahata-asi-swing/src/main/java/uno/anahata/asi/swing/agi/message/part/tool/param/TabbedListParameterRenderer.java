/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.agi.message.part.tool.param;

import java.awt.BorderLayout;
import java.awt.Color;
import java.util.ArrayList;
import java.util.List;
import javax.swing.JComponent;
import javax.swing.JPanel;
import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.Displayable;
import uno.anahata.asi.agi.tool.spi.AbstractToolCall;
import uno.anahata.asi.swing.agi.AgiPanel;
import uno.anahata.asi.swing.components.AdjustingTabPane;

/**
 * A generic composite parameter renderer that arranges a list of items into an {@link AdjustingTabPane}.
 * <p>
 * For each item in the list, a child {@link ParameterRenderer} is instantiated and registered with
 * {@link #setParentRenderer(ParameterRenderer)}. Tab titles are derived from {@link Displayable#getDisplayValue()}
 * if implemented, or an indexed fallback.
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
public class TabbedListParameterRenderer extends AbstractListParameterRenderer<Object> {

    /** The container panel hosting the tab pane. */
    private final JPanel container = new JPanel(new BorderLayout());

    /** The adjusting tab pane hosting child renderers. */
    private final AdjustingTabPane tabPane = new AdjustingTabPane(150);

    /**
     * Constructs a new TabbedListParameterRenderer.
     */
    public TabbedListParameterRenderer() {
        container.add(tabPane, BorderLayout.CENTER);
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
     * <p>Builds the tabs and child renderers for each item in the list.</p>
     */
    @Override
    public boolean render() {
        tabPane.removeAll();
        childRenderers.clear();

        if (value == null || value.isEmpty()) {
            return false;
        }

        for (int i = 0; i < value.size(); i++) {
            Object item = value.get(i);
            ParameterRenderer<Object> child = createChildRenderer(item, i);
            childRenderers.add(child);
            child.render();

            String title = (item instanceof Displayable d) ? d.getDisplayValue() : ("#" + (i + 1));
            tabPane.addTab(title, child.getComponent());
        }

        container.revalidate();
        container.repaint();
        return true;
    }

    /**
     * {@inheritDoc}
     * <p>Updates the tab title with an asterisk (*) and blue color when an item is modified.</p>
     */
    @Override
    protected void onChildItemUpdated(int index, Object updatedItem) {
        String title = (updatedItem instanceof Displayable d) ? d.getDisplayValue() : ("#" + (index + 1));
        tabPane.setTitleAt(index, title + "*");
        tabPane.setForegroundAt(index, Color.BLUE);
    }
}
