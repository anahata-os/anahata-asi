/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.agi.message.part.tool.param;

import java.util.ArrayList;
import java.util.List;
import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.tool.spi.AbstractToolCall;
import uno.anahata.asi.swing.agi.AgiPanel;

/**
 * Abstract base class for composite list and collection parameter renderers.
 * <p>
 * Manages the lifecycle of child {@link ParameterRenderer} instances, delegates
 * incremental {@link #updateContent} events, and intercepts {@link #childValueChanged}
 * to update the collection at the corresponding index and propagate changes.
 * </p>
 *
 * @param <E> The element type of the list.
 * @author anahata
 */
@Slf4j
@Getter
@Setter
public abstract class AbstractListParameterRenderer<E> extends AbstractParameterRenderer<List<E>> {

    /** Active list of child renderers corresponding to items in the collection. */
    protected final List<ParameterRenderer<E>> childRenderers = new ArrayList<>();

    /**
     * {@inheritDoc}
     * <p>Initializes the list parameter renderer with an editable copy of the list.</p>
     */
    @Override
    public void init(AgiPanel agiPanel, AbstractToolCall<?, ?> call, String paramName, List<E> value) {
        super.init(agiPanel, call, paramName, value != null ? new ArrayList<>(value) : new ArrayList<>());
    }

    /**
     * Creates and initializes a child parameter renderer for a specific element in the list.
     * Sets this list renderer as the child's parent renderer.
     *
     * @param item The element in the list.
     * @param index The 0-based index of the element.
     * @return The configured child parameter renderer.
     */
    protected ParameterRenderer<E> createChildRenderer(E item, int index) {
        ParameterRenderer<E> child = (ParameterRenderer<E>) ParameterRendererFactory.create(agiPanel, call, paramName, item, null);
        child.setParentRenderer(this);
        return child;
    }

    /**
     * {@inheritDoc}
     * <p>Updates the underlying list and pushes incremental updates to each child renderer if sizes match, or re-renders.</p>
     */
    @Override
    public void updateContent(List<E> newValue) {
        this.value = newValue != null ? new ArrayList<>(newValue) : new ArrayList<>();
        if (this.value.size() == childRenderers.size()) {
            for (int i = 0; i < this.value.size(); i++) {
                childRenderers.get(i).updateContent(this.value.get(i));
            }
        } else {
            render();
        }
    }

    /**
     * {@inheritDoc}
     * <p>
     * Intercepts child value changes, updates the element in the list at the corresponding index,
     * propagates the updated list via {@link #valueChanged(Object)}, and triggers {@link #onChildItemUpdated}.
     * </p>
     */
    @Override
    public void childValueChanged(ParameterRenderer<?> child, Object newChildValue) {
        int index = childRenderers.indexOf(child);
        if (index != -1 && this.value != null && index < this.value.size()) {
            this.value.set(index, (E) newChildValue);
            valueChanged(this.value);
            onChildItemUpdated(index, (E) newChildValue);
        } else if (getParentRenderer() != null) {
            getParentRenderer().childValueChanged(child, newChildValue);
        }
    }

    /**
     * Hook invoked when a child element at the given index has been modified.
     * Subclasses (like {@link TabbedListParameterRenderer}) override this to update UI indicators.
     *
     * @param index The 0-based index of the updated item.
     * @param updatedItem The updated item.
     */
    protected void onChildItemUpdated(int index, E updatedItem) {
        // Default: no-op
    }
}
