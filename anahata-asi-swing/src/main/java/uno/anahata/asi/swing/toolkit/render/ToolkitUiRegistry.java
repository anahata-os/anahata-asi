/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.toolkit.render;

import java.util.Optional;
import javax.swing.JPanel;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.tool.spi.AbstractToolkit;
import uno.anahata.asi.agi.tool.spi.java.JavaObjectToolkit;
import uno.anahata.asi.swing.agi.AgiPanel;
import uno.anahata.asi.swing.agi.context.AbstractContextNode;
import uno.anahata.asi.swing.internal.ClassHierarchyMap;

/**
 * A singleton registry for mapping toolkit classes to their specialized {@link ToolkitUI} strategies.
 * <p>
 * Manages the resolution of both specialized tree nodes and detail panels for toolkits,
 * supporting hierarchical lookup using {@link ClassHierarchyMap}.
 * </p>
 * 
 * @author anahata
 */
@Slf4j
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class ToolkitUiRegistry {

    /**
     * The singleton instance of the registry.
     */
    private static final ToolkitUiRegistry INSTANCE = new ToolkitUiRegistry();

    /**
     * Gets the singleton registry instance.
     *
     * @return The singleton registry instance.
     */
    public static ToolkitUiRegistry getInstance() {
        return INSTANCE;
    }

    /** 
     * Internal registry mapping toolkit classes to their specialized UI strategies.
     */
    private final ClassHierarchyMap<Object, ToolkitUI<Object>> uis = new ClassHierarchyMap<>(Object.class);

    /**
     * Registers a UI strategy for a specific toolkit class.
     * 
     * @param <T> The toolkit type.
     * @param toolkitClass The toolkit class.
     * @param ui The UI strategy instance.
     */
    @SuppressWarnings("unchecked")
    public <T> void register(Class<T> toolkitClass, ToolkitUI<T> ui) {
        uis.put(toolkitClass, (ToolkitUI<Object>) ui);
    }

    /**
     * Creates a specialized context tree node for the given {@link JavaObjectToolkit},
     * if a strategy is registered for its underlying instance class.
     *
     * @param agiPanel The parent AgiPanel.
     * @param toolkit The JavaObjectToolkit wrapper.
     * @return A custom context node, or {@code null} if no custom node is registered.
     */
    @SuppressWarnings("unchecked")
    public AbstractContextNode<?> createNode(AgiPanel agiPanel, JavaObjectToolkit toolkit) {
        if (toolkit != null && toolkit.getToolkitInstance() != null) {
            Object instance = toolkit.getToolkitInstance();
            Optional<ToolkitUI<Object>> uiOpt = uis.find(instance.getClass());
            if (uiOpt.isPresent()) {
                return uiOpt.get().createNode(agiPanel, instance, toolkit);
            }
        }
        return null;
    }

    /**
     * Creates the specialized detail panel for the given toolkit wrapper, if a strategy is registered.
     *
     * @param toolkit The abstract toolkit wrapper.
     * @param parent The parent AgiPanel.
     * @return An Optional containing the bound JPanel if a panel was created.
     */
    @SuppressWarnings("unchecked")
    public Optional<JPanel> createRenderer(AbstractToolkit<?> toolkit, AgiPanel parent) {
        if (toolkit instanceof JavaObjectToolkit jot && jot.getToolkitInstance() != null) {
            Object instance = jot.getToolkitInstance();
            Optional<ToolkitUI<Object>> uiOpt = uis.find(instance.getClass());
            if (uiOpt.isPresent()) {
                JPanel panel = uiOpt.get().createPanel(instance, parent);
                return Optional.ofNullable(panel);
            }
        }
        return Optional.empty();
    }
}
