/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.ide.ui.project;

import javax.swing.JPanel;
import uno.anahata.asi.ide.tools.project.context.AbstractProjectContextProvider;
import uno.anahata.asi.swing.agi.AgiPanel;
import uno.anahata.asi.swing.agi.context.AbstractContextNode;
import uno.anahata.asi.swing.agi.context.ContextPanel;
import uno.anahata.asi.swing.agi.context.ContextProviderUI;

/**
 * Universal UI strategy for {@link AbstractProjectContextProvider} instances.
 * <p>
 * Manages the creation of the {@link ProjectContextProviderNode} for the context tree
 * and the {@link ProjectContextProviderPanel} for scope override configuration.
 * </p>
 *
 * @author anahata
 */
public class ProjectContextProviderUI implements ContextProviderUI<AbstractProjectContextProvider> {

    @Override
    public AbstractContextNode<?> createNode(AgiPanel agiPanel, AbstractProjectContextProvider provider) {
        return new ProjectContextProviderNode(agiPanel, provider);
    }

    @Override
    public JPanel createPanel(AbstractProjectContextProvider provider, ContextPanel contextPanel) {
        ProjectContextProviderPanel panel = new ProjectContextProviderPanel();
        return panel.createProviderPanel(provider, contextPanel);
    }
}
