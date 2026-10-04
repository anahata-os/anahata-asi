/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.ide.ui.resources;

import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JPanel;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.resource.Resource;
import uno.anahata.asi.agi.resource.handle.PathHandle;
import uno.anahata.asi.ide.resources.handle.IdeHandle;
import uno.anahata.asi.swing.agi.AgiPanel;
import uno.anahata.asi.swing.agi.resources.DefaultResourceUI;
import uno.anahata.asi.swing.icons.ActionIconKey;

/**
 * Universal IDE resource UI strategy.
 * <p>
 * Standardizes common IDE resource actions ("Open in Editor" and "Select in Project")
 * for physical resources and provides unified path resolution for {@link IdeHandle} instances.
 * </p>
 *
 * @author anahata
 */
@Slf4j
public abstract class IdeResourceUI extends DefaultResourceUI {

    /**
     * Returns the icon for the "Open in Editor" action button.
     *
     * @param agiPanel The active parent AGI panel.
     * @return The icon to display.
     */
    protected Icon getOpenInEditorIcon(AgiPanel agiPanel) {
        return agiPanel.getAgiConfig().getActionIcon(ActionIconKey.OPEN_IN_IDE, 16);
    }

    /**
     * Returns the icon for the "Select in Project" action button.
     *
     * @param agiPanel The active parent AGI panel.
     * @return The icon to display.
     */
    protected Icon getSelectInProjectIcon(AgiPanel agiPanel) {
        return agiPanel.getAgiConfig().getActionIcon(ActionIconKey.SEARCH, 16);
    }

    /**
     * {@inheritDoc}
     * <p>
     * Injects standard IDE 'Open in Editor' and 'Select in Project' actions for physical resources.
     * </p>
     */
    @Override
    public void populateActions(JPanel actionContainer, Resource resource, AgiPanel agiPanel) {
        if (!resource.getHandle().isVirtual()) {
            JButton openBtn = createLinkButton("Open in Editor",
                    "Open the file in the code editor.",
                    getOpenInEditorIcon(agiPanel));
            openBtn.addActionListener(e -> open(resource, agiPanel));
            actionContainer.add(openBtn);

            JButton selectBtn = createLinkButton("Select in Project",
                    "Locate and highlight the file in the IDE Projects tree.",
                    getSelectInProjectIcon(agiPanel));
            selectBtn.addActionListener(e -> select(resource, agiPanel));
            actionContainer.add(selectBtn);
        } else {
            super.populateActions(actionContainer, resource, agiPanel);
        }
    }

    /**
     * Resolves the physical path from an {@link IdeHandle} or {@link PathHandle}.
     *
     * @param resource The resource to resolve.
     * @return The absolute filesystem path, or null if virtual or non-file.
     */
    protected String getPath(Resource resource) {
        if (resource.getHandle() instanceof IdeHandle ih) {
            return ih.getPath();
        } else if (resource.getHandle() instanceof PathHandle ph) {
            return ph.getPath();
        }
        return null;
    }
}
