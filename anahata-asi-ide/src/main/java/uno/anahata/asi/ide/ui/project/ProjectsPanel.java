/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.ide.ui.project;

import java.awt.BorderLayout;
import javax.swing.BorderFactory;
import javax.swing.JPanel;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.swing.agi.AgiPanel;
import uno.anahata.asi.swing.toolkit.render.AbstractToolkitRenderer;
import uno.anahata.asi.ide.tools.project.AbstractProjects;

/**
 * Custom toolkit UI renderer for {@link AbstractProjects}.
 * <p>
 * Displays the workspace-wide default project structure granularity scope,
 * allowing the user to configure default structure detail settings inherited
 * by all open project context providers.
 * </p>
 *
 * @author anahata
 */
@Slf4j
public class ProjectsPanel extends AbstractToolkitRenderer<AbstractProjects> {

    private ProjectStructureScopePanel scopePanel;

    /**
     * Constructs a new ProjectsPanel.
     */
    public ProjectsPanel() {
        super();
        setBorder(BorderFactory.createTitledBorder("Default Project Structure Scope"));
    }

    @Override
    protected void onBind() {
        removeAll();
        AbstractProjects toolkit = getAnahataToolkit();
        scopePanel = new ProjectStructureScopePanel(toolkit.getDefaultScope());
        scopePanel.setOnScopeChanged(newScope -> {
            log.info("Updating workspace-wide default ProjectStructureScope: {}", newScope);
            toolkit.setDefaultScope(newScope);
        });

        add(scopePanel, BorderLayout.CENTER);
        revalidate();
        repaint();
    }

    @Override
    public void propertyChange(java.beans.PropertyChangeEvent evt) {
        if ("defaultScope".equals(evt.getPropertyName()) && scopePanel != null) {
            AbstractProjects toolkit = getAnahataToolkit();
            scopePanel.setScope(toolkit.getDefaultScope());
        }
    }
}
