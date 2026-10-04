/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.ide.ui.project;

import javax.swing.JPanel;
import uno.anahata.asi.agi.tool.spi.java.JavaObjectToolkit;
import uno.anahata.asi.ide.tools.project.AbstractProjects;
import uno.anahata.asi.swing.agi.AgiPanel;
import uno.anahata.asi.swing.agi.context.AbstractContextNode;
import uno.anahata.asi.swing.toolkit.render.ToolkitUI;

/**
 * Universal UI strategy for the {@link AbstractProjects} toolkit.
 * <p>
 * Manages the creation of the {@link ProjectsToolkitNode} for the context tree
 * and the {@link ProjectsPanel} for default scope configuration.
 * </p>
 *
 * @author anahata
 */
public class ProjectsUI implements ToolkitUI<AbstractProjects> {

    @Override
    public AbstractContextNode<?> createNode(AgiPanel agiPanel, AbstractProjects toolkit, JavaObjectToolkit toolkitWrapper) {
        return new ProjectsToolkitNode(agiPanel, toolkit, toolkitWrapper);
    }

    @Override
    public JPanel createPanel(AbstractProjects toolkit, AgiPanel agiPanel) {
        ProjectsPanel panel = new ProjectsPanel();
        return panel.createToolkitPanel(toolkit, agiPanel);
    }
}
