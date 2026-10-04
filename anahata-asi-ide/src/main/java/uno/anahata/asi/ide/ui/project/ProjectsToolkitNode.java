/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.ide.ui.project;

import uno.anahata.asi.agi.tool.spi.java.JavaObjectToolkit;
import uno.anahata.asi.ide.tools.project.AbstractProjects;
import uno.anahata.asi.swing.agi.AgiPanel;
import uno.anahata.asi.swing.agi.context.ToolkitNode;

/**
 * Context tree node representing the {@link AbstractProjects} toolkit.
 *
 * @author anahata
 */
public class ProjectsToolkitNode extends ToolkitNode {

    /**
     * Constructs a new ProjectsToolkitNode.
     *
     * @param agiPanel The parent AgiPanel component.
     * @param toolkit The typed AbstractProjects instance.
     * @param toolkitWrapper The JavaObjectToolkit wrapper.
     */
    public ProjectsToolkitNode(AgiPanel agiPanel, AbstractProjects toolkit, JavaObjectToolkit toolkitWrapper) {
        super(agiPanel, toolkitWrapper);
    }
}
