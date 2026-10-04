/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.ide;

import java.io.IOException;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.ide.tools.project.AbstractProjects;
import uno.anahata.asi.ide.tools.project.context.AbstractProjectContextProvider;
import uno.anahata.asi.ide.ui.project.ProjectContextProviderUI;
import uno.anahata.asi.ide.ui.project.ProjectsUI;
import uno.anahata.asi.swing.AbstractSwingAsiContainer;
import uno.anahata.asi.swing.agi.context.ContextProviderUiRegistry;
import uno.anahata.asi.swing.toolkit.render.ToolkitUiRegistry;

/**
 * Universal abstract base container for all IDE host environments.
 * <p>
 * Bridges common IDE UI strategies (such as {@link ProjectsUI} and {@link ProjectContextProviderUI})
 * into the Swing container registry, establishing a unified foundation for NetBeans, IntelliJ IDEA,
 * and future IDE integration plugins.
 * </p>
 *
 * @author anahata
 */
@Slf4j
public abstract class AbstractIdeAsiContainer extends AbstractSwingAsiContainer {

    static {
        ToolkitUiRegistry.getInstance().register(AbstractProjects.class, new ProjectsUI());
        ContextProviderUiRegistry.getInstance().register(AbstractProjectContextProvider.class, new ProjectContextProviderUI());
    }

    /**
     * Constructs a new AbstractIdeAsiContainer.
     *
     * @param hostApplicationId The unique identifier of the host IDE (e.g. {@code "netbeans"}, {@code "intellij"}).
     * @throws IOException if the container directory cannot be created or accessed.
     */
    public AbstractIdeAsiContainer(String hostApplicationId) throws IOException {
        super(hostApplicationId);
    }
}
