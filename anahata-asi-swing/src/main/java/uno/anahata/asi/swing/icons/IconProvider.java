/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.icons;

import javax.swing.Icon;
import uno.anahata.asi.context.ContextProvider;
import uno.anahata.asi.model.tool.AbstractTool;
import uno.anahata.asi.model.tool.AbstractToolkit;

/**
 * An interface for providing icons for various context-related objects.
 * This allows the UI to remain provider-agnostic while still displaying 
 * authentic, host-specific icons (e.g., NetBeans project or file icons).
 * 
 * @author anahata
 */
public interface IconProvider {

    /**
     * Gets an icon for a specific context provider.
     * @param cp The context provider.
     * @return The icon, or null if not available.
     */
    Icon getIconFor(ContextProvider cp);

    /**
     * Gets an icon for a specific toolkit.
     * @param toolkit The toolkit.
     * @return The icon, or null if not available.
     */
    Icon getIconFor(AbstractToolkit<?> toolkit);

    /**
     * Gets an icon for a specific tool.
     * @param tool The tool.
     * @return The icon, or null if not available.
     */
    Icon getIconFor(AbstractTool<?, ?> tool);
}
