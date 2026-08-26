/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.uc;

import java.util.logging.Logger;
import org.openide.modules.ModuleInstall;

/**
 * Lifecycle installer for the standalone Anahata ASI Update Center NetBeans module.
 * <p>
 * Manages module restoration, clean window cleanup during uninstallation or updates,
 * and background catalog initialization.
 * </p>
 *
 * @author anahata
 */
public class AnahataUcInstaller extends ModuleInstall {

    /**
     * Logger instance for module lifecycle events.
     */
    private static final Logger LOG = Logger.getLogger(AnahataUcInstaller.class.getName());

    /**
     * {@inheritDoc}
     * <p>
     * Initializes default update centers on module startup.
     * </p>
     */
    @Override
    public void restored() {
        LOG.info("Anahata ASI Update Center Module Restored");
        AnahataUcUtils.registerDefaultUpdateCenters();
    }

    /**
     * {@inheritDoc}
     * <p>
     * Closes and disposes the update center dialog when the module is uninstalled
     * or upgraded, preventing window leaks across ClassLoader transitions.
     * </p>
     */
    @Override
    public void uninstalled() {
        LOG.info("Anahata ASI Update Center Module Uninstalled / Reloading - closing active dialogs");
        AnahataUpdateCenterDialog.closeDialog();
    }
}
