/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.util;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.netbeans.api.autoupdate.InstallSupport;
import org.netbeans.api.autoupdate.OperationContainer;
import org.netbeans.api.autoupdate.OperationSupport.Restarter;
import org.netbeans.api.autoupdate.UpdateElement;
import org.netbeans.api.autoupdate.UpdateManager;
import org.netbeans.api.autoupdate.UpdateUnit;
import org.netbeans.api.autoupdate.UpdateUnitProvider;
import org.netbeans.api.autoupdate.UpdateUnitProviderFactory;
import org.openide.modules.Places;
import uno.anahata.asi.agi.tool.AgiToolException;

/**
 * Utility class for managing the registration and lifecycle of official Anahata
 * NetBeans Update Centers.
 * <p>
 * Automatically detects the active NetBeans major release version (e.g. 30, 31)
 * across multiple system heuristics (user directory, build number, and product
 * version properties) and registers the official production Update Center catalog
 * ({@code https://asi.anahata.uno/nb/{major}/updates.xml}) into the IDE's Autoupdate
 * infrastructure.
 * </p>
 *
 * @author anahata
 */
public final class AnahataUpdateCenterUtils {

    /**
     * Logger for update center registration diagnostics.
     */
    private static final Logger LOG = Logger.getLogger(AnahataUpdateCenterUtils.class.getName());

    /**
     * Unique internal provider code name for the official stable update center.
     */
    public static final String PROVIDER_CODENAME = "anahata-asi-update-center";

    /**
     * Unique internal provider code name for the development snapshot update center.
     */
    public static final String PROVIDER_CODENAME_DEV = "anahata-asi-dev-update-center";

    /**
     * Resource path on classpath for the Anahata 16x16 icon displayed in the Plugins manager.
     */
    public static final String ICON_BASE = "icons/anahata_16.png";

    /**
     * Category display name rendered for the stable provider source.
     */
    public static final String CATEGORY_DISPLAY_NAME = "Anahata ASI Official";

    /**
     * Category display name rendered for the development snapshot provider source.
     */
    public static final String CATEGORY_DEV_DISPLAY_NAME = "Anahata ASI Development";

    /**
     * Code name base for the core NetBeans plugin module.
     */
    public static final String STUDIO_CODE_NAME = "uno.anahata.asi.nb";

    /**
     * Private constructor to prevent direct instantiation of static utility class.
     */
    private AnahataUpdateCenterUtils() {
    }

    /**
     * Detects the active NetBeans IDE major version (e.g., "30", "31") using a robust
     * multi-tiered detection strategy.
     * <ol>
     * <li>Checks the NetBeans user directory name (e.g. {@code ~/.netbeans/30}).</li>
     * <li>Checks the {@code netbeans.buildnumber} system property (e.g. {@code 30-46c1feab...}).</li>
     * <li>Checks the {@code netbeans.productversion} system property (e.g. {@code Apache NetBeans IDE 30}).</li>
     * </ol>
     *
     * @return The detected major NetBeans version string (e.g. "30"), or {@code null} if running
     *         in an unrecognized or custom NetBeans Platform/RCP environment.
     */
    public static String getNetBeansMajorVersion() {
        // Tier 1: Check user directory name (most deterministic on standard IDE installs)
        try {
            File userDir = Places.getUserDirectory();
            if (userDir != null && userDir.getName().matches("^\\d+$")) {
                return userDir.getName();
            }
        } catch (Exception ex) {
            LOG.log(Level.FINE, "Failed to resolve NetBeans user directory for version detection", ex);
        }

        // Tier 2: Check netbeans.buildnumber property (e.g. "30-46c1feab2cb98b58ae1eccb4f9fba1c29137cf5d")
        String buildNumber = System.getProperty("netbeans.buildnumber");
        if (buildNumber != null) {
            Matcher matcher = Pattern.compile("^(\\d+)").matcher(buildNumber);
            if (matcher.find()) {
                return matcher.group(1);
            }
        }

        // Tier 3: Check netbeans.productversion property (e.g. "Apache NetBeans IDE 30")
        String productVersion = System.getProperty("netbeans.productversion");
        if (productVersion != null) {
            Matcher matcher = Pattern.compile("(?i)NetBeans(?:\\s+IDE)?\\s+(\\d+)").matcher(productVersion);
            if (matcher.find()) {
                return matcher.group(1);
            }
        }

        return null;
    }

    /**
     * Resolves the official Stable Update Center URL for the given NetBeans major version.
     *
     * @param major The NetBeans major version string (e.g., "30").
     * @return The absolute URL string to the stable updates.xml catalog.
     */
    public static String getStableUpdateUrl(String major) {
        return "https://asi.anahata.uno/nb/" + major + "/updates.xml";
    }

    /**
     * Resolves the official Development Snapshot Update Center URL for the given NetBeans major version.
     *
     * @param major The NetBeans major version string (e.g., "30").
     * @return The absolute URL string to the dev-updates.xml catalog.
     */
    public static String getDevUpdateUrl(String major) {
        return "https://asi.anahata.uno/nb/" + major + "/dev-updates.xml";
    }

    /**
     * Registers both Stable and Dev Snapshot Update Centers in the IDE if not already registered.
     * <p>
     * - The Stable Update Center is enabled by default upon initial registration. If the user subsequently
     *   disables it, its enabled status will not be overridden.
     * - The Dev Snapshot Update Center is registered in a disabled state by default.
     * </p>
     */
    public static void registerDefaultUpdateCenter() {
        try {
            String major = getNetBeansMajorVersion();
            if (major == null) {
                LOG.log(Level.INFO, "Could not determine NetBeans major version (e.g. custom RCP application). Skipping Update Center auto-registration.");
                return;
            }

            UpdateUnitProviderFactory factory = UpdateUnitProviderFactory.getDefault();
            List<UpdateUnitProvider> providers = factory.getUpdateUnitProviders(false);

            // 1. Stable Update Center
            String stableUrlStr = getStableUpdateUrl(major);
            URL stableUrl = new URL(stableUrlStr);
            String stableDisplayName = "Anahata ASI (NB " + major + ") - Stable";
            UpdateUnitProvider existingStable = findProvider(providers, PROVIDER_CODENAME, stableUrlStr);

            if (existingStable == null) {
                LOG.log(Level.INFO, "Auto-registering Anahata ASI Stable Update Center: {0} -> {1}", new Object[]{stableDisplayName, stableUrlStr});
                UpdateUnitProvider createdStable = factory.create(PROVIDER_CODENAME, stableDisplayName, stableUrl, ICON_BASE, CATEGORY_DISPLAY_NAME);
                createdStable.setEnable(true);
                LOG.log(Level.INFO, "Successfully registered and enabled Anahata Stable Update Center [{0}]", createdStable.getName());
            }

            // 2. Dev Snapshot Update Center (Disabled by default)
            String devUrlStr = getDevUpdateUrl(major);
            URL devUrl = new URL(devUrlStr);
            String devDisplayName = "Anahata ASI (NB " + major + ") - Dev Snapshot";
            UpdateUnitProvider existingDev = findProvider(providers, PROVIDER_CODENAME_DEV, devUrlStr);

            if (existingDev == null) {
                LOG.log(Level.INFO, "Auto-registering Anahata ASI Dev Snapshot Update Center: {0} -> {1}", new Object[]{devDisplayName, devUrlStr});
                UpdateUnitProvider createdDev = factory.create(PROVIDER_CODENAME_DEV, devDisplayName, devUrl, ICON_BASE, CATEGORY_DEV_DISPLAY_NAME);
                createdDev.setEnable(false);
                LOG.log(Level.INFO, "Successfully registered Anahata Dev Snapshot Update Center [{0}] (disabled by default)", createdDev.getName());
            }
        } catch (Exception ex) {
            LOG.log(Level.WARNING, "Failed to auto-register Anahata Update Centers", ex);
        }
    }

    /**
     * Searches a list of update unit providers for a matching code name or URL.
     *
     * @param providers The list of providers to search.
     * @param codeName The provider code name to look for.
     * @param urlStr The provider URL string to look for.
     * @return The matching provider, or {@code null} if not found.
     */
    private static UpdateUnitProvider findProvider(List<UpdateUnitProvider> providers, String codeName, String urlStr) {
        for (UpdateUnitProvider p : providers) {
            if (codeName.equals(p.getName())
                    || (p.getProviderURL() != null && urlStr.equalsIgnoreCase(p.getProviderURL().toExternalForm()))) {
                return p;
            }
        }
        return null;
    }

    /**
     * Retrieves the specified Anahata {@link UpdateUnitProvider} if registered.
     *
     * @param dev {@code true} for the Dev Snapshot center, {@code false} for the Stable center.
     * @return The registered {@link UpdateUnitProvider}, or {@code null} if not registered.
     */
    public static UpdateUnitProvider getUpdateUnitProvider(boolean dev) {
        String major = getNetBeansMajorVersion();
        String targetCodeName = dev ? PROVIDER_CODENAME_DEV : PROVIDER_CODENAME;
        String targetUrl = dev ? (major != null ? getDevUpdateUrl(major) : null) : (major != null ? getStableUpdateUrl(major) : null);

        List<UpdateUnitProvider> providers = UpdateUnitProviderFactory.getDefault().getUpdateUnitProviders(false);
        return findProvider(providers, targetCodeName, targetUrl != null ? targetUrl : "");
    }

    /**
     * Checks whether the specified Anahata Update Center is registered in the IDE.
     *
     * @param dev {@code true} for Dev Snapshot, {@code false} for Stable.
     * @return {@code true} if registered, {@code false} otherwise.
     */
    public static boolean isUpdateCenterRegistered(boolean dev) {
        return getUpdateUnitProvider(dev) != null;
    }

    /**
     * Checks whether the specified Anahata Update Center is registered and enabled.
     *
     * @param dev {@code true} for Dev Snapshot, {@code false} for Stable.
     * @return {@code true} if enabled, {@code false} otherwise.
     */
    public static boolean isUpdateCenterEnabled(boolean dev) {
        UpdateUnitProvider provider = getUpdateUnitProvider(dev);
        return provider != null && provider.isEnabled();
    }

    /**
     * Sets the enabled state of the specified Anahata Update Center.
     *
     * @param dev {@code true} for Dev Snapshot, {@code false} for Stable.
     * @param enabled The desired enabled state.
     */
    public static void setUpdateCenterEnabled(boolean dev, boolean enabled) {
        UpdateUnitProvider provider = getUpdateUnitProvider(dev);
        if (provider != null) {
            provider.setEnable(enabled);
        }
    }

    /**
     * Gets the installed specification version of the Anahata ASI Studio plugin.
     *
     * @return The version string (e.g., "1.1.2"), or {@code null} if not found.
     */
    public static String getInstalledPluginVersion() {
        for (UpdateUnit unit : UpdateManager.getDefault().getUpdateUnits(UpdateManager.TYPE.MODULE)) {
            if (STUDIO_CODE_NAME.equals(unit.getCodeName()) && unit.getInstalled() != null) {
                return unit.getInstalled().getSpecificationVersion();
            }
        }
        return null;
    }

    /**
     * Locates the newest available {@link UpdateElement} for the Anahata ASI Studio plugin across
     * all active update centers.
     *
     * @return The latest update element, or {@code null} if no updates are available.
     */
    public static UpdateElement getAvailablePluginUpdate() {
        for (UpdateUnit unit : UpdateManager.getDefault().getUpdateUnits(UpdateManager.TYPE.MODULE)) {
            if (STUDIO_CODE_NAME.equals(unit.getCodeName())) {
                List<UpdateElement> updates = unit.getAvailableUpdates();
                if (updates != null && !updates.isEmpty()) {
                    return updates.get(0);
                }
            }
        }
        return null;
    }

    /**
     * Refreshes all active Anahata update unit providers against remote catalogs.
     */
    private static void refreshAnahataProviders() {
        for (UpdateUnitProvider p : UpdateUnitProviderFactory.getDefault().getUpdateUnitProviders(true)) {
            if (PROVIDER_CODENAME.equals(p.getName()) || PROVIDER_CODENAME_DEV.equals(p.getName())) {
                try {
                    p.refresh(null, true);
                } catch (IOException ex) {
                    LOG.log(Level.WARNING, "Failed to refresh update provider: " + p.getDisplayName(), ex);
                }
            }
        }
    }

    /**
     * Checks if updates are available for the Anahata ASI Studio plugin and returns a summary report.
     *
     * @param forceRefresh Whether to force a network refresh of the registered update catalogs.
     * @return A human-readable summary of the update check results.
     * @throws Exception if an error occurs while checking for updates.
     */
    public static String checkForUpdates(boolean forceRefresh) throws Exception {
        if (forceRefresh) {
            refreshAnahataProviders();
        }

        String currentVersion = getInstalledPluginVersion();
        UpdateElement availableUpdate = getAvailablePluginUpdate();

        StringBuilder sb = new StringBuilder();
        sb.append("Anahata ASI Studio Plugin Update Status:\n");
        sb.append("- NetBeans Major Version: ").append(getNetBeansMajorVersion()).append("\n");
        sb.append("- Installed Version: ").append(currentVersion != null ? currentVersion : "Not detected").append("\n");

        if (availableUpdate != null) {
            sb.append("- Available Update: ").append(availableUpdate.getSpecificationVersion())
                    .append(" (from ").append(availableUpdate.getSourceDescription()).append(")\n");
            sb.append("- Action: Run IDE.upgradePlugin to install this update.");
        } else {
            sb.append("- Available Update: None (Plugin is up to date).");
        }

        return sb.toString();
    }

    /**
     * Downloads, validates, and installs the latest available update for the Anahata ASI Studio plugin.
     *
     * @return A status message describing the outcome of the update operation.
     * @throws Exception if the update process fails.
     */
    public static String performPluginUpdate() throws Exception {
        UpdateElement updateElement = getAvailablePluginUpdate();
        if (updateElement == null) {
            return "No updates available for Anahata ASI Studio.";
        }

        OperationContainer<InstallSupport> container = OperationContainer.createForUpdate();
        OperationContainer.OperationInfo<InstallSupport> info = container.add(updateElement);
        if (info == null) {
            throw new AgiToolException("Unable to add update element to container: " + updateElement.getCodeName());
        }

        if (info.getRequiredElements() != null && !info.getRequiredElements().isEmpty()) {
            container.add(info.getRequiredElements());
        }

        InstallSupport support = container.getSupport();
        InstallSupport.Validator validator = support.doDownload(null, false, true);
        InstallSupport.Installer installer = support.doValidate(validator, null);
        Restarter restarter = support.doInstall(installer, null);

        if (restarter != null) {
            support.doRestartLater(restarter);
            return "Successfully downloaded and installed update to version " + updateElement.getSpecificationVersion()
                    + ". The update will be activated on next IDE restart.";
        }

        return "Successfully installed Anahata ASI Studio update to version " + updateElement.getSpecificationVersion() + ".";
    }

    /**
     * Generates a concise Markdown status report of the Anahata ASI Studio plugin and its update centers
     * for prompt augmentation in the RAG message.
     *
     * @return A formatted Markdown string containing update center and plugin status.
     */
    public static String getPluginUpdateStatusMarkdown() {
        String major = getNetBeansMajorVersion();
        String currentVersion = getInstalledPluginVersion();
        UpdateElement availableUpdate = getAvailablePluginUpdate();

        UpdateUnitProvider stableProvider = getUpdateUnitProvider(false);
        UpdateUnitProvider devProvider = getUpdateUnitProvider(true);

        StringBuilder sb = new StringBuilder();
        sb.append("## Anahata ASI NetBeans Plugin & Update Status\n");
        sb.append("- **Host NetBeans Generation**: ").append(major != null ? "NetBeans " + major : "Unknown").append("\n");
        sb.append("- **Installed Plugin Version**: ").append(currentVersion != null ? currentVersion : "Development Build").append("\n");

        if (stableProvider != null) {
            sb.append("- **Stable Update Center**: ")
                    .append(stableProvider.isEnabled() ? "✅ Enabled" : "⏸️ Disabled by user")
                    .append(" (").append(stableProvider.getProviderURL()).append(")\n");
        } else {
            sb.append("- **Stable Update Center**: ❌ Not registered\n");
        }

        if (devProvider != null) {
            sb.append("- **Dev Snapshot Update Center**: ")
                    .append(devProvider.isEnabled() ? "✅ Enabled" : "⏸️ Disabled (Default)")
                    .append(" (").append(devProvider.getProviderURL()).append(")\n");
        } else {
            sb.append("- **Dev Snapshot Update Center**: ❌ Not registered\n");
        }

        if (availableUpdate != null) {
            sb.append("- **Update Available**: 🚀 Version ").append(availableUpdate.getSpecificationVersion())
                    .append(" available. Use `IDE.upgradePlugin` to install.\n");
        } else {
            sb.append("- **Update Available**: None (Up to date)\n");
        }

        return sb.toString();
    }
}
