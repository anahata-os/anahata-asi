/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.uc;

import java.io.File;
import java.io.IOException;
import java.net.HttpURLConnection;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.netbeans.api.autoupdate.InstallSupport;
import org.netbeans.api.autoupdate.OperationContainer;
import org.netbeans.api.autoupdate.OperationSupport;
import org.netbeans.api.autoupdate.OperationSupport.Restarter;
import org.netbeans.api.autoupdate.UpdateElement;
import org.netbeans.api.autoupdate.UpdateManager;
import org.netbeans.api.autoupdate.UpdateUnit;
import org.netbeans.api.autoupdate.UpdateUnitProvider;
import org.netbeans.api.autoupdate.UpdateUnitProviderFactory;
import org.openide.modules.ModuleInfo;
import org.openide.modules.Places;
import org.openide.util.Lookup;

/**
 * Universal utility class for managing NetBeans version detection, Anahata ASI Update Centers
 * (Universal, Stable, and Dev Snapshot), URL connectivity checks, legacy update center cleanup,
 * and automated installation of Anahata ASI Studio and JavaFX runtimes.
 *
 * @author anahata
 */
public final class AnahataUcUtils {

    /**
     * Logger instance for update center operations.
     */
    private static final Logger LOG = Logger.getLogger(AnahataUcUtils.class.getName());

    /**
     * Unique provider code name for the universal cross-version update center.
     */
    public static final String PROVIDER_CODENAME_UNIVERSAL = "anahata-asi-uc-universal";

    /**
     * Unique internal provider code name for the official stable update center.
     */
    public static final String PROVIDER_CODENAME_STABLE = "anahata-asi-update-center";

    /**
     * Unique internal provider code name for the development snapshot update center.
     */
    public static final String PROVIDER_CODENAME_DEV = "anahata-asi-dev-update-center";

    /**
     * Resource path on classpath for the Anahata 16x16 icon displayed in the Plugins manager.
     */
    public static final String ICON_BASE = "icons/anahata_16.png";

    /**
     * Category display name rendered for the provider sources.
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
     * Code name base for the standalone update center plugin module.
     */
    public static final String UC_CODE_NAME = "uno.anahata.asi.nb.uc";

    /**
     * Code name base for JavaFX runtime kit module.
     */
    public static final String JAVAFX_KIT_CODE_NAME = "org.netbeans.modules.javafx2.kit";

    /**
     * Universal update center catalog URL.
     */
    public static final String UNIVERSAL_UPDATE_URL = "https://asi.anahata.uno/nb/updates.xml";

    /**
     * Maven Central search URL for Anahata ASI Studio NetBeans module.
     */
    public static final String MAVEN_STUDIO_URL = "https://central.sonatype.com/artifact/uno.anahata/anahata-asi-nb";

    /**
     * Maven Central search URL for Anahata ASI Update Center module.
     */
    public static final String MAVEN_UC_URL = "https://central.sonatype.com/artifact/uno.anahata/anahata-asi-nb-uc";

    /**
     * Identification enum for supported Anahata Update Centers.
     */
    public enum UpdateCenterType {
        /**
         * Universal cross-version catalog serving the Update Center plugin.
         */
        UNIVERSAL,
        /**
         * Official production GA catalog for the current NetBeans major version.
         */
        STABLE,
        /**
         * Rolling continuous integration snapshot catalog for the current NetBeans major version.
         */
        DEV
    }

    /**
     * Private constructor to prevent direct instantiation.
     */
    private AnahataUcUtils() {
    }

    /**
     * Detects the active NetBeans IDE major version (e.g., "30", "31") using a robust
     * multi-tiered detection strategy.
     *
     * @return The detected major NetBeans version string (e.g. "30"), or {@code null} if running
     *         in an unrecognized environment.
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
     * Resolves the catalog URL for the given update center type.
     *
     * @param type The {@link UpdateCenterType}.
     * @return The URL string.
     */
    public static String getUpdateCenterUrl(UpdateCenterType type) {
        String major = getNetBeansMajorVersion();
        return switch (type) {
            case UNIVERSAL -> UNIVERSAL_UPDATE_URL;
            case STABLE -> major != null ? "https://asi.anahata.uno/nb/" + major + "/updates.xml" : "https://asi.anahata.uno/nb/updates.xml";
            case DEV -> major != null ? "https://asi.anahata.uno/nb/" + major + "/dev-updates.xml" : "https://asi.anahata.uno/nb/dev-updates.xml";
        };
    }

    /**
     * Resolves the default display name for the given update center type.
     *
     * @param type The {@link UpdateCenterType}.
     * @return The display name string.
     */
    public static String getUpdateCenterDisplayName(UpdateCenterType type) {
        String major = getNetBeansMajorVersion();
        String majorStr = major != null ? "NB " + major : "Generic";
        return switch (type) {
            case UNIVERSAL -> "Anahata ASI Update Center";
            case STABLE -> "Anahata ASI (" + majorStr + ") - Stable";
            case DEV -> "Anahata ASI (" + majorStr + ") - Dev Snapshot";
        };
    }

    /**
     * Resolves the internal provider code name for the given update center type.
     *
     * @param type The {@link UpdateCenterType}.
     * @return The code name string.
     */
    public static String getUpdateCenterCodeName(UpdateCenterType type) {
        return switch (type) {
            case UNIVERSAL -> PROVIDER_CODENAME_UNIVERSAL;
            case STABLE -> PROVIDER_CODENAME_STABLE;
            case DEV -> PROVIDER_CODENAME_DEV;
        };
    }

    /**
     * Resolves the description for the given update center type.
     *
     * @param type The {@link UpdateCenterType}.
     * @return The descriptive text.
     */
    public static String getUpdateCenterDescription(UpdateCenterType type) {
        String major = getNetBeansMajorVersion();
        return switch (type) {
            case UNIVERSAL -> "Cross-version catalog for the standalone Update Center & Installer plugin.";
            case STABLE -> "Official Production GA releases of Anahata ASI Studio for NetBeans " + (major != null ? major : "") + ".";
            case DEV -> "Continuous integration rolling snapshot builds of Anahata ASI Studio.";
        };
    }

    /**
     * Registers all standard Anahata Update Centers in NetBeans if not already registered.
     * <p>
     * - Universal Update Center: Registered and enabled by default.
     * - Stable Update Center: Registered and enabled by default.
     * - Dev Snapshot Update Center: Registered in a disabled state by default.
     * </p>
     */
    public static void registerDefaultUpdateCenters() {
        try {
            UpdateUnitProviderFactory factory = UpdateUnitProviderFactory.getDefault();
            List<UpdateUnitProvider> providers = factory.getUpdateUnitProviders(false);

            for (UpdateCenterType type : UpdateCenterType.values()) {
                String codeName = getUpdateCenterCodeName(type);
                String displayName = getUpdateCenterDisplayName(type);
                String urlStr = getUpdateCenterUrl(type);
                URL url = new URL(urlStr);

                UpdateUnitProvider existing = findProvider(providers, codeName, urlStr);
                if (existing == null) {
                    String categoryDisplayName = type == UpdateCenterType.DEV ? CATEGORY_DEV_DISPLAY_NAME : CATEGORY_DISPLAY_NAME;
                    UpdateUnitProvider created = factory.create(codeName, displayName, url, ICON_BASE, categoryDisplayName);
                    created.setEnable(type != UpdateCenterType.DEV);
                    LOG.log(Level.INFO, "Auto-registered Anahata Update Center: {0} ({1})", new Object[]{displayName, urlStr});
                }
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
    public static UpdateUnitProvider findProvider(List<UpdateUnitProvider> providers, String codeName, String urlStr) {
        for (UpdateUnitProvider p : providers) {
            if (codeName != null && codeName.equals(p.getName())) {
                return p;
            }
            if (urlStr != null && !urlStr.isEmpty() && p.getProviderURL() != null
                    && urlStr.equalsIgnoreCase(p.getProviderURL().toExternalForm())) {
                return p;
            }
        }
        return null;
    }

    /**
     * Retrieves the specified Anahata {@link UpdateUnitProvider} if registered.
     *
     * @param type The {@link UpdateCenterType}.
     * @return The registered {@link UpdateUnitProvider}, or {@code null} if not registered.
     */
    public static UpdateUnitProvider getUpdateUnitProvider(UpdateCenterType type) {
        String codeName = getUpdateCenterCodeName(type);
        String urlStr = getUpdateCenterUrl(type);
        List<UpdateUnitProvider> providers = UpdateUnitProviderFactory.getDefault().getUpdateUnitProviders(false);
        return findProvider(providers, codeName, urlStr);
    }

    /**
     * Detects any legacy/obsolete Anahata update unit providers registered from previous or different
     * NetBeans major versions that can be safely cleaned up.
     *
     * @return A list of legacy {@link UpdateUnitProvider} instances.
     */
    public static List<UpdateUnitProvider> getLegacyAnahataProviders() {
        String currentStableUrl = getUpdateCenterUrl(UpdateCenterType.STABLE);
        String currentDevUrl = getUpdateCenterUrl(UpdateCenterType.DEV);
        String universalUrl = getUpdateCenterUrl(UpdateCenterType.UNIVERSAL);

        List<UpdateUnitProvider> legacy = new ArrayList<>();
        List<UpdateUnitProvider> providers = UpdateUnitProviderFactory.getDefault().getUpdateUnitProviders(false);

        for (UpdateUnitProvider p : providers) {
            String name = p.getName() != null ? p.getName() : "";
            String displayName = p.getDisplayName() != null ? p.getDisplayName() : "";
            String urlStr = p.getProviderURL() != null ? p.getProviderURL().toExternalForm() : "";

            boolean isAnahata = name.contains("anahata") || displayName.contains("Anahata") || urlStr.contains("anahata.uno");
            if (isAnahata) {
                boolean isCurrent = urlStr.equalsIgnoreCase(currentStableUrl)
                        || urlStr.equalsIgnoreCase(currentDevUrl)
                        || urlStr.equalsIgnoreCase(universalUrl);
                if (!isCurrent) {
                    legacy.add(p);
                }
            }
        }

        return legacy;
    }

    /**
     * Removes an {@link UpdateUnitProvider} from NetBeans Autoupdate.
     *
     * @param provider The provider to remove.
     */
    public static void removeProvider(UpdateUnitProvider provider) {
        if (provider != null) {
            UpdateUnitProviderFactory.getDefault().remove(provider);
        }
    }

    /**
     * Checks whether the specified Anahata Update Center is registered in the IDE.
     *
     * @param type The {@link UpdateCenterType}.
     * @return {@code true} if registered, {@code false} otherwise.
     */
    public static boolean isUpdateCenterRegistered(UpdateCenterType type) {
        return getUpdateUnitProvider(type) != null;
    }

    /**
     * Checks whether the specified Anahata Update Center is registered and enabled.
     *
     * @param type The {@link UpdateCenterType}.
     * @return {@code true} if enabled, {@code false} otherwise.
     */
    public static boolean isUpdateCenterEnabled(UpdateCenterType type) {
        UpdateUnitProvider provider = getUpdateUnitProvider(type);
        return provider != null && provider.isEnabled();
    }

    /**
     * Sets the enabled state of the specified Anahata Update Center.
     *
     * @param type The {@link UpdateCenterType}.
     * @param enabled The desired enabled state.
     */
    public static void setUpdateCenterEnabled(UpdateCenterType type, boolean enabled) {
        UpdateUnitProvider provider = getUpdateUnitProvider(type);
        if (provider != null) {
            provider.setEnable(enabled);
        }
    }

    /**
     * Sets the trusted state of the specified Anahata Update Center.
     *
     * @param type The {@link UpdateCenterType}.
     * @param trusted The desired trusted state.
     */
    public static void setUpdateCenterTrusted(UpdateCenterType type, boolean trusted) {
        UpdateUnitProvider provider = getUpdateUnitProvider(type);
        if (provider != null) {
            provider.setTrusted(trusted);
        }
    }

    /**
     * Performs a lightweight HTTP network connectivity check against a remote URL.
     *
     * @param urlStr The URL to check.
     * @return A status description (e.g., "Online", "Offline (404 Not Found)", "Offline (Timeout)").
     */
    public static String checkUrlConnectivity(String urlStr) {
        if (urlStr == null || urlStr.isEmpty()) {
            return "Offline (No URL)";
        }
        try {
            URL url = new URL(urlStr);
            HttpURLConnection conn = (HttpURLConnection) url.openConnection();
            conn.setRequestMethod("HEAD");
            conn.setConnectTimeout(2500);
            conn.setReadTimeout(2500);
            conn.setInstanceFollowRedirects(true);
            conn.setRequestProperty("User-Agent", "Anahata-ASI-UpdateCenter/1.0");

            int code = conn.getResponseCode();
            if (code >= 200 && code < 400) {
                return "Online";
            }
            if (code == HttpURLConnection.HTTP_BAD_METHOD || code == 403) {
                // Fallback to minimal range GET if HEAD is forbidden
                conn = (HttpURLConnection) url.openConnection();
                conn.setRequestMethod("GET");
                conn.setRequestProperty("Range", "bytes=0-10");
                conn.setConnectTimeout(2500);
                conn.setReadTimeout(2500);
                conn.setRequestProperty("User-Agent", "Anahata-ASI-UpdateCenter/1.0");
                int getCode = conn.getResponseCode();
                if (getCode >= 200 && getCode < 400) {
                    return "Online";
                }
                return "Offline (HTTP " + getCode + ")";
            }
            if (code == 404) {
                return "Offline (404 Not Found)";
            }
            return "Offline (HTTP " + code + ")";
        } catch (java.net.SocketTimeoutException ex) {
            return "Offline (Timeout)";
        } catch (java.net.UnknownHostException ex) {
            return "Offline (DNS Unresolved)";
        } catch (Exception ex) {
            String msg = ex.getMessage();
            return "Offline (" + (msg != null ? msg : "Unreachable") + ")";
        }
    }

    /**
     * DTO capturing detailed version metadata for an installed NetBeans module.
     *
     * @param specVersion The specification version (e.g. "1.1.2").
     * @param implVersion The implementation version string (e.g. "1.1.2-20260824").
     * @param buildVersion The build version string (e.g. "202608241014").
     * @param enabled Whether the module is currently active/enabled.
     */
    public record InstalledModuleDetails(String specVersion, String implVersion, String buildVersion, boolean enabled) {
        public String toFormattedString() {
            StringBuilder sb = new StringBuilder("v").append(specVersion != null ? specVersion : "unknown");
            if (implVersion != null || buildVersion != null) {
                sb.append(" (");
                if (implVersion != null) {
                    sb.append("Impl: ").append(implVersion);
                }
                if (buildVersion != null) {
                    if (implVersion != null) {
                        sb.append(" | ");
                    }
                    sb.append("Build: ").append(buildVersion);
                }
                sb.append(")");
            }
            return sb.toString();
        }
    }

    /**
     * Retrieves detailed version metadata for the installed Anahata ASI Studio module.
     *
     * @return The {@link InstalledModuleDetails}, or {@code null} if not installed on disk.
     */
    public static InstalledModuleDetails getInstalledStudioDetails() {
        ModuleInfo mi = org.openide.modules.Modules.getDefault().findCodeNameBase(STUDIO_CODE_NAME);
        if (mi == null) {
            return null;
        }
        String spec = mi.getSpecificationVersion() != null ? mi.getSpecificationVersion().toString() : null;
        String impl = mi.getImplementationVersion();
        String build = mi.getBuildVersion();
        return new InstalledModuleDetails(spec, impl, build, mi.isEnabled());
    }

    /**
     * Retrieves detailed version metadata for the standalone Anahata Update Center module itself.
     *
     * @return The {@link InstalledModuleDetails}, or {@code null} if not found.
     */
    public static InstalledModuleDetails getInstalledUcDetails() {
        ModuleInfo mi = org.openide.modules.Modules.getDefault().findCodeNameBase(UC_CODE_NAME);
        if (mi == null) {
            return null;
        }
        String spec = mi.getSpecificationVersion() != null ? mi.getSpecificationVersion().toString() : null;
        String impl = mi.getImplementationVersion();
        String build = mi.getBuildVersion();
        return new InstalledModuleDetails(spec, impl, build, mi.isEnabled());
    }

    /**
     * Formats a byte size into a human-readable string (e.g. "137.0 MB" or "450 KB").
     *
     * @param bytes The size in bytes.
     * @return The formatted string, or {@code null} if size is zero or unknown.
     */
    public static String formatDownloadSize(int bytes) {
        if (bytes <= 0) {
            return null;
        }
        if (bytes >= 1024 * 1024) {
            return String.format(java.util.Locale.US, "%.1f MB", bytes / (1024.0 * 1024.0));
        }
        return String.format(java.util.Locale.US, "%,d KB", bytes / 1024);
    }

    /**
     * Gets the installed specification version of the Anahata ASI Studio plugin.
     *
     * @return The version string (e.g., "1.1.2"), or {@code null} if not installed.
     */
    public static String getInstalledStudioVersion() {
        for (UpdateUnit unit : UpdateManager.getDefault().getUpdateUnits(UpdateManager.TYPE.MODULE)) {
            if (STUDIO_CODE_NAME.equals(unit.getCodeName()) && unit.getInstalled() != null) {
                return unit.getInstalled().getSpecificationVersion();
            }
        }
        return null;
    }

    /**
     * Checks whether the Anahata ASI Studio plugin is installed on disk.
     *
     * @return {@code true} if installed, {@code false} otherwise.
     */
    public static boolean isStudioInstalled() {
        return getInstalledStudioVersion() != null;
    }

    /**
     * Checks whether the Anahata ASI Studio plugin is installed and enabled.
     *
     * @return {@code true} if installed and active, {@code false} otherwise.
     */
    public static boolean isStudioEnabled() {
        for (UpdateUnit unit : UpdateManager.getDefault().getUpdateUnits(UpdateManager.TYPE.MODULE)) {
            if (STUDIO_CODE_NAME.equals(unit.getCodeName()) && unit.getInstalled() != null) {
                return unit.getInstalled().isEnabled();
            }
        }
        return false;
    }

    /**
     * Locates any available update or install element for the Anahata ASI Studio plugin that originates
     * from a specific update center provider.
     *
     * @param type The {@link UpdateCenterType} to check.
     * @return The matching {@link UpdateElement}, or {@code null} if none found in this provider.
     */
    public static UpdateElement getAvailableStudioElementForProvider(UpdateCenterType type) {
        UpdateUnitProvider provider = getUpdateUnitProvider(type);
        if (provider == null || !provider.isEnabled()) {
            return null;
        }

        for (UpdateUnit unit : provider.getUpdateUnits(UpdateManager.TYPE.MODULE)) {
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
     * Locates the newest available {@link UpdateElement} for the Anahata ASI Studio plugin across
     * all active update centers.
     *
     * @return The latest update element, or {@code null} if no updates/install elements are available.
     */
    public static UpdateElement getAvailableStudioElement() {
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
    public static void refreshAnahataProviders() {
        for (UpdateUnitProvider p : UpdateUnitProviderFactory.getDefault().getUpdateUnitProviders(true)) {
            String name = p.getName() != null ? p.getName() : "";
            if (name.contains("anahata")) {
                try {
                    p.refresh(null, true);
                } catch (IOException ex) {
                    LOG.log(Level.WARNING, "Failed to refresh update provider: " + p.getDisplayName(), ex);
                }
            }
        }
    }

    /**
     * Installs or updates the Anahata ASI Studio module using NetBeans Autoupdate API.
     *
     * @param element The {@link UpdateElement} to install.
     * @return A status message describing the outcome of the operation.
     * @throws Exception if installation fails.
     */
    public static String installOrUpdateStudio(UpdateElement element) throws Exception {
        if (element == null) {
            throw new IllegalArgumentException("UpdateElement must not be null.");
        }

        OperationContainer<InstallSupport> container = isStudioInstalled()
                ? OperationContainer.createForUpdate()
                : OperationContainer.createForInstall();

        OperationContainer.OperationInfo<InstallSupport> info = container.add(element);
        if (info == null) {
            throw new Exception("Unable to schedule installation for: " + element.getCodeName());
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
            return "Successfully downloaded and installed Anahata ASI Studio v" + element.getSpecificationVersion()
                    + ". It will be fully activated on next IDE restart.";
        }

        return "Successfully installed Anahata ASI Studio v" + element.getSpecificationVersion() + "!";
    }

    /**
     * Retrieves a comprehensive, human-readable description of the host Java runtime platform.
     *
     * @return The formatted Java platform string (e.g. "OpenJDK Runtime Environment 26.0.1 (Eclipse Adoptium)").
     */
    public static String getJavaPlatformDescription() {
        String runtimeName = System.getProperty("java.runtime.name");
        String javaVersion = System.getProperty("java.version");
        String javaVendor = System.getProperty("java.vendor");

        StringBuilder sb = new StringBuilder();
        if (runtimeName != null && !runtimeName.isEmpty()) {
            sb.append(runtimeName);
        } else {
            sb.append("Java");
        }
        if (javaVersion != null && !javaVersion.isEmpty()) {
            sb.append(" ").append(javaVersion);
        }
        if (javaVendor != null && !javaVendor.isEmpty()) {
            sb.append(" (").append(javaVendor).append(")");
        }
        return sb.toString();
    }

    /**
     * Resolves the formatted JavaFX status string for NetBeans environment display.
     * <p>
     * Returns "Version (JDK)" if provided by the host JVM, "Version (NB Module)" if provided
     * by a NetBeans module, or "Not available" if absent.
     * </p>
     *
     * @return The formatted JavaFX status string.
     */
    public static String getJavaFxFormattedStatus() {
        if (isSystemJdkJavaFx()) {
            String ver = getSystemJdkJavaFxVersion();
            return (ver != null ? ver : "Active") + " (JDK)";
        }

        for (ModuleInfo mi : Lookup.getDefault().lookupAll(ModuleInfo.class)) {
            if (mi.isEnabled()) {
                for (String token : mi.getProvides()) {
                    if ("org.openide.modules.jre.JavaFX".equals(token)) {
                        String ver = null;
                        if (mi.getClassLoader() != null) {
                            try {
                                Class<?> versionInfo = mi.getClassLoader().loadClass("com.sun.javafx.runtime.VersionInfo");
                                ver = (String) versionInfo.getMethod("getVersion").invoke(null);
                            } catch (Throwable ignored) {
                            }
                        }
                        if (ver == null && mi.getSpecificationVersion() != null) {
                            ver = mi.getSpecificationVersion().toString();
                        }
                        return (ver != null ? ver : "Active") + " (NB Module)";
                    }
                }
            }
        }
        return "Not available";
    }

    /**
     * Checks whether JavaFX is bundled directly in the host system JDK (e.g., Liberica Full JDK or Azul Zulu FX).
     * <p>
     * Because this module does not declare a module dependency on NetBeans' JavaFX module, standard
     * {@code Class.forName("javafx.application.Platform")} will ONLY find JavaFX if it is provided
     * by the underlying JDK.
     * </p>
     *
     * @return {@code true} if JavaFX classes are available directly from the host JDK.
     */
    public static boolean isSystemJdkJavaFx() {
        try {
            Class.forName("javafx.application.Platform");
            return true;
        } catch (Throwable ignored) {
            return false;
        }
    }

    /**
     * Resolves the version of JavaFX if bundled in the host JDK.
     *
     * @return The JavaFX version string, or {@code null} if not found.
     */
    private static String getSystemJdkJavaFxVersion() {
        try {
            Class<?> versionInfo = Class.forName("com.sun.javafx.runtime.VersionInfo");
            return (String) versionInfo.getMethod("getVersion").invoke(null);
        } catch (Throwable ignored) {
        }
        return System.getProperty("javafx.version");
    }

    /**
     * Checks whether JavaFX runtime support is installed and enabled in NetBeans via plugin modules.
     *
     * @return {@code true} if a NetBeans module providing {@code org.openide.modules.jre.JavaFX} is active.
     */
    public static boolean isNetBeansJavaFxModuleActive() {
        for (ModuleInfo mi : Lookup.getDefault().lookupAll(ModuleInfo.class)) {
            if (mi.isEnabled()) {
                for (String token : mi.getProvides()) {
                    if ("org.openide.modules.jre.JavaFX".equals(token)) {
                        return true;
                    }
                }
            }
        }
        return false;
    }

    /**
     * Checks whether JavaFX runtime support is active anywhere in the runtime (System JDK or NetBeans Module).
     *
     * @return {@code true} if active, {@code false} otherwise.
     */
    public static boolean isJavaFxActive() {
        return isSystemJdkJavaFx() || isNetBeansJavaFxModuleActive();
    }

    /**
     * Gets the JavaFX version string and origin descriptor if available.
     *
     * @return The JavaFX version (e.g., "21.0.9 (System JDK)" or "21.0.9 (org.netbeans.modules.javafx2.kit)"), or null.
     */
    public static String getJavaFxVersion() {
        if (isSystemJdkJavaFx()) {
            try {
                Class<?> versionInfo = Class.forName("com.sun.javafx.runtime.VersionInfo");
                return (String) versionInfo.getMethod("getVersion").invoke(null) + " (System JDK)";
            } catch (Throwable ignored) {
                return "Active (System JDK)";
            }
        }

        for (ModuleInfo mi : Lookup.getDefault().lookupAll(ModuleInfo.class)) {
            if (mi.isEnabled()) {
                for (String token : mi.getProvides()) {
                    if ("org.openide.modules.jre.JavaFX".equals(token)) {
                        if (mi.getClassLoader() != null) {
                            try {
                                Class<?> versionInfo = mi.getClassLoader().loadClass("com.sun.javafx.runtime.VersionInfo");
                                return (String) versionInfo.getMethod("getVersion").invoke(null) + " (" + mi.getCodeNameBase() + ")";
                            } catch (Throwable ignored) {
                            }
                        }
                        return mi.getSpecificationVersion() != null ? mi.getSpecificationVersion().toString() : "Active (NetBeans Module)";
                    }
                }
            }
        }
        return null;
    }

    /**
     * Installs or activates the NetBeans JavaFX runtime support module.
     *
     * @return The outcome status message.
     * @throws Exception if activation fails.
     */
    public static String installOrActivateJavaFx() throws Exception {
        if (isSystemJdkJavaFx()) {
            return "JavaFX is already natively available in the host JDK.";
        }

        UpdateUnit unit = UpdateManager.getDefault().getUpdateUnits(UpdateManager.TYPE.MODULE)
                .stream()
                .filter(u -> JAVAFX_KIT_CODE_NAME.equals(u.getCodeName()))
                .findFirst()
                .orElseThrow(() -> new Exception("JavaFX 2 Support module (org.netbeans.modules.javafx2.kit) not found."));

        UpdateElement installed = unit.getInstalled();
        if (installed != null) {
            if (installed.isEnabled()) {
                return "JavaFX support is already active.";
            }
            OperationContainer<OperationSupport> container = OperationContainer.createForEnable();
            OperationContainer.OperationInfo<OperationSupport> info = container.add(installed);
            if (info != null) {
                container.add(info.getRequiredElements());
                container.getSupport().doOperation(null);
                return "JavaFX runtime support successfully activated!";
            }
        } else if (!unit.getAvailableUpdates().isEmpty()) {
            UpdateElement toInstall = unit.getAvailableUpdates().get(0);
            OperationContainer<InstallSupport> container = OperationContainer.createForInstall();
            OperationContainer.OperationInfo<InstallSupport> info = container.add(toInstall);
            if (info != null) {
                container.add(info.getRequiredElements());
                InstallSupport support = container.getSupport();
                InstallSupport.Validator validator = support.doDownload(null, false, true);
                InstallSupport.Installer installer = support.doValidate(validator, null);
                Restarter restarter = support.doInstall(installer, null);
                if (restarter != null) {
                    support.doRestartLater(restarter);
                }
                return "JavaFX runtime support installed successfully!";
            }
        }

        throw new Exception("Unable to install or enable JavaFX support.");
    }
}
