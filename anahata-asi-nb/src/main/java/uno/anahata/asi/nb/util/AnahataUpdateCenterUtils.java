/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.util;

import java.io.File;
import java.net.URL;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.netbeans.api.autoupdate.UpdateUnitProvider;
import org.netbeans.api.autoupdate.UpdateUnitProviderFactory;
import org.openide.modules.Places;

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
     * Unique internal provider code name used by the NetBeans Autoupdate services.
     */
    public static final String PROVIDER_CODENAME = "anahata-asi-update-center";

    /**
     * Resource path on classpath for the Anahata 16x16 icon displayed in the Plugins manager.
     */
    public static final String ICON_BASE = "icons/anahata_16.png";

    /**
     * Category display name rendered for the provider source.
     */
    public static final String CATEGORY_DISPLAY_NAME = "Anahata ASI Official";

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
     * Registers and enables the official Anahata production Update Center in the IDE
     * if not already present, complete with the custom Anahata 16x16 icon.
     * <p>
     * The catalog URL is dynamically resolved to {@code https://asi.anahata.uno/nb/{major}/updates.xml}
     * matching the host IDE version. If the major version cannot be reliably determined (such as
     * inside a custom NetBeans Platform or RCP application), registration is safely skipped.
     * </p>
     */
    public static void registerDefaultUpdateCenter() {
        try {
            String major = getNetBeansMajorVersion();
            if (major == null) {
                LOG.log(Level.INFO, "Could not determine NetBeans major version (e.g. custom RCP application). Skipping Update Center auto-registration.");
                return;
            }

            String updateUrlStr = "https://asi.anahata.uno/nb/" + major + "/updates.xml";
            URL updateUrl = new URL(updateUrlStr);
            String displayName = "Anahata ASI (NB " + major + ")";

            UpdateUnitProviderFactory factory = UpdateUnitProviderFactory.getDefault();
            UpdateUnitProvider existing = null;

            for (UpdateUnitProvider p : factory.getUpdateUnitProviders(false)) {
                if (PROVIDER_CODENAME.equals(p.getName())
                        || (p.getProviderURL() != null && updateUrlStr.equalsIgnoreCase(p.getProviderURL().toExternalForm()))) {
                    existing = p;
                    break;
                }
            }

            if (existing == null) {
                LOG.log(Level.INFO, "Auto-registering Anahata ASI Update Center: {0} -> {1}", new Object[]{displayName, updateUrlStr});
                UpdateUnitProvider created = factory.create(PROVIDER_CODENAME, displayName, updateUrl, ICON_BASE, CATEGORY_DISPLAY_NAME);
                created.setEnable(true);
                LOG.log(Level.INFO, "Successfully registered and enabled Anahata Update Center [{0}] with custom icon", created.getName());
            } else {
                if (!existing.isEnabled()) {
                    existing.setEnable(true);
                    LOG.log(Level.INFO, "Enabled previously disabled Anahata Update Center [{0}]", existing.getDisplayName());
                }
            }
        } catch (Exception ex) {
            LOG.log(Level.WARNING, "Failed to auto-register Anahata Update Center", ex);
        }
    }
}
