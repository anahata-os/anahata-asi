/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.uc;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Cursor;
import java.awt.Desktop;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Image;
import java.awt.Insets;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.net.URI;
import java.net.URL;
import java.util.EnumMap;
import java.util.List;
import java.util.Map;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.JScrollPane;
import javax.swing.JToggleButton;
import javax.swing.SwingWorker;
import org.netbeans.api.autoupdate.UpdateElement;
import org.netbeans.api.autoupdate.UpdateUnitProvider;
import org.openide.awt.Actions;
import org.openide.awt.HtmlBrowser;
import org.openide.util.ImageUtilities;
import uno.anahata.asi.nb.uc.AnahataUcUtils.UpdateCenterType;

/**
 * Main presentation panel for the Anahata ASI NetBeans Update Center.
 * <p>
 * Displays the host NetBeans environment, manages official Universal, Stable, and Dev Update
 * Catalogs with live online/offline reachability indicators, provides 1-click installation
 * and upgrading of Anahata ASI Studio directly within the source catalog cards, and controls
 * JavaFX runtime activation.
 * </p>
 *
 * @author anahata
 */
public class AnahataUpdateCenterPanel extends JPanel {

    /**
     * Environment version label.
     */
    private final JLabel lblEnvironment = new JLabel();

    /**
     * UI control holders for each {@link UpdateCenterType}.
     */
    private final Map<UpdateCenterType, JToggleButton> toggleButtons = new EnumMap<>(UpdateCenterType.class);
    private final Map<UpdateCenterType, JCheckBox> trustCheckBoxes = new EnumMap<>(UpdateCenterType.class);
    private final Map<UpdateCenterType, JLabel> statusDotLabels = new EnumMap<>(UpdateCenterType.class);
    private final Map<UpdateCenterType, JLabel> statusTextLabels = new EnumMap<>(UpdateCenterType.class);
    private final Map<UpdateCenterType, JLabel> urlLabels = new EnumMap<>(UpdateCenterType.class);
    private final Map<UpdateCenterType, JButton> updateButtons = new EnumMap<>(UpdateCenterType.class);
    private final Map<UpdateCenterType, JLabel> packageDetailsLabels = new EnumMap<>(UpdateCenterType.class);
    private final Map<UpdateCenterType, JLabel> notificationLabels = new EnumMap<>(UpdateCenterType.class);

    /**
     * Anahata ASI Studio installed version label.
     */
    private final JLabel lblStudioInstalled = new JLabel();

    /**
     * JavaFX runtime status label.
     */
    private final JLabel lblJavaFxStatus = new JLabel();

    /**
     * Button to install or activate NetBeans JavaFX runtime.
     */
    private final JButton btnJavaFxAction = new JButton();

    /**
     * Container panel for legacy update center notifications.
     */
    private final JPanel pnlLegacyContainer = new JPanel();

    /**
     * Progress bar for asynchronous background catalog operations.
     */
    private final JProgressBar progressBar = new JProgressBar();

    /**
     * Global status text label.
     */
    private final JLabel lblGlobalStatus = new JLabel("Ready.");

    /**
     * Check for updates button.
     */
    private final JButton btnCheckUpdates = new JButton("Check for Updates Now");

    /**
     * Constructs the Anahata Update Center presentation panel and initializes UI components.
     */
    public AnahataUpdateCenterPanel() {
        super(new BorderLayout());
        initComponents();
        refreshAllStateAsync(false);
    }

    /**
     * Builds and lays out the entire Swing component hierarchy.
     */
    private void initComponents() {
        JPanel contentPanel = new JPanel();
        contentPanel.setLayout(new BoxLayout(contentPanel, BoxLayout.Y_AXIS));
        contentPanel.setBorder(BorderFactory.createEmptyBorder(10, 14, 10, 14));
        contentPanel.setOpaque(false);

        // 1. Header Section with Logo on the Right and Title on the Left (Pinned in single compact row)
        contentPanel.add(buildHeaderPanel());
        contentPanel.add(Box.createVerticalStrut(8));

        // 2. Host Environment Card (Studio left, NetBeans right)
        contentPanel.add(buildEnvironmentCard());
        contentPanel.add(Box.createVerticalStrut(8));

        // 3. Official Update Centers (Individual Card per Catalog)
        contentPanel.add(buildUpdateCentersSection());
        contentPanel.add(Box.createVerticalStrut(8));

        // 4. JavaFX Runtime Support Card
        contentPanel.add(buildJavaFxCard());
        contentPanel.add(Box.createVerticalStrut(8));

        // 5. Legacy Update Centers Card (Dynamic)
        pnlLegacyContainer.setLayout(new BoxLayout(pnlLegacyContainer, BoxLayout.Y_AXIS));
        pnlLegacyContainer.setOpaque(false);
        contentPanel.add(pnlLegacyContainer);

        // Pinned Viewport Wrapper: Placing contentPanel in BorderLayout.NORTH prevents any vertical stretching!
        JPanel viewportWrapper = new JPanel(new BorderLayout());
        viewportWrapper.setOpaque(false);
        viewportWrapper.add(contentPanel, BorderLayout.NORTH);

        JScrollPane scrollPane = new JScrollPane(viewportWrapper);
        scrollPane.setBorder(BorderFactory.createEmptyBorder());
        scrollPane.getVerticalScrollBar().setUnitIncrement(16);
        add(scrollPane, BorderLayout.CENTER);

        // Bottom Action Bar
        add(buildBottomBar(), BorderLayout.SOUTH);
    }

    /**
     * Builds the branding header banner containing title and subtitle on the left,
     * and the Anahata logo on the right, constrained to a compact single row.
     *
     * @return The header JPanel.
     */
    private JPanel buildHeaderPanel() {
        JPanel pnl = new JPanel(new BorderLayout(14, 0));
        pnl.setOpaque(false);
        pnl.setMaximumSize(new Dimension(32767, 50));

        // Left section: Title and Subtitle
        JPanel leftPnl = new JPanel();
        leftPnl.setLayout(new BoxLayout(leftPnl, BoxLayout.Y_AXIS));
        leftPnl.setOpaque(false);
        leftPnl.setAlignmentX(Component.LEFT_ALIGNMENT);

        JLabel lblTitle = new JLabel("Anahata ASI Update Center");
        lblTitle.setFont(lblTitle.getFont().deriveFont(Font.BOLD, 17f));
        lblTitle.setAlignmentX(Component.LEFT_ALIGNMENT);
        leftPnl.add(lblTitle);
        leftPnl.add(Box.createVerticalStrut(2));

        JLabel lblSubtitle = new JLabel("Cross-Version Update Management & Runtime Setup for Apache NetBeans");
        lblSubtitle.setFont(lblSubtitle.getFont().deriveFont(11.5f));
        lblSubtitle.setForeground(new Color(115, 115, 115));
        lblSubtitle.setAlignmentX(Component.LEFT_ALIGNMENT);
        leftPnl.add(lblSubtitle);

        pnl.add(leftPnl, BorderLayout.CENTER);

        // Right section: Logo
        Image logoImg = ImageUtilities.loadImage("icons/logo-horizontal.png");
        if (logoImg == null) {
            logoImg = ImageUtilities.loadImage("icons/anahata_32.png");
        }
        if (logoImg != null) {
            Image scaled = logoImg.getScaledInstance(-1, 38, Image.SCALE_SMOOTH);
            JLabel lblLogo = new JLabel(new ImageIcon(scaled));
            lblLogo.setVerticalAlignment(javax.swing.SwingConstants.CENTER);
            pnl.add(lblLogo, BorderLayout.EAST);
        }

        return pnl;
    }

    /**
     * Builds two separate side-by-side bordered panels on the same row:
     * - Left panel: "Anahata ASI Studio" (left-aligned version, implementation version, and build info).
     * - Right panel: "NetBeans" (right-aligned product, Java runtime platform, major version, and JavaFX status).
     *
     * @return The side-by-side JPanel container.
     */
    private JPanel buildEnvironmentCard() {
        JPanel container = new JPanel(new java.awt.GridLayout(1, 2, 10, 0));
        container.setOpaque(false);

        // 1. Left Card: Anahata ASI Studio (Left aligned)
        JPanel studioCard = createCardPanel("Anahata ASI Studio");
        studioCard.setLayout(new BorderLayout());
        lblStudioInstalled.setFont(lblStudioInstalled.getFont().deriveFont(11.5f));
        studioCard.add(lblStudioInstalled, BorderLayout.CENTER);
        container.add(studioCard);

        // 2. Right Card: NetBeans (Right aligned)
        JPanel netBeansCard = createCardPanel("NetBeans");
        netBeansCard.setLayout(new BorderLayout());
        lblEnvironment.setFont(lblEnvironment.getFont().deriveFont(11.5f));
        netBeansCard.add(lblEnvironment, BorderLayout.CENTER);
        container.add(netBeansCard);

        return container;
    }

    /**
     * Builds the container section containing an individual card for each Anahata update center.
     *
     * @return The section JPanel.
     */
    private JPanel buildUpdateCentersSection() {
        JPanel section = new JPanel();
        section.setLayout(new BoxLayout(section, BoxLayout.Y_AXIS));
        section.setOpaque(false);

        for (UpdateCenterType type : UpdateCenterType.values()) {
            section.add(buildUpdateCenterCard(type));
            section.add(Box.createVerticalStrut(8));
        }

        return section;
    }

    /**
     * Builds a distinct, highly visible card for a single update center catalog using a clean
     * 3-column layout:
     * - Column 1 (Left): Toggle button and centered Online/Offline status (vertically centered).
     * - Column 2 (Middle): Name + Trust, Description, URL link, live Maven Central shield badge, and alerts.
     * - Column 3 (Right): 1-Click Update action button with single-line Size & Released metadata (vertically centered).
     *
     * @param type The {@link UpdateCenterType}.
     * @return The configured JPanel card.
     */
    private JPanel buildUpdateCenterCard(UpdateCenterType type) {
        String title = switch (type) {
            case UNIVERSAL -> "Anahata ASI Update Center (Cross-Version)";
            case STABLE -> "Official Production Channel (Stable)";
            case DEV -> "Continuous Integration Channel (Dev Snapshots)";
        };

        JPanel card = createCardPanel(title);
        card.setLayout(new BorderLayout(12, 0));

        // =========================================================================
        // COLUMN 1: LEFT (Toggle Switch + Centered Status - Vertically Centered)
        // =========================================================================
        JPanel leftCol = new JPanel(new GridBagLayout());
        leftCol.setPreferredSize(new Dimension(115, 60));
        leftCol.setOpaque(false);

        JPanel leftInner = new JPanel();
        leftInner.setLayout(new BoxLayout(leftInner, BoxLayout.Y_AXIS));
        leftInner.setOpaque(false);

        JToggleButton btnToggle = new JToggleButton("Checking...", AnahataUcIcons.createDisabledIcon());
        btnToggle.setPreferredSize(new Dimension(115, 27));
        btnToggle.setMaximumSize(new Dimension(115, 27));
        btnToggle.setAlignmentX(Component.CENTER_ALIGNMENT);
        btnToggle.addActionListener(e -> onToggleCenter(type));
        toggleButtons.put(type, btnToggle);
        leftInner.add(btnToggle);
        leftInner.add(Box.createVerticalStrut(4));

        JLabel lblStatus = new JLabel("Checking...", javax.swing.SwingConstants.CENTER);
        lblStatus.setFont(lblStatus.getFont().deriveFont(Font.BOLD, 11f));
        lblStatus.setAlignmentX(Component.CENTER_ALIGNMENT);
        statusTextLabels.put(type, lblStatus);
        leftInner.add(lblStatus);

        leftCol.add(leftInner);
        card.add(leftCol, BorderLayout.WEST);

        // =========================================================================
        // COLUMN 2: MIDDLE (Name+Trust -> Description -> URL -> Maven Badge -> Notifications)
        // =========================================================================
        JPanel midCol = new JPanel();
        midCol.setLayout(new BoxLayout(midCol, BoxLayout.Y_AXIS));
        midCol.setOpaque(false);

        // Row 1: Name (with Anahata icon) + Trust Checkbox
        JPanel topMiddlePnl = new JPanel(new FlowLayout(FlowLayout.LEFT, 10, 0));
        topMiddlePnl.setOpaque(false);
        topMiddlePnl.setAlignmentX(Component.LEFT_ALIGNMENT);

        JLabel lblName = new JLabel("<html><b>" + AnahataUcUtils.getUpdateCenterDisplayName(type) + "</b></html>");
        lblName.setFont(lblName.getFont().deriveFont(12.5f));
        Image anahataIconImg = ImageUtilities.loadImage("icons/anahata_32.png");
        if (anahataIconImg == null) {
            anahataIconImg = ImageUtilities.loadImage("icons/anahata_16.png");
        }
        if (anahataIconImg != null) {
            Image scaled = anahataIconImg.getScaledInstance(18, 18, Image.SCALE_SMOOTH);
            lblName.setIcon(new ImageIcon(scaled));
            lblName.setIconTextGap(6);
        }
        topMiddlePnl.add(lblName);

        JCheckBox chkTrust = new JCheckBox("Trust (Auto-update)");
        chkTrust.setToolTipText("Trusts this update center to allow automated installations and suppress certificate warnings.");
        chkTrust.addActionListener(e -> onToggleTrust(type));
        trustCheckBoxes.put(type, chkTrust);
        topMiddlePnl.add(chkTrust);
        midCol.add(topMiddlePnl);
        midCol.add(Box.createVerticalStrut(2));

        // Row 2: Description
        JLabel lblDesc = new JLabel(AnahataUcUtils.getUpdateCenterDescription(type));
        lblDesc.setFont(lblDesc.getFont().deriveFont(11f));
        lblDesc.setForeground(new Color(110, 110, 110));
        lblDesc.setAlignmentX(Component.LEFT_ALIGNMENT);
        midCol.add(lblDesc);
        midCol.add(Box.createVerticalStrut(2));

        // Row 3: URL Link (Swapped below description)
        String urlStr = AnahataUcUtils.getUpdateCenterUrl(type);
        JLabel lblUrl = new JLabel("<html><font color='#2563eb'><u>" + urlStr + "</u></font></html>");
        lblUrl.setFont(lblUrl.getFont().deriveFont(11f));
        lblUrl.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
        lblUrl.setToolTipText("Click to view catalog XML in browser: " + urlStr);
        lblUrl.setAlignmentX(Component.LEFT_ALIGNMENT);
        lblUrl.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                openUrlInBrowser(urlStr);
            }
        });
        urlLabels.put(type, lblUrl);
        midCol.add(lblUrl);

        // Row 4: Live Maven Central Shield Badges (Universal and Stable only)
        if (type == UpdateCenterType.UNIVERSAL) {
            midCol.add(Box.createVerticalStrut(3));
            midCol.add(createLiveMavenShieldRow("https://img.shields.io/maven-central/v/uno.anahata/anahata-asi-nb-uc.png", AnahataUcUtils.MAVEN_UC_URL));
        } else if (type == UpdateCenterType.STABLE) {
            midCol.add(Box.createVerticalStrut(3));
            midCol.add(createLiveMavenShieldRow("https://img.shields.io/maven-central/v/uno.anahata/anahata-asi-nb.png", AnahataUcUtils.MAVEN_STUDIO_URL));
        }

        // Row 5: Notification / Release Alert Banner (if present)
        JLabel lblNotif = new JLabel();
        lblNotif.setFont(lblNotif.getFont().deriveFont(11f));
        lblNotif.setAlignmentX(Component.LEFT_ALIGNMENT);
        lblNotif.setVisible(false);
        notificationLabels.put(type, lblNotif);
        midCol.add(lblNotif);

        card.add(midCol, BorderLayout.CENTER);

        // =========================================================================
        // COLUMN 3: RIGHT (Update Action Button + Single-line Size & Date - Vertically Centered)
        // =========================================================================
        JPanel rightCol = new JPanel(new GridBagLayout());
        rightCol.setPreferredSize(new Dimension(245, 60));
        rightCol.setOpaque(false);

        JPanel rightInner = new JPanel();
        rightInner.setLayout(new BoxLayout(rightInner, BoxLayout.Y_AXIS));
        rightInner.setOpaque(false);

        JButton btnUpdate = new JButton("Update");
        btnUpdate.setIcon(AnahataUcIcons.createUpdateActionIcon());
        btnUpdate.setFont(btnUpdate.getFont().deriveFont(Font.BOLD, 12f));
        btnUpdate.setPreferredSize(new Dimension(240, 28));
        btnUpdate.setMaximumSize(new Dimension(240, 28));
        btnUpdate.setAlignmentX(Component.LEFT_ALIGNMENT);
        btnUpdate.setVisible(false);
        btnUpdate.addActionListener(e -> onInstallOrUpdateFromProvider(type));
        updateButtons.put(type, btnUpdate);
        rightInner.add(btnUpdate);
        rightInner.add(Box.createVerticalStrut(3));

        JLabel lblPkgDetails = new JLabel();
        lblPkgDetails.setFont(lblPkgDetails.getFont().deriveFont(10.5f));
        lblPkgDetails.setForeground(new Color(100, 100, 100));
        lblPkgDetails.setAlignmentX(Component.LEFT_ALIGNMENT);
        lblPkgDetails.setVisible(false);
        packageDetailsLabels.put(type, lblPkgDetails);
        rightInner.add(lblPkgDetails);

        rightCol.add(rightInner);
        card.add(rightCol, BorderLayout.EAST);

        return card;
    }

    /**
     * Creates a live Maven Central shield badge row with label prefix and asynchronous remote image fetch.
     *
     * @param badgeImageUrl The shields.io image URL.
     * @param targetPortalUrl The Sonatype/Maven Central portal URL opened on click.
     * @return The badge JPanel row.
     */
    private JPanel createLiveMavenShieldRow(String badgeImageUrl, String targetPortalUrl) {
        JPanel row = new JPanel(new FlowLayout(FlowLayout.LEFT, 6, 0));
        row.setOpaque(false);
        row.setAlignmentX(Component.LEFT_ALIGNMENT);

        JLabel lblPrefix = new JLabel("Latest in Maven Central:");
        lblPrefix.setFont(lblPrefix.getFont().deriveFont(Font.BOLD, 10.5f));
        lblPrefix.setForeground(new Color(90, 90, 90));
        row.add(lblPrefix);

        JLabel lblBadge = new JLabel();
        lblBadge.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
        lblBadge.setToolTipText("Open artifact on Maven Central (Sonatype): " + targetPortalUrl);
        lblBadge.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                openUrlInBrowser(targetPortalUrl);
            }
        });
        row.add(lblBadge);

        // Fetch live image asynchronously
        new SwingWorker<ImageIcon, Void>() {
            @Override
            protected ImageIcon doInBackground() throws Exception {
                return new ImageIcon(new URL(badgeImageUrl));
            }

            @Override
            protected void done() {
                try {
                    ImageIcon icon = get();
                    if (icon != null && icon.getIconWidth() > 0) {
                        lblBadge.setIcon(icon);
                        row.revalidate();
                        row.repaint();
                    }
                } catch (Exception ignored) {
                }
            }
        }.execute();

        return row;
    }

    /**
     * Builds the NetBeans JavaFX runtime activation card.
     *
     * @return The JavaFX support JPanel.
     */
    private JPanel buildJavaFxCard() {
        JPanel card = createCardPanel("JavaFX Runtime Support (NetBeans Platform)");
        card.setLayout(new GridBagLayout());
        GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.HORIZONTAL;
        c.insets = new Insets(4, 6, 4, 6);

        c.gridx = 0;
        c.gridy = 0;
        c.weightx = 1.0;
        JPanel infoPnl = new JPanel();
        infoPnl.setLayout(new BoxLayout(infoPnl, BoxLayout.Y_AXIS));
        infoPnl.setOpaque(false);

        lblJavaFxStatus.setFont(lblJavaFxStatus.getFont().deriveFont(Font.BOLD, 12.5f));
        infoPnl.add(lblJavaFxStatus);
        infoPnl.add(Box.createVerticalStrut(3));
        JLabel lblDesc = new JLabel("Allows the AI model and Java tool to execute interactive JavaFX scenes, 3D renderers, and charts.");
        lblDesc.setFont(lblDesc.getFont().deriveFont(11f));
        lblDesc.setForeground(new Color(110, 110, 110));
        infoPnl.add(lblDesc);
        card.add(infoPnl, c);

        c.gridx = 1;
        c.weightx = 0.0;
        btnJavaFxAction.setPreferredSize(new Dimension(190, 28));
        btnJavaFxAction.addActionListener(e -> onJavaFxAction());
        card.add(btnJavaFxAction, c);

        return card;
    }

    /**
     * Builds the bottom action bar containing global actions and status indicators.
     *
     * @return The bottom bar JPanel.
     */
    private JPanel buildBottomBar() {
        JPanel bar = new JPanel(new BorderLayout(10, 0));
        bar.setBorder(BorderFactory.createCompoundBorder(
                BorderFactory.createMatteBorder(1, 0, 0, 0, new Color(210, 210, 210)),
                BorderFactory.createEmptyBorder(8, 14, 8, 14)
        ));

        JPanel statusPnl = new JPanel(new FlowLayout(FlowLayout.LEFT, 8, 0));
        statusPnl.setOpaque(false);
        progressBar.setPreferredSize(new Dimension(100, 16));
        progressBar.setVisible(false);
        statusPnl.add(progressBar);
        statusPnl.add(lblGlobalStatus);
        bar.add(statusPnl, BorderLayout.WEST);

        JPanel btnPnl = new JPanel(new FlowLayout(FlowLayout.RIGHT, 8, 0));
        btnPnl.setOpaque(false);

        JButton btnOpenPlugins = new JButton("NetBeans Plugins", AnahataUcIcons.createPluginsManagerIcon());
        btnOpenPlugins.setToolTipText("Open standard NetBeans Plugins manager (Settings & Installed Plugins)");
        btnOpenPlugins.addActionListener(e -> {
            try {
                Actions.forID("System", "org.netbeans.modules.autoupdate.ui.actions.PluginManagerAction")
                        .actionPerformed(e);
            } catch (Exception ex) {
                JOptionPane.showMessageDialog(this, "Use Tools -> Plugins menu in NetBeans.", "Plugin Manager", JOptionPane.INFORMATION_MESSAGE);
            }
        });
        btnPnl.add(btnOpenPlugins);

        btnCheckUpdates.setIcon(AnahataUcIcons.createRefreshActionIcon());
        btnCheckUpdates.addActionListener(e -> refreshAllStateAsync(true));
        btnPnl.add(btnCheckUpdates);

        bar.add(btnPnl, BorderLayout.EAST);
        return bar;
    }

    /**
     * Helper to create a stylish bordered card panel with a distinct, high-contrast outline.
     *
     * @param title The card header title.
     * @return A configured JPanel.
     */
    private JPanel createCardPanel(String title) {
        JPanel pnl = new JPanel();
        pnl.setBorder(BorderFactory.createCompoundBorder(
                BorderFactory.createTitledBorder(
                        BorderFactory.createLineBorder(new Color(175, 190, 210), 1, true),
                        title,
                        0,
                        0,
                        getFont().deriveFont(Font.BOLD, 12f)
                ),
                BorderFactory.createEmptyBorder(3, 8, 4, 8)
        ));
        return pnl;
    }

    /**
     * Opens a URL in the user's default desktop browser or NetBeans internal browser.
     *
     * @param urlStr The URL string to open.
     */
    private void openUrlInBrowser(String urlStr) {
        try {
            if (Desktop.isDesktopSupported() && Desktop.getDesktop().isSupported(Desktop.Action.BROWSE)) {
                Desktop.getDesktop().browse(new URI(urlStr));
                return;
            }
        } catch (Exception ignored) {
        }
        try {
            HtmlBrowser.URLDisplayer.getDefault().showURL(new URL(urlStr));
        } catch (Exception ex) {
            JOptionPane.showMessageDialog(this, "Could not open URL: " + urlStr, "Browser Error", JOptionPane.ERROR_MESSAGE);
        }
    }

    /**
     * Refreshes all update center and module statuses asynchronously in a background worker.
     *
     * @param forceNetworkRefresh Whether to force a network refresh of the update catalogs.
     */
    public void refreshAllStateAsync(boolean forceNetworkRefresh) {
        setBusy(true, forceNetworkRefresh ? "Refreshing update catalogs from network..." : "Reading IDE module state...");

        new SwingWorker<Void, Void>() {
            private String major;
            private String productVersion;
            private final Map<UpdateCenterType, Boolean> registeredMap = new EnumMap<>(UpdateCenterType.class);
            private final Map<UpdateCenterType, Boolean> enabledMap = new EnumMap<>(UpdateCenterType.class);
            private final Map<UpdateCenterType, Boolean> trustedMap = new EnumMap<>(UpdateCenterType.class);
            private final Map<UpdateCenterType, String> connectivityMap = new EnumMap<>(UpdateCenterType.class);
            private final Map<UpdateCenterType, UpdateElement> providerUpdatesMap = new EnumMap<>(UpdateCenterType.class);

            private AnahataUcUtils.InstalledModuleDetails installedStudioDetails;
            private AnahataUcUtils.InstalledModuleDetails installedUcDetails;
            private String javaPlatformDesc;
            private String javaFxFormattedStatus;
            private boolean isJdkFx;
            private boolean fxActive;
            private String fxVer;
            private List<UpdateUnitProvider> legacyProviders;

            @Override
            protected Void doInBackground() throws Exception {
                major = AnahataUcUtils.getNetBeansMajorVersion();
                productVersion = System.getProperty("netbeans.productversion");
                if (productVersion == null) {
                    productVersion = "Apache NetBeans IDE " + (major != null ? major : "Unknown");
                }

                javaPlatformDesc = AnahataUcUtils.getJavaPlatformDescription();
                javaFxFormattedStatus = AnahataUcUtils.getJavaFxFormattedStatus();

                // Ensure all default update centers are registered
                AnahataUcUtils.registerDefaultUpdateCenters();

                if (forceNetworkRefresh) {
                    AnahataUcUtils.refreshAnahataProviders();
                }

                for (UpdateCenterType type : UpdateCenterType.values()) {
                    UpdateUnitProvider p = AnahataUcUtils.getUpdateUnitProvider(type);
                    registeredMap.put(type, p != null);
                    enabledMap.put(type, p != null && p.isEnabled());
                    trustedMap.put(type, p != null && p.isTrusted());

                    String url = AnahataUcUtils.getUpdateCenterUrl(type);
                    String connStatus = AnahataUcUtils.checkUrlConnectivity(url);
                    connectivityMap.put(type, connStatus);

                    UpdateElement updateElem = AnahataUcUtils.getAvailableStudioElementForProvider(type);
                    if (updateElem != null) {
                        providerUpdatesMap.put(type, updateElem);
                    }
                }

                installedStudioDetails = AnahataUcUtils.getInstalledStudioDetails();
                installedUcDetails = AnahataUcUtils.getInstalledUcDetails();
                isJdkFx = AnahataUcUtils.isSystemJdkJavaFx();
                fxActive = AnahataUcUtils.isJavaFxActive();
                fxVer = AnahataUcUtils.getJavaFxVersion();

                legacyProviders = AnahataUcUtils.getLegacyAnahataProviders();
                return null;
            }

            @Override
            protected void done() {
                try {
                    get();

                    // 1. Update Left Card: Anahata ASI Studio (Left-aligned 3 lines)
                    if (installedStudioDetails != null) {
                        StringBuilder sb = new StringBuilder("<html><div style='text-align: left; line-height: 1.35;'>");
                        sb.append("<b>Status:</b> <font color='#16a34a'><b>v")
                                .append(installedStudioDetails.specVersion() != null ? installedStudioDetails.specVersion() : "unknown")
                                .append("</b></font><br/>");
                        sb.append("<font color='#666666'>Implementation Version: </font>")
                                .append(installedStudioDetails.implVersion() != null ? installedStudioDetails.implVersion() : "N/A")
                                .append("<br/>");
                        sb.append("<font color='#666666'>Build: </font>")
                                .append(installedStudioDetails.buildVersion() != null ? installedStudioDetails.buildVersion() : "N/A");
                        sb.append("</div></html>");
                        lblStudioInstalled.setText(sb.toString());
                    } else {
                        lblStudioInstalled.setText("<html><div style='text-align: left; line-height: 1.35;'>"
                                + "<b>Status:</b> <font color='#dc2626'><b>Not Installed</b></font><br/>"
                                + "<font color='#666666'>Implementation Version: </font>N/A<br/>"
                                + "<font color='#666666'>Build: </font>N/A</div></html>");
                    }

                    // 2. Update Right Card: NetBeans (Left-aligned 4 lines)
                    StringBuilder nbSb = new StringBuilder("<html><div style='text-align: left; line-height: 1.35;'>");
                    nbSb.append("<b>Product:</b> ").append(productVersion).append("<br/>");
                    nbSb.append("<b>Java:</b> ").append(javaPlatformDesc).append("<br/>");
                    nbSb.append("<b>Detected Major:</b> <code>").append(major != null ? major : "unknown").append("</code><br/>");
                    nbSb.append("<b>JavaFX:</b> ");
                    if (javaFxFormattedStatus.contains("Not available")) {
                        nbSb.append("<font color='#dc2626'>").append(javaFxFormattedStatus).append("</font>");
                    } else {
                        nbSb.append("<font color='#16a34a'><b>").append(javaFxFormattedStatus).append("</b></font>");
                    }
                    nbSb.append("</div></html>");
                    lblEnvironment.setText(nbSb.toString());

                    // Update all 3 Update Centers
                    for (UpdateCenterType type : UpdateCenterType.values()) {
                        boolean reg = registeredMap.getOrDefault(type, false);
                        boolean enabled = enabledMap.getOrDefault(type, false);
                        boolean trusted = trustedMap.getOrDefault(type, false);
                        String conn = connectivityMap.getOrDefault(type, "Offline");
                        UpdateElement updateElem = providerUpdatesMap.get(type);

                        JToggleButton btn = toggleButtons.get(type);
                        JCheckBox chk = trustCheckBoxes.get(type);
                        JLabel dot = statusDotLabels.get(type);
                        JLabel text = statusTextLabels.get(type);
                        JButton btnUpdate = updateButtons.get(type);
                        JLabel lblPkg = packageDetailsLabels.get(type);
                        JLabel lblNotif = notificationLabels.get(type);

                        if (btn != null) {
                            if (!reg) {
                                btn.setIcon(AnahataUcIcons.createDisabledIcon());
                                btn.setText("Install");
                                btn.setSelected(false);
                            } else if (enabled) {
                                btn.setIcon(AnahataUcIcons.createEnabledIcon());
                                btn.setText("Enabled");
                                btn.setSelected(true);
                            } else {
                                btn.setIcon(AnahataUcIcons.createDisabledIcon());
                                btn.setText("Disabled");
                                btn.setSelected(false);
                            }
                        }

                        if (chk != null) {
                            chk.setEnabled(reg);
                            chk.setSelected(trusted);
                        }

                        if (text != null) {
                            if ("Online".equalsIgnoreCase(conn)) {
                                text.setText("<html><div style='text-align: center;'><font color='#16a34a'>● <b>Online</b></font></div></html>");
                            } else {
                                String rawErr = conn.startsWith("Offline") ? conn.substring(7).trim() : conn;
                                rawErr = rawErr.replaceAll("^[\\(\\[]+|[\\)\\]]+$", "").trim();
                                text.setText("<html><div style='text-align: center;'><font color='#dc2626'>● <b>Offline</b></font><br/><font color='#888888'>" + rawErr + "</font></div></html>");
                            }
                        }

                        if (btnUpdate != null) {
                            if (updateElem != null) {
                                String channelTag = type == UpdateCenterType.DEV ? " (Dev Snapshot)" : (type == UpdateCenterType.STABLE ? " (Stable GA)" : "");
                                String actionText = (installedStudioDetails == null)
                                        ? "Install v" + updateElem.getSpecificationVersion() + channelTag
                                        : "Update to v" + updateElem.getSpecificationVersion() + channelTag;
                                btnUpdate.setText(actionText);
                                btnUpdate.putClientProperty("targetElement", updateElem);
                                btnUpdate.setVisible(true);
                                btnUpdate.setEnabled(true);

                                // Format package download size and release date on a single line directly under button
                                String sizeStr = AnahataUcUtils.formatDownloadSize(updateElem.getDownloadSize());
                                String dateStr = updateElem.getDate();
                                StringBuilder pkgInfo = new StringBuilder("<html><div style='text-align: left; font-size: 10px;'>");
                                if (sizeStr != null) {
                                    pkgInfo.append("<font color='#666666'>Size:</font> <b>").append(sizeStr).append("</b>");
                                }
                                if (dateStr != null) {
                                    if (sizeStr != null) {
                                        pkgInfo.append("&nbsp;&nbsp;|&nbsp;&nbsp;");
                                    }
                                    pkgInfo.append("<font color='#666666'>Released:</font> <b>").append(dateStr).append("</b>");
                                }
                                pkgInfo.append("</div></html>");

                                if (lblPkg != null) {
                                    lblPkg.setText(pkgInfo.toString());
                                    lblPkg.setVisible(true);
                                }

                                // Notification / Release Banner
                                String notif = updateElem.getNotification();
                                if (lblNotif != null) {
                                    if (notif != null && !notif.trim().isEmpty()) {
                                        lblNotif.setText("<html><font color='#b45309'><b>🔔 Announcement:</b> " + notif + "</font></html>");
                                        lblNotif.setVisible(true);
                                    } else {
                                        lblNotif.setVisible(false);
                                    }
                                }
                            } else {
                                btnUpdate.setVisible(false);
                                if (lblPkg != null) {
                                    lblPkg.setVisible(false);
                                }
                                if (lblNotif != null) {
                                    lblNotif.setVisible(false);
                                }
                            }
                        }
                    }

                    // Update JavaFX Status
                    if (isJdkFx) {
                        lblJavaFxStatus.setText("<html>JavaFX Runtime: <font color='#16a34a'><b>Active (System JDK)</b></font></html>");
                        btnJavaFxAction.setText("JavaFX Active (JDK)");
                        btnJavaFxAction.setEnabled(false);
                    } else if (fxActive) {
                        lblJavaFxStatus.setText("<html>JavaFX Runtime: <font color='#16a34a'><b>Active"
                                + (fxVer != null ? " (" + fxVer + ")" : "") + "</b></font></html>");
                        btnJavaFxAction.setText("JavaFX Active (Module)");
                        btnJavaFxAction.setEnabled(false);
                    } else {
                        lblJavaFxStatus.setText("<html>JavaFX Runtime: <font color='#dc2626'><b>Not Enabled</b></font></html>");
                        btnJavaFxAction.setText("Install / Activate JavaFX");
                        btnJavaFxAction.setEnabled(true);
                    }

                    // Update Legacy Providers Panel
                    renderLegacyProviders(legacyProviders);

                    setBusy(false, "Update center catalogs verified.");
                } catch (Exception ex) {
                    setBusy(false, "Error refreshing status: " + ex.getMessage());
                }
            }
        }.execute();
    }

    /**
     * Renders warning banners for any obsolete update centers from previous NetBeans versions.
     *
     * @param legacy List of legacy providers.
     */
    private void renderLegacyProviders(List<UpdateUnitProvider> legacy) {
        pnlLegacyContainer.removeAll();
        if (legacy != null && !legacy.isEmpty()) {
            JPanel card = createCardPanel("Legacy Update Centers Detected");
            card.setLayout(new BoxLayout(card, BoxLayout.Y_AXIS));

            JLabel lblWarning = new JLabel("The following update centers belong to earlier NetBeans versions and can be cleaned up:");
            lblWarning.setForeground(new Color(180, 50, 50));
            card.add(lblWarning);
            card.add(Box.createVerticalStrut(6));

            for (UpdateUnitProvider p : legacy) {
                JPanel row = new JPanel(new FlowLayout(FlowLayout.LEFT, 8, 2));
                row.setOpaque(false);
                JLabel lblName = new JLabel(p.getDisplayName() + " (" + p.getProviderURL() + ")");
                JButton btnRemove = new JButton("Remove");
                btnRemove.addActionListener(e -> {
                    AnahataUcUtils.removeProvider(p);
                    refreshAllStateAsync(false);
                });
                row.add(btnRemove);
                row.add(lblName);
                card.add(row);
            }
            pnlLegacyContainer.add(card);
            pnlLegacyContainer.add(Box.createVerticalStrut(10));
        }
        pnlLegacyContainer.revalidate();
        pnlLegacyContainer.repaint();
    }

    /**
     * Handles toggle action for an update center.
     *
     * @param type The {@link UpdateCenterType}.
     */
    private void onToggleCenter(UpdateCenterType type) {
        boolean currentlyReg = AnahataUcUtils.isUpdateCenterRegistered(type);
        if (!currentlyReg) {
            AnahataUcUtils.registerDefaultUpdateCenters();
            AnahataUcUtils.setUpdateCenterEnabled(type, true);
        } else {
            boolean enabled = AnahataUcUtils.isUpdateCenterEnabled(type);
            AnahataUcUtils.setUpdateCenterEnabled(type, !enabled);
        }
        refreshAllStateAsync(false);
    }

    /**
     * Handles toggling the Trusted checkbox for an update center.
     *
     * @param type The {@link UpdateCenterType}.
     */
    private void onToggleTrust(UpdateCenterType type) {
        JCheckBox chk = trustCheckBoxes.get(type);
        if (chk != null) {
            AnahataUcUtils.setUpdateCenterTrusted(type, chk.isSelected());
        }
    }

    /**
     * Handles 1-click install or update of Anahata ASI Studio from a specific update center.
     *
     * @param type The source {@link UpdateCenterType}.
     */
    private void onInstallOrUpdateFromProvider(UpdateCenterType type) {
        JButton btn = updateButtons.get(type);
        if (btn == null) {
            return;
        }
        UpdateElement element = (UpdateElement) btn.getClientProperty("targetElement");
        if (element == null) {
            return;
        }

        setBusy(true, "Installing Anahata ASI Studio v" + element.getSpecificationVersion() + " from " + AnahataUcUtils.getUpdateCenterDisplayName(type) + "...");

        new SwingWorker<String, Void>() {
            @Override
            protected String doInBackground() throws Exception {
                return AnahataUcUtils.installOrUpdateStudio(element);
            }

            @Override
            protected void done() {
                try {
                    String result = get();
                    setBusy(false, result);
                    JOptionPane.showMessageDialog(AnahataUpdateCenterPanel.this, result, "Installation Complete", JOptionPane.INFORMATION_MESSAGE);
                    refreshAllStateAsync(false);
                } catch (Exception ex) {
                    setBusy(false, "Installation failed: " + ex.getMessage());
                    JOptionPane.showMessageDialog(AnahataUpdateCenterPanel.this, "Installation failed: " + ex.getMessage(), "Error", JOptionPane.ERROR_MESSAGE);
                }
            }
        }.execute();
    }

    /**
     * Handles the 1-click JavaFX install/activate action.
     */
    private void onJavaFxAction() {
        setBusy(true, "Activating NetBeans JavaFX runtime support...");

        new SwingWorker<String, Void>() {
            @Override
            protected String doInBackground() throws Exception {
                return AnahataUcUtils.installOrActivateJavaFx();
            }

            @Override
            protected void done() {
                try {
                    String result = get();
                    setBusy(false, result);
                    JOptionPane.showMessageDialog(AnahataUpdateCenterPanel.this, result, "JavaFX Support", JOptionPane.INFORMATION_MESSAGE);
                    refreshAllStateAsync(false);
                } catch (Exception ex) {
                    setBusy(false, "JavaFX activation failed: " + ex.getMessage());
                    JOptionPane.showMessageDialog(AnahataUpdateCenterPanel.this, "Activation failed: " + ex.getMessage(), "Error", JOptionPane.ERROR_MESSAGE);
                }
            }
        }.execute();
    }

    /**
     * Sets the UI busy state, toggling the progress bar and disabling interactive controls.
     *
     * @param busy {@code true} if an async task is running.
     * @param status The status text to display.
     */
    private void setBusy(boolean busy, String status) {
        lblGlobalStatus.setText(status);
        progressBar.setVisible(busy);
        progressBar.setIndeterminate(busy);
        btnCheckUpdates.setEnabled(!busy);
        setCursor(busy ? Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR) : Cursor.getDefaultCursor());
    }
}
