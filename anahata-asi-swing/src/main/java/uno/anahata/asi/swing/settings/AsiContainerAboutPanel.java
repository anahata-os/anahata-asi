/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.settings;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Desktop;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.io.IOException;
import java.nio.file.Path;
import java.util.List;
import javax.swing.BorderFactory;
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.UIManager;
import lombok.NonNull;
import lombok.extern.slf4j.Slf4j;
import net.miginfocom.swing.MigLayout;
import uno.anahata.asi.AbstractAsiContainer;
import uno.anahata.asi.internal.TextUtils;
import uno.anahata.asi.swing.AbstractSwingAsiContainer;
import uno.anahata.asi.swing.components.ScrollablePanel;
import uno.anahata.asi.swing.icons.DeleteIcon;
import uno.anahata.asi.swing.icons.ExternalIcon;
import uno.anahata.asi.swing.icons.IconUtils;
import uno.anahata.asi.swing.internal.EdtPropertyChangeListener;

/**
 * A comprehensive telemetry and diagnostics panel for the active ASI container.
 * <p>
 * Displays foundational runtime metadata including host application identifier,
 * container implementation version, Core framework version, verified storage
 * paths on disk, active session and provider counts, JVM memory telemetry, and
 * operational boot notifications (e.g. quarantined entities or initialization alerts).
 * </p>
 *
 * @author anahata
 */
@Slf4j
public class AsiContainerAboutPanel extends ScrollablePanel {

    /**
     * The parent ASI container instance providing diagnostic data.
     */
    private final AbstractSwingAsiContainer container;

    /**
     * Text area for displaying operational notifications and boot diagnostic logs.
     */
    private final JTextArea notificationsArea;

    /**
     * Progress bar displaying real-time JVM heap memory utilization.
     */
    private final JProgressBar memoryBar;

    /**
     * Label showing exact JVM heap memory metrics in megabytes.
     */
    private final JLabel memoryLabel;

    /**
     * Constructs a new AsiContainerAboutPanel bound to the specified container.
     *
     * @param container The active ASI container.
     */
    public AsiContainerAboutPanel(@NonNull AbstractSwingAsiContainer container) {
        this.container = container;
        setLayout(new BorderLayout());
        setOpaque(false);

        JPanel content = new JPanel(new MigLayout("fillx, insets 20", "[grow,fill]", "[]15[]15[]15[]"));
        content.setOpaque(false);

        // 1. Header with Logo & Title
        JPanel headerPanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 15, 0));
        headerPanel.setOpaque(false);
        Icon logoIcon = IconUtils.getIcon("v2/anahata.png", 48, 48);
        if (logoIcon != null) {
            headerPanel.add(new JLabel(logoIcon));
        }
        JPanel titleBox = new JPanel(new MigLayout("ins 0, gap 2", "[]", "[]0[]"));
        titleBox.setOpaque(false);
        JLabel titleLabel = new JLabel("Anahata ASI");
        titleLabel.setFont(titleLabel.getFont().deriveFont(Font.BOLD, 18f));
        titleBox.add(titleLabel, "wrap");
        JLabel subtitleLabel = new JLabel("Pure-Java Model-Agnostic Super Intelligence");
        subtitleLabel.setForeground(UIManager.getColor("Label.disabledForeground"));
        subtitleLabel.setFont(subtitleLabel.getFont().deriveFont(Font.ITALIC, 12f));
        titleBox.add(subtitleLabel);
        headerPanel.add(titleBox);
        content.add(headerPanel, "wrap");

        // 2. Container Identity & Versions
        JPanel identitySection = createTitledSection("Container Identity & Specifications");
        identitySection.setLayout(new MigLayout("fillx, insets 12", "[right]15[grow,fill]5[]", "[]6[]6[]6[]6[]"));

        addMetadataRow(identitySection, "Host Application:", container.getHostApplicationId());
        addMetadataRow(identitySection, "Container Class:", container.getClass().getName());
        addMetadataRow(identitySection, "Container Version:", container.getContainerImplementationVersion() != null ? container.getContainerImplementationVersion() : "Development Snapshot");
        addMetadataRow(identitySection, "Core Framework Version:", AbstractAsiContainer.getAsiCoreImplementationVersion() != null ? AbstractAsiContainer.getAsiCoreImplementationVersion() : "Development Snapshot");
        addMetadataRow(identitySection, "Active Sessions:", String.valueOf(container.getActiveAgis().size()));
        addMetadataRow(identitySection, "Configured Providers:", String.valueOf(container.getAllProviders().size()));
        content.add(identitySection, "wrap");

        // 3. Storage Hierarchy on Disk
        JPanel storageSection = createTitledSection("Storage Directories on Disk");
        storageSection.setLayout(new MigLayout("fillx, insets 12", "[right]15[grow,fill]5[]", "[]6[]6[]6[]6[]"));

        addPathRow(storageSection, "Root Working Directory:", AbstractAsiContainer.getWorkDir());
        try {
            addPathRow(storageSection, "App Version Directory:", container.getDirectory());
            addPathRow(storageSection, "Providers Directory:", container.getProvidersDir());
            addPathRow(storageSection, "Templates Directory:", container.getTemplatesDir());
            addPathRow(storageSection, "Sessions Directory:", container.getSessionsDir());
        } catch (IOException e) {
            log.error("Failed to resolve container directories for About panel", e);
        }
        content.add(storageSection, "wrap");

        // 4. Runtime & JVM Telemetry
        JPanel runtimeSection = createTitledSection("JVM Environment & Heap Utilization");
        runtimeSection.setLayout(new MigLayout("fillx, insets 12", "[right]15[grow,fill]", "[]6[]6[]"));

        addMetadataRow(runtimeSection, "Java Runtime:", System.getProperty("java.runtime.name") + " (" + System.getProperty("java.version") + ")");
        addMetadataRow(runtimeSection, "Available Processors:", Runtime.getRuntime().availableProcessors() + " Cores");

        memoryBar = new JProgressBar(0, 100);
        memoryBar.setStringPainted(true);
        memoryLabel = new JLabel();
        updateMemoryMetrics();

        JPanel memoryPanel = new JPanel(new BorderLayout(8, 0));
        memoryPanel.setOpaque(false);
        memoryPanel.add(memoryBar, BorderLayout.CENTER);
        memoryPanel.add(memoryLabel, BorderLayout.EAST);

        runtimeSection.add(new JLabel("Heap Memory Usage:"));
        runtimeSection.add(memoryPanel, "wrap");
        content.add(runtimeSection, "wrap");

        // 5. Operational Notifications & Diagnostic Log
        JPanel notifSection = createTitledSection("Operational Notifications & Boot Diagnostics");
        notifSection.setLayout(new BorderLayout(0, 8));

        notificationsArea = new JTextArea(5, 40);
        notificationsArea.setFont(new Font(Font.MONOSPACED, Font.PLAIN, 12));
        notificationsArea.setEditable(false);
        notificationsArea.setLineWrap(true);
        notificationsArea.setWrapStyleWord(true);
        updateNotifications();

        JScrollPane notifScroll = new JScrollPane(notificationsArea);
        notifScroll.setBorder(BorderFactory.createLineBorder(new Color(180, 180, 180)));
        notifSection.add(notifScroll, BorderLayout.CENTER);

        JPanel notifActions = new JPanel(new FlowLayout(FlowLayout.RIGHT, 0, 0));
        notifActions.setOpaque(false);
        JButton clearBtn = new JButton("Clear Notifications", new DeleteIcon(14));
        clearBtn.setToolTipText("Clear all recorded diagnostic notifications");
        clearBtn.addActionListener(e -> container.clearNotifications());
        notifActions.add(clearBtn);
        notifSection.add(notifActions, BorderLayout.SOUTH);

        content.add(notifSection, "wrap");

        // Bind reactive listener for notifications
        new EdtPropertyChangeListener(this, container, "notifications", evt -> updateNotifications());

        JScrollPane mainScroll = new JScrollPane(content);
        mainScroll.setBorder(null);
        mainScroll.getVerticalScrollBar().setUnitIncrement(24);
        add(mainScroll, BorderLayout.CENTER);
    }

    /**
     * Creates a titled section panel with consistent Barça design styling.
     *
     * @param title The section title.
     * @return The styled container panel.
     */
    private JPanel createTitledSection(String title) {
        JPanel panel = new JPanel();
        panel.setOpaque(false);
        panel.setBorder(BorderFactory.createTitledBorder(
                BorderFactory.createLineBorder(new Color(200, 200, 200)),
                title, 0, 0, getFont().deriveFont(Font.BOLD, 12f), new Color(80, 80, 80)));
        return panel;
    }

    /**
     * Adds a key-value text row to the target layout panel.
     *
     * @param target The target panel.
     * @param label The metadata label.
     * @param value The metadata value.
     */
    private void addMetadataRow(JPanel target, String label, String value) {
        target.add(new JLabel(label));
        JTextField field = new JTextField(value);
        field.setEditable(false);
        field.setBorder(null);
        field.setOpaque(false);
        target.add(field, "span 2, wrap");
    }

    /**
     * Adds a filesystem path row with an action button to open it in desktop file manager.
     *
     * @param target The target layout panel.
     * @param label The directory label.
     * @param path The resolved Path.
     */
    private void addPathRow(JPanel target, String label, Path path) {
        target.add(new JLabel(label));
        JTextField field = new JTextField(path != null ? path.toString() : "N/A");
        field.setEditable(false);
        field.setBorder(null);
        field.setOpaque(false);
        target.add(field);

        JButton openBtn = new JButton(new ExternalIcon(14));
        openBtn.setToolTipText("Open folder in Desktop File Manager");
        openBtn.addActionListener(e -> {
            if (path != null) {
                try {
                    Desktop.getDesktop().open(path.toFile());
                } catch (Exception ex) {
                    log.error("Failed to open directory {}", path, ex);
                }
            }
        });
        target.add(openBtn, "wrap");
    }

    /**
     * Refreshes the memory progress bar and label based on active JVM metrics.
     */
    public void updateMemoryMetrics() {
        long totalMem = Runtime.getRuntime().totalMemory();
        long freeMem = Runtime.getRuntime().freeMemory();
        long usedMem = totalMem - freeMem;
        long maxMem = Runtime.getRuntime().maxMemory();

        int percent = maxMem > 0 ? (int) ((usedMem * 100) / maxMem) : 0;
        memoryBar.setValue(percent);
        memoryBar.setString(percent + "% (" + TextUtils.formatSize(usedMem) + " / " + TextUtils.formatSize(maxMem) + ")");
        memoryLabel.setText(TextUtils.formatSize(usedMem) + " of " + TextUtils.formatSize(maxMem));
    }

    /**
     * Updates the diagnostic notifications text area from the container's record.
     */
    private void updateNotifications() {
        List<String> list = container.getNotifications();
        if (list.isEmpty()) {
            notificationsArea.setText("No operational issues or quarantine warnings recorded. Container operating normally.");
            notificationsArea.setForeground(UIManager.getColor("Label.disabledForeground"));
        } else {
            StringBuilder sb = new StringBuilder();
            for (int i = 0; i < list.size(); i++) {
                sb.append("[").append(i + 1).append("] ").append(list.get(i)).append("\n");
            }
            notificationsArea.setText(sb.toString().trim());
            notificationsArea.setForeground(new Color(180, 0, 0));
        }
    }
}
