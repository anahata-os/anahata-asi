/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.settings;

import java.awt.BorderLayout;
import java.awt.FlowLayout;
import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;
import lombok.Getter;
import lombok.NonNull;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.swing.AbstractSwingAsiContainer;
import uno.anahata.asi.swing.components.ScrollablePanel;
import uno.anahata.asi.swing.icons.CancelIcon;

/**
 * The unified Command Center panel for managing the ASI container.
 * <p>
 * Houses tabs for AI Providers, Templates, and About telemetry.
 * </p>
 *
 * @author anahata
 */
@Slf4j
@Getter
public class AsiContainerSettingsPanel extends ScrollablePanel {

    /**
     * The parent ASI container instance.
     */
    private final AbstractSwingAsiContainer container;

    /**
     * The master tabbed pane.
     */
    private final JTabbedPane mainTabs;

    /**
     * The standalone AI Providers management panel.
     */
    private final AiProvidersPanel providersPanel;

    /**
     * The telemetry and diagnostics About panel.
     */
    private final AsiContainerAboutPanel aboutPanel;

    /**
     * Callback invoked when the user closes the settings dialog/frame.
     */
    @Setter
    private Runnable closeCallback;

    /**
     * Constructs a new settings panel, defaulting to tab 0.
     *
     * @param container The active ASI container.
     */
    public AsiContainerSettingsPanel(@NonNull AbstractSwingAsiContainer container) {
        this(container, 0);
    }

    /**
     * Constructs a new settings panel with a specific initial tab selected.
     *
     * @param container The active ASI container.
     * @param initialTabIndex The index of the tab to open.
     */
    public AsiContainerSettingsPanel(@NonNull AbstractSwingAsiContainer container, int initialTabIndex) {
        this.container = container;
        setLayout(new BorderLayout());

        this.mainTabs = new JTabbedPane();
        this.providersPanel = new AiProvidersPanel(container);
        this.aboutPanel = new AsiContainerAboutPanel(container);

        mainTabs.addTab("AI Providers", providersPanel);
        mainTabs.addTab("About", aboutPanel);

        if (initialTabIndex >= 0 && initialTabIndex < mainTabs.getTabCount()) {
            mainTabs.setSelectedIndex(initialTabIndex);
        }

        add(mainTabs, BorderLayout.CENTER);
        add(createBottomButtonPanel(), BorderLayout.SOUTH);
    }

    /**
     * Creates the bottom command bar with the Close button.
     *
     * @return The bottom panel.
     */
    private JPanel createBottomButtonPanel() {
        JPanel panel = new JPanel(new FlowLayout(FlowLayout.RIGHT, 10, 10));
        panel.setOpaque(false);

        JButton closeBtn = new JButton("Close", new CancelIcon(16));
        closeBtn.addActionListener(e -> {
            if (closeCallback != null) {
                closeCallback.run();
            }
        });
        panel.add(closeBtn);
        return panel;
    }


    /**
     * Selects a specific tab by index.
     *
     * @param index The tab index.
     */
    public void selectTab(int index) {
        if (index >= 0 && index < mainTabs.getTabCount()) {
            mainTabs.setSelectedIndex(index);
        }
    }
}
