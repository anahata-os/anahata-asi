/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.settings;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import javax.swing.JFrame;
import lombok.Getter;
import lombok.NonNull;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.swing.AbstractSwingAsiContainer;
import uno.anahata.asi.swing.icons.IconUtils;

/**
 * A dedicated, single-instance JFrame for the ASI Container Settings Command Center.
 * <p>
 * Opens in full maximized mode ({@link JFrame#MAXIMIZED_BOTH}) and hosts the
 * {@link AsiContainerSettingsPanel}.
 * </p>
 *
 * @author anahata
 */
@Slf4j
@Getter
public class AsiContainerSettingsFrame extends JFrame {

    /**
     * The embedded settings panel instance.
     */
    private final AsiContainerSettingsPanel settingsPanel;

    /**
     * Constructs a new settings frame for the specified container.
     *
     * @param container The active ASI container.
     * @param initialTabIndex The index of the tab to open.
     */
    public AsiContainerSettingsFrame(@NonNull AbstractSwingAsiContainer container, int initialTabIndex) {
        super("ASI Container Settings - " + container.getHostApplicationId());
        setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);

        try {
            setIconImages(IconUtils.getLogoImages());
        } catch (Exception e) {
            log.warn("Failed to set frame icons", e);
        }

        this.settingsPanel = new AsiContainerSettingsPanel(container, initialTabIndex);
        setLayout(new BorderLayout());
        add(settingsPanel, BorderLayout.CENTER);

        settingsPanel.setCloseCallback(() -> {
            dispose();
            container.setSettingsFrame(null);
        });

        addWindowListener(new WindowAdapter() {
            @Override
            public void windowClosed(WindowEvent e) {
                container.setSettingsFrame(null);
            }
        });

        container.setSettingsFrame(this);

        setMinimumSize(new Dimension(800, 600));
        setPreferredSize(new Dimension(1400, 900));
        pack();
        setExtendedState(JFrame.MAXIMIZED_BOTH);
    }
}
