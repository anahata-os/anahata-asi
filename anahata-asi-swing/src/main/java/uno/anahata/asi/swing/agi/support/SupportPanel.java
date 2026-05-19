/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.agi.support;

import javax.swing.*;
import java.awt.*;
import java.net.URI;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.swing.components.ScrollablePanel;
import uno.anahata.asi.swing.components.WrapLayout;
import uno.anahata.asi.swing.games.Arkanoid;
import uno.anahata.asi.swing.games.Snake;
import uno.anahata.asi.swing.games.Tetris;
import uno.anahata.asi.swing.icons.ArkanoidIcon;
import uno.anahata.asi.swing.icons.IconUtils;
import uno.anahata.asi.swing.icons.SnakeIcon;
import uno.anahata.asi.swing.icons.TetrisIcon;
import uno.anahata.asi.swing.games.BugDefense;
import uno.anahata.asi.swing.icons.BugDefenseIcon;

/**
 * A high-salience UI component providing support links and community resources.
 * <p>
 * This panel serves as the primary gateway for users to interact with the
 * Anahata ecosystem, providing direct access to Discord, GitHub, and official
 * documentation. It follows the ported V2 architecture for consistent modal
 * support and rendering.
 * </p>
 *
 * @author anahata
 */
@Slf4j
public class SupportPanel extends ScrollablePanel {

    /**
     * Constructs a new SupportPanel.
     */
    public SupportPanel() {
        setScrollableTracksViewportWidth(true);
        initComponents();
    }

    /**
     * {@inheritDoc}
     * <p>
     * Adds the "Play Arkanoid" button to the links grid, providing a
     * recreational break for the developer.</p>
     */
    private void initComponents() {
        setLayout(new GridBagLayout());
        setOpaque(false);

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.anchor = GridBagConstraints.NORTHWEST;
        gbc.insets = new Insets(20, 20, 10, 20);

        // Title
        JLabel titleLabel = new JLabel("Support & Community");
        titleLabel.setFont(titleLabel.getFont().deriveFont(Font.BOLD, 20f));
        add(titleLabel, gbc);

        gbc.gridy++;
        gbc.insets = new Insets(0, 20, 20, 20);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.weightx = 1.0;
        add(new JSeparator(), gbc);

        // Links Grid - Using our WrapLayout for superior responsiveness
        JPanel linksGrid = new JPanel(new WrapLayout(FlowLayout.LEFT, 15, 15));
        linksGrid.setOpaque(false);

        List<JPanel> cards = new ArrayList<>();

        // Support Links
        cards.add(createCard("Join our Discord", () -> openWebpage("https://discord.gg/gwGWWxPUXE"),
                "Connect with the community and get real-time help.", "discord.png"));

        cards.add(createCard("Report an Issue", () -> openWebpage("https://github.com/anahata-os/anahata-asi/issues"),
                "Found a bug? Let us know on GitHub.", "github.png"));

        cards.add(createCard("Email Support", () -> openWebpage("mailto:support@anahata.uno"),
                "Send us a direct message at support@anahata.uno", "email.png"));

        cards.add(createCard("Official Website", () -> openWebpage("https://asi.anahata.uno/"),
                "Learn more about the Anahata ecosystem.", "v2/anahata.png"));

        cards.add(createCard("AnahataTV (YouTube)", () -> openWebpage("https://www.youtube.com/@anahata108"),
                "Watch tutorials and feature showcases.", "youtube.png"));

        cards.add(createCard("Browse Javadocs", () -> openWebpage("https://asi.anahata.uno/apidocs/"),
                "Technical documentation and API reference.", "javadoc.png"));

        cards.add(createCard("Give to Anahata", () -> openWebpage("https://www.paypal.com/donate/?hosted_button_id=SS8B8R7S68R7G"),
                "Support the development of the first ASI.", "v2/anahata.png"));

        // Games Row
        cards.add(createCard("Play Arkanoid", () -> Arkanoid.main(null),
                "Take a break with the classic brick breaker.", new ArkanoidIcon(16)));

        cards.add(createCard("Mapacho Snake", () -> Snake.main(null),
                "Hunt for cigars in the digital jungle. Força Barça!", new SnakeIcon(16)));

        cards.add(createCard("Atoms Tetris", () -> Tetris.main(null),
                "Find the perfect place for every atom.", new TetrisIcon(16)));

        cards.add(createCard("Bug Defense", () -> BugDefense.main(null),
                "Defend the digital continent from logic bugs.", new BugDefenseIcon(16)));

        // Shuffle cards for a dynamic experience
        Collections.shuffle(cards);
        cards.forEach(linksGrid::add);

        gbc.gridy++;
        gbc.insets = new Insets(0, 5, 20, 20); // Reduced left inset to account for WrapLayout's internal flow
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.anchor = GridBagConstraints.NORTHWEST;
        gbc.weighty = 0.0;
        add(linksGrid, gbc);

        gbc.gridy++;
        gbc.weighty = 1.0;
        add(Box.createVerticalGlue(), gbc);

    }

    /**
     * Creates a standardized card component using a direct Icon instance.
     *
     * @param title The title of the card.
     * @param action The logic to execute when the button is clicked.
     * @param description A brief description of the resource.
     * @param icon The Icon instance to display.
     * @return A panel containing the UI card.
     */
    private JPanel createCard(String title, Runnable action, String description, Icon icon) {
        JPanel card = new JPanel(new BorderLayout(5, 2));
        card.setOpaque(false);
        card.setPreferredSize(new Dimension(200, 80));

        JButton btn = new JButton(title, icon);
        btn.setPreferredSize(new Dimension(180, 35));
        btn.setHorizontalAlignment(SwingConstants.LEFT);
        btn.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
        btn.addActionListener(e -> action.run());

        JTextArea descArea = new JTextArea(description);
        descArea.setWrapStyleWord(true);
        descArea.setLineWrap(true);
        descArea.setEditable(false);
        descArea.setFocusable(false);
        descArea.setOpaque(false);
        descArea.setForeground(Color.GRAY);
        descArea.setFont(descArea.getFont().deriveFont(11f));
        descArea.setBorder(BorderFactory.createEmptyBorder(0, 5, 0, 5));

        card.add(btn, BorderLayout.NORTH);
        card.add(descArea, BorderLayout.CENTER);

        return card;
    }

    /**
     * Overload to support loading icons by resource name from the classpath.
     *
     * @param title The title of the card.
     * @param action The logic to execute.
     * @param description Description text.
     * @param iconName Resource name of the icon.
     * @return The card panel.
     */
    private JPanel createCard(String title, Runnable action, String description, String iconName) {
        return createCard(title, action, description, IconUtils.getIcon(iconName, 16, 16));
    }

    /**
     * Dispatches a URL request to the default system browser.
     * <p>
     * Utilizes {@link Desktop#browse(URI)} to open web-based resources.
     * Includes error handling and user feedback via {@link JOptionPane} in case
     * of hardware or OS-level dispatch failures.
     * </p>
     *
     * @param url The target URL to open.
     */
    private void openWebpage(String url) {
        try {
            Desktop.getDesktop().browse(new URI(url));
        } catch (Exception e) {
            log.error("Failed to open URL: " + url, e);
            JOptionPane.showMessageDialog(this, "Could not open link: " + e.getMessage(), "Error", JOptionPane.ERROR_MESSAGE);
        }
    }
}
