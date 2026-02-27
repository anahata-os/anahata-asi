/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.agi;

import javax.swing.*;
import java.awt.*;
import java.net.URI;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.swing.components.ScrollablePanel;
import uno.anahata.asi.swing.icons.IconUtils;

/**
 * A panel providing support links and community resources for users.
 * Ported from V1 to the V2 architecture.
 * 
 * @author anahata
 */
@Slf4j
public class SupportPanel extends ScrollablePanel {

    /**
     * Constructs a new SupportPanel.
     */
    public SupportPanel() {
        initComponents();
    }

    /**
     * Initializes the components and layout of the panel.
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

        // Links Grid - Two columns
        JPanel linksGrid = new JPanel(new GridBagLayout());
        linksGrid.setOpaque(false);
        GridBagConstraints gridGbc = new GridBagConstraints();
        gridGbc.insets = new Insets(0, 0, 15, 15);
        gridGbc.anchor = GridBagConstraints.NORTHWEST;
        gridGbc.fill = GridBagConstraints.NONE;
        gridGbc.weightx = 0.0;

        gridGbc.gridx = 0; gridGbc.gridy = 0;
        linksGrid.add(createLinkCard("Join our Discord", "https://discord.com/invite/M396BNtX", 
                "Connect with the community and get real-time help.", "discord.png"), gridGbc);

        gridGbc.gridx = 1;
        linksGrid.add(createLinkCard("Report an Issue", "https://github.com/anahata-os/anahata-asi/issues", 
                "Found a bug? Let us know on GitHub.", "github.png"), gridGbc);

        gridGbc.gridx = 0; gridGbc.gridy = 1;
        linksGrid.add(createLinkCard("Email Support", "mailto:support@anahata.uno", 
                "Send us a direct message at support@anahata.uno", "email.png"), gridGbc);

        gridGbc.gridx = 1;
        linksGrid.add(createLinkCard("Official Website", "https://anahata.uno/", 
                "Learn more about the Anahata ecosystem.", "anahata.png"), gridGbc);

        gridGbc.gridx = 0; gridGbc.gridy = 2;
        linksGrid.add(createLinkCard("AnahataTV (YouTube)", "https://www.youtube.com/@anahata108", 
                "Watch tutorials and feature showcases.", "youtube.png"), gridGbc);

        gridGbc.gridx = 1;
        linksGrid.add(createLinkCard("Browse Javadocs", "https://asi.anahata.uno/apidocs/", 
                "Technical documentation and API reference.", "javadoc.png"), gridGbc);

        gbc.gridy++;
        gbc.insets = new Insets(0, 20, 20, 20);
        gbc.fill = GridBagConstraints.NONE; // Do not stretch the grid
        gbc.anchor = GridBagConstraints.NORTHWEST;
        gbc.weighty = 0.0; 
        add(linksGrid, gbc);

        // Add a spacer at the bottom to push everything up
        gbc.gridy++;
        gbc.weighty = 1.0;
        add(Box.createVerticalGlue(), gbc);
    }

    /**
     * Creates a link card component.
     * @param title The title of the card.
     * @param url The URL to open.
     * @param description The description text.
     * @param iconName The name of the icon file.
     * @return A JPanel representing the link card.
     */
    private JPanel createLinkCard(String title, String url, String description, String iconName) {
        JPanel card = new JPanel(new BorderLayout(5, 2));
        card.setOpaque(false);
        card.setPreferredSize(new Dimension(300, 60));
        
        JButton btn = new JButton(title, IconUtils.getIcon(iconName, 16, 16));
        btn.setPreferredSize(new Dimension(250, 35));
        btn.setHorizontalAlignment(SwingConstants.LEFT);
        btn.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
        btn.addActionListener(e -> openWebpage(url));
        
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
     * Opens the specified URL in the default system browser.
     * @param url The URL to open.
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
