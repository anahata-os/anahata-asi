/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing;

import java.awt.BorderLayout;
import java.awt.Cursor;
import java.awt.Desktop;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import lombok.extern.slf4j.Slf4j;
import net.miginfocom.swing.MigLayout;
import org.jdesktop.swingx.prompt.PromptSupport;
import uno.anahata.asi.agi.provider.AbstractAgiProvider;

/**
 * A reusable panel for managing API keys for a specific AI provider.
 * It provides a clean monospace editor for the provider's 'api_keys.txt' file
 * and a link to acquire new keys.
 * 
 * @author anahata
 */
@Slf4j
public class ProviderKeysPanel extends JPanel {

    private final AbstractAgiProvider provider;
    private final JTextArea textArea;

    /**
     * Constructs a new panel for the specified provider.
     * 
     * @param provider The provider to manage keys for.
     */
    public ProviderKeysPanel(AbstractAgiProvider provider) {
        super(new BorderLayout(5, 5));
        this.provider = provider;

        this.textArea = new JTextArea();
        this.textArea.setFont(new Font(Font.MONOSPACED, Font.PLAIN, 13));
        
        // Setup PromptSupport for the template hint
        PromptSupport.setPrompt(provider.getApiKeyHint(), textArea);
        PromptSupport.setFocusBehavior(PromptSupport.FocusBehavior.SHOW_PROMPT, textArea);
        PromptSupport.setForeground(java.awt.Color.GRAY, textArea);
        
        // Add acquisition link and Round-Robin explanation
        JPanel headerPanel = new JPanel(new MigLayout("fillx, insets 0", "[grow]", "[]0[]"));
        
        if (provider.getKeysAcquisitionUri() != null) {
            JLabel linkLabel = new JLabel("<html><a href=''>Get " + provider.getProviderId() + " API Keys</a></html>");
            linkLabel.setCursor(new Cursor(Cursor.HAND_CURSOR));
            linkLabel.addMouseListener(new MouseAdapter() {
                @Override
                public void mouseClicked(MouseEvent e) {
                    try {
                        Desktop.getDesktop().browse(provider.getKeysAcquisitionUri());
                    } catch (Exception ex) {
                        log.error("Failed to open acquisition URI", ex);
                    }
                }
            });
            headerPanel.add(linkLabel, "wrap");
        }
        
        JLabel tipLabel = new JLabel("<html><font color='#707070'><i><b>Pro Tip:</b> You can add multiple keys (one per Gmail account) to create a 'Key Pool'. The ASI will rotate through them in a <b>Round-Robin</b> fashion if one reaches quota limits or fails.</i></font></html>");
        headerPanel.add(tipLabel, "growx, wrap, gaptop 5, gapbottom 10");
        
        JButton saveBtn = new JButton("Save & Reload");
        saveBtn.addActionListener(e -> saveKeys());
        
        JPanel footerPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));
        footerPanel.add(saveBtn);

        add(headerPanel, BorderLayout.NORTH);
        add(new JScrollPane(textArea), BorderLayout.CENTER);
        add(footerPanel, BorderLayout.SOUTH);

        loadKeys();
    }

    /**
     * Loads the keys from the provider's configuration file.
     */
    private void loadKeys() {
        Path path = provider.getKeysFilePath();
        try {
            if (Files.exists(path)) {
                textArea.setText(Files.readString(path));
            }
        } catch (IOException e) {
            log.error("Failed to load keys from {}", path, e);
            textArea.setText("# Error loading keys: " + e.getMessage());
        }
    }

    /**
     * Saves the current text to the provider's file and triggers a key pool reload.
     */
    private void saveKeys() {
        Path path = provider.getKeysFilePath();
        try {
            Files.writeString(path, textArea.getText());
            provider.reloadKeyPool();
            JOptionPane.showMessageDialog(this, 
                    "API keys for '" + provider.getProviderId() + "' saved and reloaded.", 
                    "Success", JOptionPane.INFORMATION_MESSAGE);
        } catch (IOException e) {
            log.error("Failed to save keys to {}", path, e);
            JOptionPane.showMessageDialog(this, 
                    "Failed to save keys: " + e.getMessage(), 
                    "Error", JOptionPane.ERROR_MESSAGE);
        }
    }
}
