/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Container;
import java.awt.Cursor;
import java.awt.Desktop;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.Map;
import java.util.stream.Collectors;
import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import lombok.extern.slf4j.Slf4j;
import net.miginfocom.swing.MigLayout;
import org.jdesktop.swingx.prompt.PromptSupport;
import uno.anahata.asi.agi.provider.AbstractAiProvider;
import uno.anahata.asi.agi.provider.TokenizerType;
import uno.anahata.asi.openai.OpenAiCompatibleProvider;
import javax.swing.JFileChooser;
import javax.swing.JTabbedPane;
import javax.swing.SwingConstants;
import uno.anahata.asi.swing.icons.PulseIcon;
import uno.anahata.asi.swing.icons.DeleteIcon;
import uno.anahata.asi.swing.icons.ExternalIcon;
import uno.anahata.asi.swing.internal.AnyChangeDocumentListener;
import uno.anahata.asi.swing.internal.SwingTask;

/**
 * A centralized, high-density configuration panel for AI Providers.
 * <p>
 * This panel governs both the connectivity parameters (Base URL, Custom
 * Headers) and the metabolic identity (Tokenizer Type) of a provider. It
 * features a professional monochromatic API key editor with support for "Key
 * Pools" and round-robin rotation.
 * </p>
 *
 * @author anahata
 */
@Slf4j
public class AiProviderPanel extends JPanel {

    /**
     * The parent container panel providing access to the global executor.
     */
    private final AbstractAsiContainerPanel containerPanel;
    /**
     * The domain entity representing the AI provider being configured.
     */
    private final AbstractAiProvider provider;
    /**
     * Monospace editor for the 'api_keys.txt' file, supporting multiple keys.
     */
    private final JTextArea textArea;
    /**
     * User-facing name for this provider instance.
     */
    private final JTextField displayNameField;
    /**
     * Visual indicator of where this provider's data is stored on the host FS.
     */
    private final JLabel folderLabel;
    /**
     * The actual folder name, either custom or derived from the UUID.
     */
    private String currentFolderName;
    /**
     * Master switch to enable/disable the provider globally.
     */
    private final JCheckBox enabledCheck;
    /**
     * Determines if the key pool must be populated for this provider to
     * function.
     */
    private final JCheckBox apiKeyRequiredCheck;
    /**
     * Selector for the tokenizer used for pre-flight metabolic estimations.
     */
    private final JComboBox<TokenizerType> tokenizerCombo;

    /**
     * Toggle for forcing HTTP/1.1 on OpenAI-compatible providers.
     */
    private JCheckBox preferHttp11Check;

    // --- OpenAI Compatible Extensions ---
    /**
     * The endpoint root for Chat Completion API calls.
     */
    private JTextField baseUrlField;
    /**
     * Vendor-specific quirks defined as Key: Value headers.
     */
    private JTextArea customHeadersArea;
    /**
     * Triggers an immediate model discovery probe to verify the URL and Auth.
     */
    private JButton testConnectionBtn;

    /**
     * Link to the API key acquisition page.
     */
    private final JLabel acquisitionLinkLabel;

    /**
     * Constructs a new provider configuration panel.
     *
     * @param containerPanel The parent container dashboard.
     * @param provider The provider instance to bind to.
     * @param removeCallback Callback to trigger when the user deletes the
     * provider.
     */
    public AiProviderPanel(AbstractAsiContainerPanel containerPanel, AbstractAiProvider provider, Runnable removeCallback) {
        this.containerPanel = containerPanel;
        this.provider = provider;
        this.acquisitionLinkLabel = new JLabel();
        this.folderLabel = new JLabel();
        updateFolderLabel();
        updateLinkLabel();
        this.textArea = new JTextArea();
        this.textArea.setFont(new Font(Font.MONOSPACED, Font.PLAIN, 13));
        PromptSupport.setPrompt(provider.getApiKeyHint(), textArea);
        PromptSupport.setFocusBehavior(PromptSupport.FocusBehavior.HIDE_PROMPT, textArea);
        PromptSupport.setForeground(Color.GRAY, textArea);
        JPanel configPanel = new JPanel(new MigLayout("fillx, insets 10", "[right]10[grow,fill]5[]"));
        JButton removeBtn = new JButton(new DeleteIcon(16));
        removeBtn.setToolTipText("Remove Provider");
        removeBtn.addActionListener(e -> removeCallback.run());
        configPanel.add(removeBtn, "span 3, right, wrap");
        configPanel.add(new JLabel("UUID:"));
        JLabel uuidLabel = new JLabel(provider.getUuid());
        uuidLabel.setFont(uuidLabel.getFont().deriveFont(Font.BOLD));
        configPanel.add(uuidLabel, "span 2, wrap");
        configPanel.add(new JLabel("Provider Class:"));
        JTextField classField = new JTextField(provider.getClass().getName());
        classField.setEditable(false);
        classField.setBorder(null);
        classField.setOpaque(false);
        classField.setFont(classField.getFont().deriveFont(Font.ITALIC, 11.0F));
        configPanel.add(classField, "span 2, wrap");
        configPanel.add(new JLabel("Enabled:"));
        enabledCheck = new JCheckBox("", provider.isEnabled());
        configPanel.add(enabledCheck, "span 2, wrap, gapbottom 10");
        configPanel.add(new JLabel("Display Name:"));
        displayNameField = new JTextField(provider.getDisplayName());
        displayNameField.getDocument().addDocumentListener(new AnyChangeDocumentListener(() -> {
            updateLinkLabel();
            Container parent = getParent();
            if (parent instanceof JTabbedPane tabs) {
                int idx = tabs.indexOfComponent(this);
                if (idx != -1) {
                    tabs.setTitleAt(idx, displayNameField.getText().trim());
                }
            }
            if (currentFolderName == null || currentFolderName.isBlank()) {
                String suggested = displayNameField.getText().trim().replaceAll("[^a-zA-Z0-9.-]", "_");
                if (!suggested.isEmpty()) {
                    folderLabel.setText("<html><i>Suggested: </i><b>" + suggested + "</b></html>");
                } else {
                    updateFolderLabel();
                }
            }
        }));
        configPanel.add(displayNameField, "span 2, wrap");
        configPanel.add(new JLabel("Storage Folder:"));
        configPanel.add(folderLabel);
        JPanel folderButtons = new JPanel(new FlowLayout(FlowLayout.LEFT, 2, 0));
        JButton chooseFolderBtn = new JButton("Choose...");
        chooseFolderBtn.addActionListener(e -> {
            JFileChooser chooser = new JFileChooser();
            Path current = provider.getProviderDirectory();
            if (Files.exists(current)) {
                chooser.setCurrentDirectory(current.toFile());
            }
            chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
            if (chooser.showOpenDialog(this) == JFileChooser.APPROVE_OPTION) {
                currentFolderName = chooser.getSelectedFile().getAbsolutePath();
                updateFolderLabel();
            }
        });
        folderButtons.add(chooseFolderBtn);
        JButton openFolderBtn = new JButton(new ExternalIcon(16));
        openFolderBtn.setToolTipText("Open Provider Folder in Desktop");
        openFolderBtn.addActionListener(e -> {
            try {
                Desktop.getDesktop().open(provider.getProviderDirectory().toFile());
            } catch (Exception ex) {
                log.error("Failed to open directory", ex);
                JOptionPane.showMessageDialog(this, "Could not open directory: " + ex.getMessage());
            }
        });
        folderButtons.add(openFolderBtn);
        configPanel.add(folderButtons, "wrap");
        configPanel.add(new JLabel("Tokenizer Type:"), "gaptop 5");
        tokenizerCombo = new JComboBox<>(TokenizerType.values());
        tokenizerCombo.setSelectedItem(provider.getTokenizerType());
        configPanel.add(tokenizerCombo, "span 2, wrap");
        apiKeyRequiredCheck = new JCheckBox("API Key Required", provider.isApiKeyRequired());
        apiKeyRequiredCheck.setHorizontalTextPosition(SwingConstants.LEFT);
        apiKeyRequiredCheck.addActionListener(e -> {
            textArea.setEnabled(apiKeyRequiredCheck.isSelected());
            textArea.setBackground(apiKeyRequiredCheck.isSelected() ? Color.WHITE : new Color(245, 245, 245));
        });
        if (provider instanceof OpenAiCompatibleProvider oai) {
            configPanel.add(new JLabel("Base URL:"));
            baseUrlField = new JTextField(oai.getBaseUrl());
            configPanel.add(baseUrlField, "span 2, wrap");
            configPanel.add(new JLabel("Custom Headers:"), "top, gaptop 5");
            customHeadersArea = new JTextArea(3, 20);
            customHeadersArea.setFont(new Font(Font.MONOSPACED, Font.PLAIN, 12));
            if (oai.getCustomHeaders() != null) {
                String headers = oai.getCustomHeaders().entrySet().stream()
                        .map(entry -> entry.getKey() + ": " + entry.getValue())
                        .collect(Collectors.joining("\n"));
                customHeadersArea.setText(headers);
            }
            PromptSupport.setPrompt("Header-Name: Header-Value\nOne per line...", customHeadersArea);
            configPanel.add(new JScrollPane(customHeadersArea), "span 2, growx, wrap");
            configPanel.add(new JLabel("Prefer HTTP/1.1:"), "gaptop 5");
            preferHttp11Check = new JCheckBox("", oai.isPreferHttp11());
            preferHttp11Check.setToolTipText("Force HTTP/1.1 to avoid protocol hangs on some local servers/routers.");
            configPanel.add(preferHttp11Check, "span 2, wrap");
        }

        testConnectionBtn = new JButton("Test Connection (Discover Models)", new PulseIcon(16));
        testConnectionBtn.addActionListener(e -> testConnection());

        setLayout(new BorderLayout(5, 5));

        // --- 2. Keys Section (High Density Center) ---
        JPanel keysSection = new JPanel(new BorderLayout());
        JPanel keysHeader = new JPanel(new MigLayout("fillx, insets 0", "[left]10[grow,fill]"));
        keysHeader.setBackground(getBackground());

        keysHeader.add(apiKeyRequiredCheck, "gapleft 10");

        JLabel tipLabel = new JLabel("<html><font color='#707070'><i><b>Pro Tip:</b> Add multiple api keys to make a <i>Key Pool</i> (one per line) for <i>Round-Robin</> rotation</font></html>");
        keysHeader.add(tipLabel, "wrap");

        if (provider.getKeysAcquisitionUri() != null) {
            keysHeader.add(acquisitionLinkLabel, "span 2, wrap, gapleft 5");
        }

        keysSection.add(keysHeader, BorderLayout.NORTH);

        JScrollPane scrollPane = new JScrollPane(textArea);
        scrollPane.setBorder(BorderFactory.createMatteBorder(1, 0, 1, 0, Color.LIGHT_GRAY));
        keysSection.add(scrollPane, BorderLayout.CENTER);

        add(configPanel, BorderLayout.NORTH);
        add(keysSection, BorderLayout.CENTER);

        if (testConnectionBtn != null) {
            JPanel footer = new JPanel(new FlowLayout(FlowLayout.RIGHT));
            footer.add(testConnectionBtn);
            add(footer, BorderLayout.SOUTH);
        }

        // Initial state sync
        textArea.setEnabled(provider.isApiKeyRequired());
        textArea.setBackground(provider.isApiKeyRequired() ? Color.WHITE : new Color(245, 245, 245));
        loadKeys();
    }

    /**
     * Performs a non-blocking model discovery probe. Automatically synchronizes
     * UI state to the object and key file before testing.
     */
    private void testConnection() {
        try {
            // Force sync to ensure keys and URL are latest
            syncToProvider();
            
            new SwingTask<Integer>(containerPanel, "Testing Connection", () -> {
                var models = provider.refreshModels();
                if (models.isEmpty()) {
                    throw new Exception("Discovery returned 0 models. Check your URL and API Keys.");
                }
                return models.size();
            }, count -> {
                JOptionPane.showMessageDialog(this,
                        "Connection successful! Discovered " + count + " models.",
                        "Success", JOptionPane.INFORMATION_MESSAGE);
            }).start();
        } catch (IOException ex) {
            log.error("Failed to sync before test", ex);
            JOptionPane.showMessageDialog(this, "Pre-test sync failed: " + ex.getMessage());
        }
    }

    private void updateLinkLabel() {
        if (provider.getKeysAcquisitionUri() == null) {
            return;
        }
        String name = displayNameField != null ? displayNameField.getText().trim() : provider.getDisplayName();
        if (name.isBlank()) {
            name = "Provider";
        }

        acquisitionLinkLabel.setText("<html><a href=''>" + name + " - Get API Keys</a></html>");
        acquisitionLinkLabel.setBorder(BorderFactory.createEmptyBorder(5, 10, 5, 10));
        acquisitionLinkLabel.setCursor(new Cursor(Cursor.HAND_CURSOR));
        // Remove existing listeners to avoid duplicates
        for (var l : acquisitionLinkLabel.getMouseListeners()) {
            acquisitionLinkLabel.removeMouseListener(l);
        }

        acquisitionLinkLabel.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                try {
                    Desktop.getDesktop().browse(provider.getKeysAcquisitionUri());
                } catch (Exception ex) {
                    log.error("Failed to open acquisition URI", ex);
                }
            }
        });
    }

    /**
     * Updates the storage folder label.
     */
    private void updateFolderLabel() {
        if (currentFolderName == null || currentFolderName.isBlank()) {
            folderLabel.setText("<html><i>Default (" + provider.getUuid() + ")</i></html>");
        } else {
            folderLabel.setText(currentFolderName);
            folderLabel.setToolTipText(currentFolderName);
        }
    }

    /**
     * Loads the raw key pool text.
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
     * Synchronizes the UI state back to the provider domain and flushes the key
     * pool to disk. This is called by the parent preferences panel.
     */
    public void syncToProvider() throws IOException {
        provider.setDisplayName(displayNameField.getText().trim());
        provider.setEnabled(enabledCheck.isSelected());
        provider.setApiKeyRequired(apiKeyRequiredCheck.isSelected());

        if (currentFolderName == null || currentFolderName.isBlank()) {
            currentFolderName = displayNameField.getText().trim().replaceAll("[^a-zA-Z0-9.-]", "_");
        }
        provider.setFolderName(currentFolderName);
        updateFolderLabel();
        provider.setTokenizerType((TokenizerType) tokenizerCombo.getSelectedItem());

        if (provider instanceof OpenAiCompatibleProvider oai) {
            oai.setPreferHttp11(preferHttp11Check.isSelected());
            if (baseUrlField != null) {
                oai.setBaseUrl(baseUrlField.getText().trim());
            }
            if (customHeadersArea != null) {
                Map<String, String> headers = new HashMap<>();
                String text = customHeadersArea.getText().trim();
                if (!text.isEmpty()) {
                    for (String line : text.split("\n")) {
                        int colon = line.indexOf(":");
                        if (colon > 0) {
                            headers.put(line.substring(0, colon).trim(), line.substring(colon + 1).trim());
                        }
                    }
                }
                oai.setCustomHeaders(headers);
            }
        }

        Path path = provider.getKeysFilePath();
        Files.writeString(path, textArea.getText());
        provider.reloadKeyPool();
    }
}
