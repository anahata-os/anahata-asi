/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing;

import java.awt.BorderLayout;
import java.awt.Color;
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
import uno.anahata.asi.agi.provider.AbstractAgiProvider;
import uno.anahata.asi.agi.provider.TokenizerType;
import uno.anahata.asi.openai.OpenAiCompatibleProvider;
import javax.swing.JFileChooser;
import javax.swing.SwingConstants;
import uno.anahata.asi.swing.icons.DeleteIcon;
import uno.anahata.asi.swing.icons.ExternalIcon;
import uno.anahata.asi.swing.internal.AnyChangeDocumentListener;
import uno.anahata.asi.swing.internal.SwingTask;

/**
 * A centralized, high-density configuration panel for AI Providers.
 * <p>
 * This panel governs both the connectivity parameters (Base URL, Custom Headers) 
 * and the metabolic identity (Tokenizer Type) of a provider. It features a 
 * professional monochromatic API key editor with support for "Key Pools" 
 * and round-robin rotation.
 * </p>
 * 
 * @author anahata
 */
@Slf4j
public class AiProviderPanel extends JPanel {

    /** The domain entity representing the AI provider being configured. */
    private final AbstractAgiProvider provider;
    /** Monospace editor for the 'api_keys.txt' file, supporting multiple keys. */
    private final JTextArea textArea;
    /** User-facing name for this provider instance. */
    private final JTextField displayNameField;
    /** Visual indicator of where this provider's data is stored on the host FS. */
    private final JLabel folderLabel;
    /** The actual folder name, either custom or derived from the UUID. */
    private String currentFolderName;
    /** Master switch to enable/disable the provider globally. */
    private final JCheckBox enabledCheck;
    /** Determines if the key pool must be populated for this provider to function. */
    private final JCheckBox apiKeyRequiredCheck;
    /** Selector for the tokenizer used for pre-flight metabolic estimations. */
    private final JComboBox<TokenizerType> tokenizerCombo;
    /** Reactive callback invoked upon successful configuration persistence. */
    private final Runnable saveSuccessCallback;
    
    // --- OpenAI Compatible Extensions ---
    /** The endpoint root for Chat Completion API calls. */
    private JTextField baseUrlField;
    /** Vendor-specific quirks defined as Key: Value headers. */
    private JTextArea customHeadersArea;
    /** Triggers an immediate model discovery probe to verify the URL and Auth. */
    private JButton testConnectionBtn;

    /**
     * Constructs a new provider configuration panel.
     * 
     * @param provider The provider instance to bind to.
     * @param removeCallback Callback to trigger when the user deletes the provider.
     * @param saveSuccessCallback Callback to notify the container of preference updates.
     */
    public AiProviderPanel(AbstractAgiProvider provider, Runnable removeCallback, Runnable saveSuccessCallback) {
        super(new BorderLayout(5, 5));
        this.provider = provider;
        this.saveSuccessCallback = saveSuccessCallback;
        this.currentFolderName = provider.getFolderName();
        
        // 0. Initialize UI Components
        this.folderLabel = new JLabel();
        updateFolderLabel();
        
        this.textArea = new JTextArea();
        this.textArea.setFont(new Font(Font.MONOSPACED, Font.PLAIN, 13));
        PromptSupport.setPrompt(provider.getApiKeyHint(), textArea);
        PromptSupport.setFocusBehavior(PromptSupport.FocusBehavior.HIDE_PROMPT, textArea);
        PromptSupport.setForeground(Color.GRAY, textArea);
        
        // --- 1. Config Panel (Header/Metadata) ---
        JPanel configPanel = new JPanel(new MigLayout("fillx, insets 10", "[right]10[grow,fill]5[]"));
        
        // Top Row: Delete Button (Top Right)
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
        classField.setFont(classField.getFont().deriveFont(Font.ITALIC, 11f));
        configPanel.add(classField, "span 2, wrap");
        
        configPanel.add(new JLabel("Enabled:"));
        enabledCheck = new JCheckBox("", provider.isEnabled());
        configPanel.add(enabledCheck, "span 2, wrap, gapbottom 10");

        configPanel.add(new JLabel("Display Name:"));
        displayNameField = new JTextField(provider.getDisplayName());
        displayNameField.getDocument().addDocumentListener(new AnyChangeDocumentListener(() -> {
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
            
            testConnectionBtn = new JButton("Test Connection (Discover Models)");
            testConnectionBtn.addActionListener(e -> testConnection());
            configPanel.add(testConnectionBtn, "skip 1, span 2, wrap, gaptop 10");
        }

        // --- 2. Keys Section (High Density Center) ---
        JPanel keysSection = new JPanel(new BorderLayout());
        JPanel keysHeader = new JPanel(new MigLayout("fillx, insets 0", "[left]10[grow,fill]"));
        keysHeader.setBackground(getBackground());
        
        keysHeader.add(apiKeyRequiredCheck, "gapleft 10");
        
        JLabel tipLabel = new JLabel("<html><font color='#707070'><i><b>Pro Tip:</b> Add multiple api keys to make a <i>Key Pool</i> (one per line) for <i>Round-Robin</> rotation</font></html>");
        keysHeader.add(tipLabel, "wrap");
        
        keysSection.add(keysHeader, BorderLayout.NORTH);
        
        JScrollPane scrollPane = new JScrollPane(textArea);
        scrollPane.setBorder(BorderFactory.createMatteBorder(1, 0, 1, 0, Color.LIGHT_GRAY));
        keysSection.add(scrollPane, BorderLayout.CENTER);
        
        if (provider.getKeysAcquisitionUri() != null) {
            JLabel linkLabel = new JLabel("<html><a href=''>" + provider.getDisplayName() + " - Get API Keys</a></html>");
            linkLabel.setBorder(BorderFactory.createEmptyBorder(5, 10, 5, 10));
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
            keysSection.add(linkLabel, BorderLayout.SOUTH);
        }

        add(configPanel, BorderLayout.NORTH);
        add(keysSection, BorderLayout.CENTER);
        
        // Initial state sync
        textArea.setEnabled(provider.isApiKeyRequired());
        textArea.setBackground(provider.isApiKeyRequired() ? Color.WHITE : new Color(245, 245, 245));
        loadKeys();
    }

    /**
     * Performs a non-blocking model discovery probe.
     */
    private void testConnection() {
        if (!(provider instanceof OpenAiCompatibleProvider oai)) return;
        
        String originalUrl = oai.getBaseUrl();
        oai.setBaseUrl(baseUrlField.getText().trim());
        
        new SwingTask<Integer>(this, "Testing Connection", () -> {
            try {
                var models = oai.refreshModels();
                if (models.isEmpty()) {
                    throw new Exception("Discovery returned 0 models. Check your URL and API Keys.");
                }
                return models.size();
            } finally {
                oai.setBaseUrl(originalUrl); 
            }
        }, count -> {
            JOptionPane.showMessageDialog(this, 
                "Connection successful! Discovered " + count + " models.", 
                "Success", JOptionPane.INFORMATION_MESSAGE);
        }).execute();
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
     * Synchronizes the UI state back to the provider domain and flushes 
     * the key pool to disk. This is called by the parent preferences panel.
     */
    public void applyToProvider() throws IOException {
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
        
        if (saveSuccessCallback != null) {
            saveSuccessCallback.run();
        }
    }
}
