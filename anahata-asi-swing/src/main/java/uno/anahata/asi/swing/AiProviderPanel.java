/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing;

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
import uno.anahata.asi.openai.compatible.OpenAiCompatibleProvider;
import javax.swing.JFileChooser;
import javax.swing.JTabbedPane;
import uno.anahata.asi.anthropic.AnthropicProvider;
import uno.anahata.asi.swing.icons.PulseIcon;
import uno.anahata.asi.swing.icons.DeleteIcon;
import uno.anahata.asi.swing.icons.ExternalIcon;
import uno.anahata.asi.swing.internal.AnyChangeDocumentListener;
import uno.anahata.asi.swing.internal.SwingTask;

import uno.anahata.asi.swing.components.ScrollablePanel;

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
public class AiProviderPanel extends ScrollablePanel {

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
     * The pending folder name selected by the user.
     * <p>
     * This acts as an edit buffer. Changes are only committed to the provider
     * domain object when {@link #syncToProvider()} is invoked.
     * </p>
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

    private final JTextArea allowedModelsArea;

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
     * The version header for Anthropic API calls.
     */
    private JTextField anthropicVersionField;
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
        this.currentFolderName = provider.getFolderName();
        setOpaque(false);
        this.allowedModelsArea = new JTextArea(3, 20);
        this.allowedModelsArea.setFont(new Font(Font.MONOSPACED, Font.PLAIN, 12));
        if (provider.getAllowedModels() != null && !provider.getAllowedModels().isEmpty()) {
            this.allowedModelsArea.setText(String.join("\n", provider.getAllowedModels()));
        }
        PromptSupport.setPrompt("model-id-1\nmodel-id-2\nOne per line... (leave empty to allow all)", allowedModelsArea);
        this.acquisitionLinkLabel = new JLabel();
        this.folderLabel = new JLabel();
        updateFolderLabel();
        updateLinkLabel();
        this.textArea = new JTextArea();
        this.textArea.setFont(new Font(Font.MONOSPACED, Font.PLAIN, 13));
        PromptSupport.setPrompt(provider.getApiKeyHint(), textArea);
        PromptSupport.setFocusBehavior(PromptSupport.FocusBehavior.HIDE_PROMPT, textArea);
        PromptSupport.setForeground(Color.GRAY, textArea);
        setLayout(new MigLayout("fillx, insets 10", "[right]10[grow,fill]5[]"));

        JButton removeBtn = new JButton(new DeleteIcon(16));
        removeBtn.setToolTipText("Remove Provider");
        removeBtn.addActionListener(e -> removeCallback.run());
        add(removeBtn, "span 3, right, wrap");

        add(new JLabel("UUID:"));
        JLabel uuidLabel = new JLabel(provider.getUuid());
        uuidLabel.setFont(uuidLabel.getFont().deriveFont(Font.BOLD));
        add(uuidLabel, "span 2, wrap");

        add(new JLabel("Provider Class:"));
        JTextField classField = new JTextField(provider.getClass().getName());
        classField.setEditable(false);
        classField.setBorder(null);
        classField.setOpaque(false);
        classField.setFont(classField.getFont().deriveFont(Font.ITALIC, 11.0F));
        add(classField, "span 2, wrap");

        add(new JLabel("Enabled:"));
        enabledCheck = new JCheckBox("", provider.isEnabled());
        add(enabledCheck, "span 2, wrap, gapbottom 10");

        add(new JLabel("Display Name:"));
        displayNameField = new JTextField(provider.getDisplayName());
        displayNameField.getDocument().addDocumentListener(new AnyChangeDocumentListener(() -> {
            updateLinkLabel();
            Container parent = getParent();
            // Drill up through the scroll pane viewport to find the tabs
            if (parent != null && parent.getParent() != null && parent.getParent().getParent() instanceof JTabbedPane tabs) {
                int idx = tabs.indexOfComponent(parent.getParent());
                if (idx != -1) {
                    tabs.setTitleAt(idx, displayNameField.getText().trim());
                }
            }
        }));
        add(displayNameField, "span 2, wrap");

        add(new JLabel("Storage Folder:"));
        add(folderLabel);
        JPanel folderButtons = new JPanel(new FlowLayout(FlowLayout.LEFT, 2, 0));
        folderButtons.setOpaque(false);
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
        add(folderButtons, "wrap");

        add(new JLabel("Tokenizer Type:"), "gaptop 5");
        tokenizerCombo = new JComboBox<>(TokenizerType.values());
        tokenizerCombo.setSelectedItem(provider.getTokenizerType());
        add(tokenizerCombo, "span 2, wrap");

        // --- Allowed Models ---
        add(new JLabel("Allowed Models:"), "top, gaptop 5");
        
        JButton fillModelsBtn = new JButton("Fetch Models", new PulseIcon(12));
        fillModelsBtn.setToolTipText("Fetch available models from provider and populate this list.");
        fillModelsBtn.addActionListener(e -> {
            try {
                syncToProvider(); // Ensure URL and API keys are synced
                new SwingTask<>(containerPanel, "Fetching Models", () -> {
                    return provider.refreshModels().stream().map(uno.anahata.asi.agi.provider.AbstractModel::getModelId).collect(Collectors.toList());
                }, models -> {
                    if(models.isEmpty()) {
                        JOptionPane.showMessageDialog(this, "No models were discovered. Check API keys and connection.");
                    } else {
                        allowedModelsArea.setText(String.join("\n", models));
                    }
                }).start();
            } catch (IOException ex) {
                log.error("Failed to sync before fetching models", ex);
                JOptionPane.showMessageDialog(this, "Pre-fetch sync failed: " + ex.getMessage());
            }
        });
        
        allowedModelsArea.setRows(5);
        allowedModelsArea.setBorder(BorderFactory.createEmptyBorder(2, 2, 2, 2));
        JScrollPane allowedScroll = new JScrollPane(allowedModelsArea);
        allowedScroll.setBorder(BorderFactory.createLineBorder(Color.LIGHT_GRAY));
        add(allowedScroll, "span 2, growx, wrap");
        
        JPanel fetchBtnContainer = new JPanel(new FlowLayout(FlowLayout.LEFT, 0, 0));
        fetchBtnContainer.setOpaque(false);
        fetchBtnContainer.add(fillModelsBtn);
        add(fetchBtnContainer, "skip 1, span 2, wrap");

        add(new JLabel("API Key Required:"), "gaptop 5");
        apiKeyRequiredCheck = new JCheckBox("", provider.isApiKeyRequired());
        apiKeyRequiredCheck.setOpaque(false);
        apiKeyRequiredCheck.addActionListener(e -> {
            textArea.setEnabled(apiKeyRequiredCheck.isSelected());
            textArea.setBackground(apiKeyRequiredCheck.isSelected() ? Color.WHITE : new Color(245, 245, 245));
        });
        add(apiKeyRequiredCheck, "span 2, wrap");

        add(new JLabel("Base URL:"));
        baseUrlField = new JTextField(provider.getBaseUrl());
        add(baseUrlField, "span 2, wrap");

        if (provider instanceof AnthropicProvider anthropic) {
            add(new JLabel("Anthropic Version:"));
            anthropicVersionField = new JTextField(anthropic.getAnthropicVersion());
            add(anthropicVersionField, "span 2, wrap");
        }

        if (provider instanceof OpenAiCompatibleProvider oai) {
            add(new JLabel("Custom Headers:"), "top, gaptop 5");
            customHeadersArea = new JTextArea(3, 20);
            customHeadersArea.setFont(new Font(Font.MONOSPACED, Font.PLAIN, 12));
            if (oai.getCustomHeaders() != null) {
                String headers = oai.getCustomHeaders().entrySet().stream()
                        .map(entry -> entry.getKey() + ": " + entry.getValue())
                        .collect(Collectors.joining("\n"));
                customHeadersArea.setText(headers);
            }
            PromptSupport.setPrompt("Header-Name: Header-Value\nOne per line...", customHeadersArea);
            JScrollPane headersScroll = new JScrollPane(customHeadersArea);
            headersScroll.setBorder(BorderFactory.createLineBorder(Color.LIGHT_GRAY));
            add(headersScroll, "span 2, growx, wrap");

            add(new JLabel("Prefer HTTP/1.1:"), "gaptop 5");
            preferHttp11Check = new JCheckBox("", oai.isPreferHttp11());
            preferHttp11Check.setOpaque(false);
            preferHttp11Check.setToolTipText("Force HTTP/1.1 to avoid protocol hangs on some local servers/routers.");
            add(preferHttp11Check, "span 2, wrap");
        }

        // --- Key Pool Section ---
        add(new JLabel("API Key Pool:"), "top, gaptop 10");
        JPanel keysContainer = new JPanel(new MigLayout("ins 0, fill", "[grow,fill]", "[][][grow,fill]"));
        keysContainer.setOpaque(false);
        
        JPanel keysHeader = new JPanel(new FlowLayout(FlowLayout.LEFT, 0, 0));
        keysHeader.setOpaque(false);
        JLabel tipLabel = new JLabel("<html><font color='#707070'><i><b>Pro Tip:</b> Add multiple keys (one per line) for Round-Robin rotation.</i></font></html>");
        keysHeader.add(tipLabel);
        keysContainer.add(keysHeader, "wrap");

        if (provider.getKeysAcquisitionUri() != null) {
            keysContainer.add(acquisitionLinkLabel, "wrap, gapleft 5");
        }

        textArea.setRows(7);
        textArea.addMouseWheelListener(e -> uno.anahata.asi.swing.internal.SwingUtils.redispatchMouseWheelEvent(textArea, e));
        textArea.setBorder(BorderFactory.createEmptyBorder(2, 2, 2, 2));
        JScrollPane textScroll = new JScrollPane(textArea);
        textScroll.setBorder(BorderFactory.createLineBorder(Color.LIGHT_GRAY));
        keysContainer.add(textScroll, "grow, wrap");
        
        add(keysContainer, "span 2, grow, wrap");

        testConnectionBtn = new JButton("Test Connection (Discover Models)", new PulseIcon(16));
        testConnectionBtn.addActionListener(e -> testConnection());
        add(testConnectionBtn, "span 3, right, gaptop 20");

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
            
            new SwingTask<>(containerPanel, "Testing Connection", () -> {
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

        provider.setFolderName(currentFolderName);
        updateFolderLabel();
        provider.setTokenizerType((TokenizerType) tokenizerCombo.getSelectedItem());

        String allowed = allowedModelsArea.getText().trim();
        if (allowed.isEmpty()) {
            provider.setAllowedModels(new java.util.ArrayList<>());
        } else {
            provider.setAllowedModels(java.util.Arrays.stream(allowed.split("\\s+"))
                    .map(String::trim)
                    .filter(s -> !s.isEmpty())
                    .collect(java.util.stream.Collectors.toList()));
        }

        if (baseUrlField != null) {
            provider.setBaseUrl(baseUrlField.getText().trim());
        }

        if (provider instanceof uno.anahata.asi.anthropic.AnthropicProvider anthropic && anthropicVersionField != null) {
            anthropic.setAnthropicVersion(anthropicVersionField.getText().trim());
        }

        if (provider instanceof OpenAiCompatibleProvider oai) {
            oai.setPreferHttp11(preferHttp11Check.isSelected());
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
