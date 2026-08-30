/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing;

import java.awt.BorderLayout;
import java.awt.Container;
import java.awt.Cursor;
import java.awt.Desktop;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.Image;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.IOException;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.Map;
import java.util.stream.Collectors;
import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
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
import uno.anahata.asi.openai.compatible.OpenAiChatCompletionsProvider;
import javax.swing.Icon;
import javax.swing.JFileChooser;
import javax.swing.JTabbedPane;
import javax.swing.UIManager;
import uno.anahata.asi.anthropic.AnthropicProvider;
import uno.anahata.asi.openai.OpenAiResponsesProvider;
import uno.anahata.asi.gemini.GeminiAiProvider;
import uno.anahata.asi.swing.icons.PulseIcon;
import uno.anahata.asi.swing.icons.DeleteIcon;
import uno.anahata.asi.swing.icons.ExternalIcon;
import uno.anahata.asi.swing.icons.IconUtils;
import uno.anahata.asi.swing.internal.AnyChangeDocumentListener;
import uno.anahata.asi.swing.provider.AiModelsPanel;

import uno.anahata.asi.swing.components.ScrollablePanel;
import uno.anahata.asi.swing.icons.SaveIcon;
import uno.anahata.asi.swing.internal.SwingUtils;
import uno.anahata.asi.swing.provider.DiscoverModelsTask;

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
     * The parent ASI container instance.
     */
    private final AbstractSwingAsiContainer container;
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
     * Editable description for this provider instance.
     */
    private final JTextField descriptionField;
    /**
     * Visual indicator of where this provider's data is stored on the host FS.
     */
    private final JLabel folderLabel;
    /**
     * The pending custom API keys file path selected by the user.
     */
    private String currentApiKeysPath;
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
     * Checkbox to toggle automatically registering newly discovered models.
     */
    private JCheckBox autoRegisterCheck;

    /**
     * Checkbox to toggle Google Cloud Vertex AI endpoint usage.
     */
    private JCheckBox vertexCheck;

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

    private final AiModelsPanel registryViewer;
    /**
     * Link to the API key acquisition page.
     */
    private final JLabel acquisitionLinkLabel;

    /**
     * Constructs a new provider configuration panel bound directly to a container.
     *
     * @param container The parent ASI container instance.
     * @param provider The provider instance to bind to.
     * @param removeCallback Callback to trigger when the user deletes the provider.
     */
    public AiProviderPanel(AbstractSwingAsiContainer container, AbstractAiProvider provider, Runnable removeCallback) {
        this.container = container;
        this.provider = provider;
        this.currentApiKeysPath = provider.getApiKeysFile();
        setOpaque(false);
        this.acquisitionLinkLabel = new JLabel();
        this.folderLabel = new JLabel();
        updateFolderLabel();
        updateLinkLabel();
        this.textArea = new JTextArea();
        this.textArea.setFont(new Font(Font.MONOSPACED, Font.PLAIN, 13));
        PromptSupport.setPrompt(provider.getApiKeyHint(), textArea);
        PromptSupport.setFocusBehavior(PromptSupport.FocusBehavior.HIDE_PROMPT, textArea);
        PromptSupport.setForeground(UIManager.getColor("Label.disabledForeground"), textArea);
        setLayout(new BorderLayout());

        JPanel formPanel = new JPanel(new MigLayout("fillx, insets 15", "[right]12[grow,fill]5[]"));
        formPanel.setOpaque(false);

        JLabel promoBannerLabel = createPromoBannerLabel();
        if (promoBannerLabel != null) {
            formPanel.add(promoBannerLabel, "span, growx, center, wrap, gapbottom 12");
        }

        JButton removeBtn = new JButton("Delete", new DeleteIcon(16));
        removeBtn.setToolTipText("Remove Provider");
        removeBtn.addActionListener(e -> removeCallback.run());

        JButton saveBtn = new JButton("Save", new SaveIcon(16));
        saveBtn.setToolTipText("Save Provider Configuration & API Keys");
        saveBtn.addActionListener(e -> {
            try {
                syncToProvider();
                provider.persist();
                JOptionPane.showMessageDialog(this, "Provider '" + provider.getDisplayName() + "' saved successfully.", "Saved", JOptionPane.INFORMATION_MESSAGE);
            } catch (Exception ex) {
                log.error("Failed to save provider", ex);
                JOptionPane.showMessageDialog(this, "Failed to save provider: " + ex.getMessage(), "Error", JOptionPane.ERROR_MESSAGE);
            }
        });

        testConnectionBtn = new JButton("Test Connection", new PulseIcon(16));
        testConnectionBtn.addActionListener(e -> testConnection());

        JPanel headerRight = new JPanel(new FlowLayout(FlowLayout.RIGHT, 5, 0));
        headerRight.setOpaque(false);
        headerRight.add(saveBtn);
        headerRight.add(testConnectionBtn);
        headerRight.add(removeBtn);

        JPanel headerLeft = new JPanel(new FlowLayout(FlowLayout.LEFT, 5, 0));
        headerLeft.setOpaque(false);
        Icon providerIcon = IconUtils.getIcon("aiproviders/" + provider.getClass().getName() + ".png", 32, 32);
        if (providerIcon != null) {
            headerLeft.add(new JLabel(providerIcon));
        }
        formPanel.add(headerLeft, "left");
        formPanel.add(headerRight, "span 2, right, wrap");

        formPanel.add(new JLabel("UUID:"));
        JLabel uuidLabel = new JLabel(provider.getUuid());
        uuidLabel.setFont(uuidLabel.getFont().deriveFont(Font.BOLD));
        formPanel.add(uuidLabel, "span 2, wrap");

        formPanel.add(new JLabel("Provider Class:"));
        JTextField classField = new JTextField(provider.getClass().getName());
        classField.setEditable(false);
        classField.setBorder(null);
        classField.setOpaque(false);
        classField.setFont(classField.getFont().deriveFont(Font.ITALIC, 11.0F));
        formPanel.add(classField, "span 2, wrap");

        formPanel.add(new JLabel("Description:"));
        descriptionField = new JTextField(provider.getDescription() != null ? provider.getDescription() : "");
        formPanel.add(descriptionField, "span 2, wrap");

        formPanel.add(new JLabel("Enabled:"));
        enabledCheck = new JCheckBox("", provider.isEnabled());
        enabledCheck.setOpaque(false);
        formPanel.add(enabledCheck, "span 2, wrap, gapbottom 10");

        formPanel.add(new JLabel("Display Name:"));
        displayNameField = new JTextField(provider.getDisplayName());
        displayNameField.getDocument().addDocumentListener(new AnyChangeDocumentListener(() -> {
            updateLinkLabel();
            Container parent = getParent();
            if (parent != null && parent.getParent() != null && parent.getParent().getParent() instanceof JTabbedPane tabs) {
                int idx = tabs.indexOfComponent(parent.getParent());
                if (idx != -1) {
                    tabs.setTitleAt(idx, displayNameField.getText().trim());
                }
            }
        }));
        formPanel.add(displayNameField, "span 2, wrap");

        formPanel.add(new JLabel("Base URL:"));
        baseUrlField = new JTextField(provider.getBaseUrl());
        formPanel.add(baseUrlField, "span 2, wrap");

        formPanel.add(new JLabel("API Key Required:"), "gaptop 5");
        apiKeyRequiredCheck = new JCheckBox("", provider.isApiKeyRequired());
        apiKeyRequiredCheck.setOpaque(false);
        apiKeyRequiredCheck.addActionListener(e -> {
            textArea.setEnabled(apiKeyRequiredCheck.isSelected());
        });
        formPanel.add(apiKeyRequiredCheck, "span 2, wrap");

        // --- Key Pool Section ---
        formPanel.add(new JLabel("API Key Pool:"), "top, gaptop 10");
        JPanel keysContainer = new JPanel(new MigLayout("ins 0, fill", "[grow,fill]", "[][][grow,fill]"));
        keysContainer.setOpaque(false);

        JPanel keysHeader = new JPanel(new FlowLayout(FlowLayout.LEFT, 0, 0));
        keysHeader.setOpaque(false);
        JLabel tipLabel = new JLabel("<html><i><b>Pro Tip:</b> Add multiple keys (one per line) for Round-Robin rotation.</i></html>");
        tipLabel.setForeground(UIManager.getColor("Label.disabledForeground"));
        keysHeader.add(tipLabel);
        keysContainer.add(keysHeader, "wrap");

        if (provider.getKeysAcquisitionUri() != null) {
            keysContainer.add(acquisitionLinkLabel, "wrap, gapleft 5");
        }

        textArea.setRows(5);
        textArea.setLineWrap(true);
        textArea.setWrapStyleWord(false);
        textArea.addMouseWheelListener(e -> SwingUtils.redispatchMouseWheelEvent(textArea, e));
        textArea.setBorder(BorderFactory.createEmptyBorder(4, 4, 4, 4));
        JScrollPane textScroll = new JScrollPane(textArea, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED, JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
        keysContainer.add(textScroll, "grow, wrap");

        formPanel.add(keysContainer, "span 2, grow, wrap");

        formPanel.add(new JLabel("API Keys File:"));
        JPanel folderRow = new JPanel(new FlowLayout(FlowLayout.LEFT, 8, 0));
        folderRow.setOpaque(false);
        folderRow.add(folderLabel);
        JButton chooseFileBtn = new JButton("Choose...");
        chooseFileBtn.addActionListener(e -> {
            JFileChooser chooser = new JFileChooser();
            Path current = provider.getKeysFilePath();
            if (Files.exists(current)) {
                chooser.setSelectedFile(current.toFile());
            } else if (current.getParent() != null && Files.exists(current.getParent())) {
                chooser.setCurrentDirectory(current.getParent().toFile());
            }
            chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
            if (chooser.showOpenDialog(this) == JFileChooser.APPROVE_OPTION) {
                currentApiKeysPath = chooser.getSelectedFile().getAbsolutePath();
                provider.setApiKeysFile(currentApiKeysPath);
                updateFolderLabel();
                loadKeys();
            }
        });
        folderRow.add(chooseFileBtn);
        JButton openFolderBtn = new JButton(new ExternalIcon(16));
        openFolderBtn.setToolTipText("Open API Keys File in Desktop");
        openFolderBtn.addActionListener(e -> {
            try {
                provider.ensureKeysFileExists();
                Desktop.getDesktop().open(provider.getKeysFilePath().toFile());
            } catch (Exception ex) {
                log.error("Failed to open keys file", ex);
                JOptionPane.showMessageDialog(this, "Could not open file: " + ex.getMessage());
            }
        });
        folderRow.add(openFolderBtn);
        formPanel.add(folderRow, "span 2, wrap");

        formPanel.add(new JLabel("Tokenizer Type:"), "gaptop 5");
        tokenizerCombo = new JComboBox<>(TokenizerType.values());
        tokenizerCombo.setSelectedItem(provider.getTokenizerType());
        formPanel.add(tokenizerCombo, "wmax 300, span 2, wrap");

        formPanel.add(new JLabel("Auto-Register Discovered Models:"), "gaptop 5");
        autoRegisterCheck = new JCheckBox("", provider.isAutomaticallyRegisterNewlyDiscoveredModels());
        autoRegisterCheck.setOpaque(false);
        autoRegisterCheck.setToolTipText("Automatically register and persist newly discovered models when API discovery runs.");
        formPanel.add(autoRegisterCheck, "span 2, wrap");

        if (provider instanceof GeminiAiProvider gemini && vertexCheck != null) {
            formPanel.add(new JLabel("Use Vertex AI:"), "gaptop 5");
            vertexCheck = new JCheckBox("", gemini.isVertex());
            vertexCheck.setOpaque(false);
            vertexCheck.setToolTipText("Use Google Cloud Vertex AI endpoint instead of the standard Google AI Studio.");
            formPanel.add(vertexCheck, "span 2, wrap");
        }

        if (provider instanceof AnthropicProvider anthropic) {
            formPanel.add(new JLabel("Anthropic Version:"));
            anthropicVersionField = new JTextField(anthropic.getAnthropicVersion());
            formPanel.add(anthropicVersionField, "span 2, wrap");
        }

        if (provider instanceof OpenAiResponsesProvider nativeOai) {
            formPanel.add(new JLabel("Verified Organization:"), "gaptop 5");
            JCheckBox verifiedCheck = new JCheckBox("", nativeOai.isVerifiedOrganization());
            verifiedCheck.setOpaque(false);
            verifiedCheck.setToolTipText("Enable if your API key belongs to a verified OpenAI organization. Allows stateful API calls and plain-text reasoning summaries.");
            verifiedCheck.addActionListener(e -> {
                nativeOai.setVerifiedOrganization(verifiedCheck.isSelected());
            });
            formPanel.add(verifiedCheck, "span 2, wrap");
        }

        if (provider instanceof OpenAiChatCompletionsProvider oai) {
            formPanel.add(new JLabel("Custom Headers:"), "top, gaptop 5");
            customHeadersArea = new JTextArea(3, 20);
            customHeadersArea.setFont(new Font(Font.MONOSPACED, Font.PLAIN, 12));
            customHeadersArea.addMouseWheelListener(e -> SwingUtils.redispatchMouseWheelEvent(customHeadersArea, e));
            if (oai.getCustomHeaders() != null) {
                String headers = oai.getCustomHeaders().entrySet().stream()
                        .map(entry -> entry.getKey() + ": " + entry.getValue())
                        .collect(Collectors.joining("\n"));
                customHeadersArea.setText(headers);
            }
            PromptSupport.setPrompt("Header-Name: Header-Value\nOne per line...", customHeadersArea);
            JScrollPane headersScroll = new JScrollPane(customHeadersArea);
            formPanel.add(headersScroll, "span 2, growx, wrap");

            formPanel.add(new JLabel("Prefer HTTP/1.1:"), "gaptop 5");
            preferHttp11Check = new JCheckBox("", oai.isPreferHttp11());
            preferHttp11Check.setOpaque(false);
            preferHttp11Check.setToolTipText("Force HTTP/1.1 to avoid protocol hangs on some local servers/routers.");
            formPanel.add(preferHttp11Check, "span 2, wrap");
        }

        // Initial state sync
        textArea.setEnabled(provider.isApiKeyRequired());
        loadKeys();

        // Tabbed Interface: Tab 1 = Details, Tab 2 = Models
        JTabbedPane subTabs = new JTabbedPane();
        JScrollPane detailsScrollPane = new JScrollPane(formPanel);
        detailsScrollPane.setBorder(null);
        detailsScrollPane.getVerticalScrollBar().setUnitIncrement(20);
        subTabs.addTab("Details", detailsScrollPane);

        registryViewer = new AiModelsPanel(provider.getAllDisplayModels(), container, null);
        registryViewer.setTargetProvider(provider);
        subTabs.addTab("Models", registryViewer);

        add(subTabs, BorderLayout.CENTER);
    }

    /**
     * Performs a non-blocking model discovery probe. Automatically synchronizes
     * UI state to the object and key file before testing.
     */
    private void testConnection() {
        try {
            syncToProvider();
            new DiscoverModelsTask(this, provider, true, newModels -> {
                if (registryViewer != null) {
                    registryViewer.setTargetProvider(provider);
                }
            }).start();
        } catch (IOException ex) {
            log.error("Failed to sync before test", ex);
            JOptionPane.showMessageDialog(this, "Pre-test sync failed: " + ex.getMessage());
        }
    }

    /**
     * Creates the promotional banner label if a banner asset is bundled for
     * this provider.
     *
     * @return The clickable banner label, or null if no banner exists.
     */
    private JLabel createPromoBannerLabel() {
        if (provider.getKeysAcquisitionUri() == null) {
            return null;
        }
        URL bannerResource = getClass().getResource("/banners/aiproviders/" + provider.getClass().getName() + ".png");
        if (bannerResource == null) {
            return null;
        }
        ImageIcon orig = new ImageIcon(bannerResource);
        if (orig.getIconWidth() <= 0) {
            return null;
        }
        int targetWidth = 560;
        int targetHeight = (int) (orig.getIconHeight() * ((double) targetWidth / orig.getIconWidth()));
        Image scaled = orig.getImage().getScaledInstance(targetWidth, targetHeight, Image.SCALE_SMOOTH);
        JLabel bannerLabel = new JLabel(new ImageIcon(scaled));
        bannerLabel.setCursor(new Cursor(Cursor.HAND_CURSOR));
        bannerLabel.setToolTipText("Claim promotion & register at " + provider.getDisplayName());
        bannerLabel.setBorder(BorderFactory.createEmptyBorder(0, 0, 4, 0));
        bannerLabel.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                try {
                    Desktop.getDesktop().browse(provider.getKeysAcquisitionUri());
                } catch (Exception ex) {
                    log.error("Failed to open acquisition URI", ex);
                }
            }
        });
        return bannerLabel;
    }

    /**
     * Updates the hyperlinked label to browse to the key acquisition URL.
     */
    private void updateLinkLabel() {
        if (provider.getKeysAcquisitionUri() == null) {
            acquisitionLinkLabel.setVisible(false);
            return;
        }
        acquisitionLinkLabel.setVisible(true);
        String name = displayNameField != null ? displayNameField.getText().trim() : provider.getDisplayName();
        if (name.isBlank()) {
            name = "Provider";
        }

        acquisitionLinkLabel.setIcon(null);
        acquisitionLinkLabel.setText("<html><a href=''>" + name + " - Get API Keys</a></html>");
        acquisitionLinkLabel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
        acquisitionLinkLabel.setToolTipText("Get API keys for " + name);
        acquisitionLinkLabel.setCursor(new Cursor(Cursor.HAND_CURSOR));
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
     * Updates the API keys file path label.
     */
    private void updateFolderLabel() {
        Path path = provider.getKeysFilePath();
        folderLabel.setText(path.toString());
        folderLabel.setToolTipText(path.toString());
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
     * pool to disk.
     *
     * @throws java_io_IOException If writing the keys file or syncing the
     * provider state fails.
     */
    public void syncToProvider() throws IOException {
        provider.setDisplayName(displayNameField.getText().trim());
        provider.setDescription(descriptionField.getText().trim());
        provider.setEnabled(enabledCheck.isSelected());
        provider.setApiKeyRequired(apiKeyRequiredCheck.isSelected());

        provider.setApiKeysFile(currentApiKeysPath);
        updateFolderLabel();
        provider.setTokenizerType((TokenizerType) tokenizerCombo.getSelectedItem());
        if (autoRegisterCheck != null) {
            provider.setAutomaticallyRegisterNewlyDiscoveredModels(autoRegisterCheck.isSelected());
        }

        if (baseUrlField != null) {
            provider.setBaseUrl(baseUrlField.getText().trim());
        }

        if (provider instanceof GeminiAiProvider gemini && vertexCheck != null) {
            gemini.setVertex(vertexCheck.isSelected());
        }

        if (provider instanceof AnthropicProvider anthropic && anthropicVersionField != null) {
            anthropic.setAnthropicVersion(anthropicVersionField.getText().trim());
        }

        if (provider instanceof OpenAiChatCompletionsProvider oai) {
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
