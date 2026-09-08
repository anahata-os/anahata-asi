/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.provider;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.event.FocusAdapter;
import java.awt.event.FocusEvent;
import java.time.Duration;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import javax.swing.BorderFactory;
import javax.swing.DefaultComboBoxModel;
import javax.swing.DefaultListCellRenderer;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.UIManager;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.DefaultTableModel;
import lombok.NonNull;
import lombok.extern.slf4j.Slf4j;
import net.miginfocom.swing.MigLayout;
import uno.anahata.asi.internal.TimeUtils;
import org.apache.commons.io.FileUtils;
import org.jdesktop.swingx.prompt.PromptSupport;
import uno.anahata.asi.ollama.OllamaAiProvider;
import uno.anahata.asi.ollama.OllamaRemoteModel;
import uno.anahata.asi.ollama.OllamaRunningModel;
import uno.anahata.asi.swing.AbstractSwingAsiContainer;
import uno.anahata.asi.swing.icons.AddIcon;
import uno.anahata.asi.swing.icons.DeleteIcon;
import uno.anahata.asi.swing.icons.PulseIcon;
import uno.anahata.asi.swing.icons.RestartIcon;
import uno.anahata.asi.swing.internal.SwingTask;
import uno.anahata.asi.swing.internal.SwingUtils;

/**
 * Specialized Swing configuration panel for Ollama AI providers.
 * <p>
 * In addition to standard OpenAI-compatible base URL and custom headers, this panel
 * exposes Ollama-native diagnostics including server version detection, an active
 * GPU VRAM and loaded model monitor ({@code /api/ps}), and on-demand VRAM unloading
 * ({@code keep_alive: 0}).
 * </p>
 * 
 * @author anahata
 */
@Slf4j
public class OllamaAiProviderPanel extends OpenAiChatCompletionsProviderPanel<OllamaAiProvider> {

    /**
     * Label displaying the detected Ollama server version.
     */
    private JLabel versionLabel;

    /**
     * Label displaying active models count in memory or idle notice.
     */
    private JLabel vramStatusLabel;

    /**
     * Table displaying models currently loaded in host memory and GPU VRAM.
     */
    private JTable runningModelsTable;

    /**
     * Data model backing the running models table.
     */
    private DefaultTableModel runningModelsTableModel;

    /**
     * Button to refresh the active VRAM / running models list.
     */
    private JButton refreshRunningBtn;

    /**
     * Button to unload the selected model from GPU memory.
     */
    private JButton unloadModelBtn;

    /**
     * Button to download and install a new model from ollama.com or custom tag.
     */
    private JButton pullModelBtn;

    /**
     * Visual container for in-flight model download progress.
     */
    private JPanel pullProgressPanel;

    /**
     * Label showing the status text of the active model pull.
     */
    private JLabel pullStatusLabel;

    /**
     * Progress bar displaying download percentage and bytes.
     */
    private JProgressBar pullProgressBar;

    /**
     * Constructs a new uninitialized OllamaAiProviderPanel.
     */
    public OllamaAiProviderPanel() {
        super();
    }

    /**
     * {@inheritDoc}
     * <p>
     * Enhances the panel with Ollama server version diagnostics and a live VRAM process monitor.
     * </p>
     */
    @Override
    public void init(@NonNull AbstractSwingAsiContainer container, @NonNull OllamaAiProvider provider, Runnable removeCallback) {
        super.init(container, provider, removeCallback);

        formPanel.add(new JLabel("Server Diagnostics:"), "top, gaptop 8");

        JPanel diagPanel = new JPanel(new MigLayout("insets 8, fillx", "[grow,fill]", "[]6[]"));
        diagPanel.setOpaque(false);
        diagPanel.setBorder(BorderFactory.createTitledBorder(
                BorderFactory.createLineBorder(new Color(200, 200, 200)),
                "Ollama Server & VRAM Monitor", 0, 0,
                getFont().deriveFont(Font.BOLD, 12f), new Color(100, 100, 100)));

        JPanel topRow = new JPanel(new FlowLayout(FlowLayout.LEFT, 8, 0));
        topRow.setOpaque(false);
        versionLabel = new JLabel("Version: Not checked");
        versionLabel.setFont(versionLabel.getFont().deriveFont(Font.BOLD));
        topRow.add(versionLabel);

        JButton checkVersionBtn = new JButton("Check Version", new PulseIcon(14));
        checkVersionBtn.addActionListener(e -> checkServerVersion());
        topRow.add(checkVersionBtn);

        refreshRunningBtn = new JButton("Refresh VRAM Monitor", new RestartIcon(14));
        refreshRunningBtn.addActionListener(e -> refreshRunningModels());
        topRow.add(refreshRunningBtn);

        unloadModelBtn = new JButton("Unload from Memory", new DeleteIcon(14));
        unloadModelBtn.setToolTipText("Immediately unloads the selected model from GPU VRAM");
        unloadModelBtn.setEnabled(false);
        unloadModelBtn.addActionListener(e -> unloadSelectedModel());
        topRow.add(unloadModelBtn);

        pullModelBtn = new JButton("Pull Model from ollama.com", new AddIcon(14));
        pullModelBtn.setToolTipText("Download and install a model from ollama.com or custom tag");
        pullModelBtn.addActionListener(e -> showPullModelDialog());
        topRow.add(pullModelBtn);

        vramStatusLabel = new JLabel("0 models in VRAM (Idle)");
        vramStatusLabel.setFont(vramStatusLabel.getFont().deriveFont(Font.ITALIC, 11f));
        vramStatusLabel.setForeground(UIManager.getColor("Label.disabledForeground"));
        topRow.add(vramStatusLabel);

        diagPanel.add(topRow, "wrap");

        pullProgressPanel = new JPanel(new MigLayout("insets 4, fillx", "[grow,fill]", "[]2[]"));
        pullProgressPanel.setOpaque(false);
        pullProgressPanel.setBorder(BorderFactory.createMatteBorder(0, 0, 1, 0, new Color(220, 220, 220)));
        pullStatusLabel = new JLabel("Preparing download...");
        pullStatusLabel.setFont(pullStatusLabel.getFont().deriveFont(Font.BOLD, 11f));
        pullProgressBar = new JProgressBar(0, 100);
        pullProgressBar.setStringPainted(true);
        pullProgressPanel.add(pullStatusLabel, "wrap");
        pullProgressPanel.add(pullProgressBar, "growx");
        pullProgressPanel.setVisible(provider.hasActivePulls());
        diagPanel.add(pullProgressPanel, "growx, wrap");

        baseUrlField.addFocusListener(new FocusAdapter() {
            @Override
            public void focusLost(FocusEvent e) {
                checkServerVersion();
                refreshRunningModels();
            }
        });

        enabledCheck.addActionListener(e -> {
            if (enabledCheck.isSelected()) {
                checkServerVersion();
                refreshRunningModels();
            }
        });

        // Running models table
        String[] columns = {"Model Name", "Total RAM/VRAM", "GPU VRAM", "Context", "Expires In", "Parameters", "Quantization"};
        runningModelsTableModel = new DefaultTableModel(columns, 0) {
            @Override
            public boolean isCellEditable(int row, int column) {
                return false;
            }
        };

        runningModelsTable = new JTable(runningModelsTableModel);
        runningModelsTable.setRowHeight(24);
        runningModelsTable.getSelectionModel().addListSelectionListener(e -> {
            unloadModelBtn.setEnabled(runningModelsTable.getSelectedRow() >= 0);
        });

        // Center-align columns
        DefaultTableCellRenderer centerRenderer = new DefaultTableCellRenderer();
        centerRenderer.setHorizontalAlignment(JLabel.CENTER);
        for (int i = 1; i < columns.length; i++) {
            runningModelsTable.getColumnModel().getColumn(i).setCellRenderer(centerRenderer);
        }

        JScrollPane tableScroll = new JScrollPane(runningModelsTable);
        tableScroll.setPreferredSize(new Dimension(500, 120));
        diagPanel.add(tableScroll, "growx, wrap");

        formPanel.add(diagPanel, "span 2, growx, wrap, gapbottom 10");
    }

    /**
     * Checks the running Ollama server version via {@link OllamaAiProvider#getServerVersion()} in a background task.
     */
    private void checkServerVersion() {
        try {
            syncToProvider();
        } catch (Exception e) {
            log.error("Failed to sync settings before checking version", e);
            JOptionPane.showMessageDialog(this, "Could not synchronize settings: " + e.getMessage(), "Sync Error", JOptionPane.ERROR_MESSAGE);
            return;
        }
        new SwingTask<String>(this, container, "Checking Ollama Version", () -> {
            return provider.getServerVersion();
        }, version -> {
            if (version != null && !version.isBlank()) {
                versionLabel.setText("Ollama v" + version);
                versionLabel.setForeground(new Color(0, 128, 0));
            } else {
                versionLabel.setText("Version: Unknown (Server unreachable at " + provider.getServerUrl() + ")");
                versionLabel.setForeground(Color.RED);
            }
        }, error -> {
            versionLabel.setText("Version check failed: " + error.getMessage());
            versionLabel.setForeground(Color.RED);
        }).start();
    }

    /**
     * Refreshes the running models loaded in memory via {@link OllamaAiProvider#getRunningModels()} in a background task.
     */
    private void refreshRunningModels() {
        try {
            syncToProvider();
        } catch (Exception e) {
            log.error("Failed to sync settings before refreshing running models", e);
            JOptionPane.showMessageDialog(this, "Could not synchronize settings: " + e.getMessage(), "Sync Error", JOptionPane.ERROR_MESSAGE);
            return;
        }
        refreshRunningBtn.setEnabled(false);
        new SwingTask<List<OllamaRunningModel>>(this, container, "Querying Ollama Running Models", () -> {
            return provider.getRunningModels();
        }, models -> {
            refreshRunningBtn.setEnabled(true);
            runningModelsTableModel.setRowCount(0);
            if (models == null || models.isEmpty()) {
                unloadModelBtn.setEnabled(false);
                if (vramStatusLabel != null) {
                    vramStatusLabel.setText("0 models in VRAM (Server is idle; models load on prompt)");
                    vramStatusLabel.setForeground(UIManager.getColor("Label.disabledForeground"));
                }
                return;
            }
            if (vramStatusLabel != null) {
                vramStatusLabel.setText(models.size() + " model(s) loaded in VRAM");
                vramStatusLabel.setForeground(new Color(0, 128, 0));
            }
            for (OllamaRunningModel m : models) {
                String totalMem = formatBytes(m.size());
                String vramMem = m.sizeVram() > 0 ? formatBytes(m.sizeVram()) : "0 B (CPU)";
                String ctx = m.contextLength() != null ? String.format("%,d", m.contextLength()) : "Default";
                String expiry = formatExpiry(m.expiresAt());

                runningModelsTableModel.addRow(new Object[]{
                    m.name(),
                    totalMem,
                    vramMem,
                    ctx,
                    expiry,
                    m.parameterSize(),
                    m.quantizationLevel()
                });
            }
        }, error -> {
            refreshRunningBtn.setEnabled(true);
            log.error("Failed to query running models", error);
            JOptionPane.showMessageDialog(this, "Could not fetch running models: " + error.getMessage(), "Ollama Error", JOptionPane.ERROR_MESSAGE);
        }).start();
    }

    /**
     * Unloads the selected model from memory by sending {@code keep_alive: 0}.
     */
    private void unloadSelectedModel() {
        int row = runningModelsTable.getSelectedRow();
        if (row < 0) {
            return;
        }
        String modelName = (String) runningModelsTableModel.getValueAt(row, 0);
        int confirm = JOptionPane.showConfirmDialog(this,
                "Are you sure you want to unload '" + modelName + "' from GPU/system memory?",
                "Unload Model", JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE);

        if (confirm == JOptionPane.YES_OPTION) {
            new SwingTask<Void>(this, container, "Unloading Model " + modelName, () -> {
                provider.unloadModel(modelName);
                return null;
            }, done -> {
                refreshRunningModels();
                JOptionPane.showMessageDialog(this, "Model '" + modelName + "' unloaded from memory.", "Unloaded", JOptionPane.INFORMATION_MESSAGE);
            }, error -> {
                log.error("Failed to unload model", error);
                JOptionPane.showMessageDialog(this, "Failed to unload model: " + error.getMessage(), "Error", JOptionPane.ERROR_MESSAGE);
            }).start();
        }
    }

    /**
     * Queries ollama.com tags and displays the model pulling dialog with autocompletion.
     */
    private void showPullModelDialog() {
        pullModelBtn.setEnabled(false);
        new SwingTask<List<OllamaRemoteModel>>(this, container, "Fetching Available Models", () -> {
            return OllamaAiProvider.fetchRemoteModels();
        }, remoteModels -> {
            pullModelBtn.setEnabled(true);
            openPullDialog(remoteModels);
        }, error -> {
            pullModelBtn.setEnabled(true);
            log.warn("Could not fetch remote models from ollama.com: {}", error.getMessage());
            openPullDialog(Collections.emptyList());
        }).start();
    }

    /**
     * Constructs and opens the model pull dialog with an alphabetically sorted list
     * showing model sizes, release dates, and installed status from the live server.
     *
     * @param remoteModels the list of remote models fetched from ollama.com
     */
    private void openPullDialog(List<OllamaRemoteModel> remoteModels) {
        List<OllamaRemoteModel> sortedModels = new ArrayList<>(remoteModels);
        sortedModels.sort((a, b) -> a.name().compareToIgnoreCase(b.name()));

        DefaultComboBoxModel<OllamaRemoteModel> comboModel = new DefaultComboBoxModel<>();
        for (OllamaRemoteModel rm : sortedModels) {
            comboModel.addElement(rm);
        }
        JComboBox<OllamaRemoteModel> modelCombo = new JComboBox<>(comboModel);
        modelCombo.setRenderer(new DefaultListCellRenderer() {
            @Override
            public Component getListCellRendererComponent(JList<?> list, Object value, int index, boolean isSelected, boolean cellHasFocus) {
                super.getListCellRendererComponent(list, value, index, isSelected, cellHasFocus);
                if (value instanceof OllamaRemoteModel rm) {
                    String sizeStr = rm.size() > 0 ? FileUtils.byteCountToDisplaySize(rm.size()) : "";
                    String dateStr = rm.modifiedAt() != null ? TimeUtils.formatSmartTimestamp(rm.modifiedAt()) : "";
                    boolean installed = isModelInstalled(rm.name());
                    
                    StringBuilder sb = new StringBuilder("<html><b>").append(rm.name()).append("</b>");
                    if (!sizeStr.isBlank()) {
                        sb.append("&nbsp;&nbsp;<font color='#888888'>").append(sizeStr).append("</font>");
                    }
                    if (!dateStr.isBlank()) {
                        sb.append("&nbsp;&nbsp;<font color='#777777'>• ").append(dateStr).append("</font>");
                    }
                    if (installed) {
                        sb.append("&nbsp;&nbsp;<font color='#008800'><b>[Installed]</b></font>");
                    }
                    sb.append("</html>");
                    setText(sb.toString());
                } else if (value != null) {
                    setText(value.toString());
                }
                return this;
            }
        });

        JPanel dialogPanel = new JPanel(new MigLayout("fillx, insets 10", "[grow,fill]", "[]8[]"));
        dialogPanel.add(new JLabel("<html><b>Select Model to Download from ollama.com:</b></html>"), "wrap");
        dialogPanel.add(modelCombo, "growx, wrap");

        int option = JOptionPane.showConfirmDialog(this, dialogPanel, "Pull Model from ollama.com", JOptionPane.OK_CANCEL_OPTION, JOptionPane.PLAIN_MESSAGE);
        if (option == JOptionPane.OK_OPTION) {
            OllamaRemoteModel selected = (OllamaRemoteModel) modelCombo.getSelectedItem();
            if (selected == null) {
                return;
            }
            String modelTag = selected.name();

            if (isModelInstalled(modelTag)) {
                JOptionPane.showMessageDialog(this, "Model '" + modelTag + "' is already installed on this Ollama server.", "Already Installed", JOptionPane.WARNING_MESSAGE);
                return;
            }

            executePull(modelTag);
        }
    }

    /**
     * Checks if a model tag is already installed on the Ollama server by comparing
     * against live discovered models from {@code /api/tags} and registered local models.
     * 
     * @param modelTag the model tag to check
     * @return true if installed on the server
     */
    private boolean isModelInstalled(String modelTag) {
        if (modelTag == null || modelTag.isBlank()) {
            return false;
        }
        String clean = modelTag.trim().toLowerCase();
        String withLatest = clean.contains(":") ? clean : clean + ":latest";
        String withoutLatest = clean.endsWith(":latest") ? clean.substring(0, clean.length() - 7) : clean;

        // 1. Authoritative: Check live models reported by Ollama /api/tags
        boolean inApiTags = provider.getCachedApiModels().stream()
                .anyMatch(m -> {
                    String id = m.getModelId().toLowerCase();
                    return id.equals(clean) || id.equals(withLatest) || id.equals(withoutLatest);
                });
        if (inApiTags) {
            return true;
        }

        // 2. Check locally registered models
        return provider.getModels().stream()
                .anyMatch(m -> {
                    String id = m.getModelId().toLowerCase();
                    return id.equals(clean) || id.equals(withLatest) || id.equals(withoutLatest);
                });
    }

    /**
     * Initiates the asynchronous model pull and binds live progress to {@link #pullProgressBar}.
     *
     * @param modelTag the model name and tag to pull
     */
    private void executePull(String modelTag) {
        pullProgressPanel.setVisible(true);
        pullProgressBar.setValue(0);
        pullProgressBar.setIndeterminate(false);
        pullStatusLabel.setText("Pulling " + modelTag + "...");

        provider.startPull(modelTag, progress -> {
            SwingUtils.runInEDT(() -> {
                pullProgressPanel.setVisible(true);
                if (progress.total() > 0) {
                    pullProgressBar.setIndeterminate(false);
                    pullProgressBar.setValue(progress.getPercent());
                    pullProgressBar.setString(String.format("%s %d%% (%s / %s)",
                            progress.status(), progress.getPercent(),
                            formatBytes(progress.completed()), formatBytes(progress.total())));
                } else {
                    pullProgressBar.setIndeterminate(true);
                    pullProgressBar.setString(progress.status());
                }
                pullStatusLabel.setText("Pulling " + modelTag + ": " + progress.status());
            });
        }, () -> {
            pullProgressPanel.setVisible(false);
            if (registryViewer != null) {
                registryViewer.setTargetProvider(provider);
            }
            JOptionPane.showMessageDialog(this, "Successfully downloaded and installed model '" + modelTag + "'!", "Model Installed", JOptionPane.INFORMATION_MESSAGE);
        }, err -> {
            pullProgressPanel.setVisible(false);
            JOptionPane.showMessageDialog(this, "Failed to pull model '" + modelTag + "': " + err.getMessage(), "Download Failed", JOptionPane.ERROR_MESSAGE);
        });
    }

    /**
     * Formats bytes into human-readable representation using commons-io.
     * 
     * @param bytes the raw byte count
     * @return formatted string
     */
    private static String formatBytes(long bytes) {
        return FileUtils.byteCountToDisplaySize(bytes);
    }

    /**
     * Formats an ISO expiration timestamp into relative remaining time.
     * 
     * @param expiresAt the ISO timestamp
     * @return formatted relative string
     */
    private static String formatExpiry(String expiresAt) {
        if (expiresAt == null || expiresAt.isBlank()) {
            return "Indefinite";
        }
        try {
            Instant expiry = Instant.parse(expiresAt);
            long millis = Duration.between(Instant.now(), expiry).toMillis();
            if (millis <= 0) {
                return "Expiring now";
            }
            return TimeUtils.formatDuration(millis);
        } catch (Exception e) {
            return expiresAt;
        }
    }
}
