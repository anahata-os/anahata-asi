/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing;

import java.awt.Component;
import java.awt.GraphicsEnvironment;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Instant;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicBoolean;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.filechooser.FileNameExtensionFilter;
import uno.anahata.asi.AsiContainerProperties;
import uno.anahata.asi.AsiContainerUpgrade;
import uno.anahata.asi.Version;
import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.AbstractAsiContainer;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.agi.provider.AbstractAiProvider;
import uno.anahata.asi.anthropic.AnthropicProvider;
import uno.anahata.asi.gemini.GeminiAiProvider;
import uno.anahata.asi.gemini.GeminiGoogleCloudExpressAIProvider;
import uno.anahata.asi.huggingface.HuggingFaceProvider;
import uno.anahata.asi.minimax.MinimaxAnthropicProvider;
import uno.anahata.asi.mistral.MistralAiProvider;
import uno.anahata.asi.modal.ModalProvider;
import uno.anahata.asi.novarouteai.NovaRouteAiProvider;
import uno.anahata.asi.nvidia.NvidiaAiProvider;
import uno.anahata.asi.openai.OpenAiResponsesProvider;
import uno.anahata.asi.openai.compatible.OpenAiChatCompletionsProvider;
import uno.anahata.asi.swing.agi.AgiPanel;
import uno.anahata.asi.swing.agi.message.part.tool.param.FullTextFileCreateRenderer;
import uno.anahata.asi.swing.agi.message.part.tool.param.ParameterRendererFactory;
import uno.anahata.asi.swing.agi.message.part.tool.param.PathParameterRenderer;
import uno.anahata.asi.swing.agi.message.part.tool.param.ResourceUUIDParameterRenderer;
import uno.anahata.asi.swing.agi.message.part.tool.param.UriParameterRenderer;
import uno.anahata.asi.swing.components.ExceptionDialog;
import uno.anahata.asi.swing.internal.SwingUtils;
import uno.anahata.asi.swing.provider.AiProviderUiRegistry;
import uno.anahata.asi.swing.provider.AnthropicProviderPanel;
import uno.anahata.asi.swing.provider.DiscoverModelsTask;
import uno.anahata.asi.swing.provider.GeminiAiProviderPanel;
import uno.anahata.asi.swing.provider.OpenAiChatCompletionsProviderPanel;
import uno.anahata.asi.swing.provider.OpenAiResponsesProviderPanel;
import uno.anahata.asi.swing.settings.AsiContainerSettingsFrame;
import uno.anahata.asi.swing.toolkit.radio.RadioRenderer;
import uno.anahata.asi.swing.toolkit.render.ToolkitUiRegistry;
import uno.anahata.asi.toolkit.resources.text.FullTextFileCreate;
import uno.anahata.asi.yam.tools.Radio;

/**
 * A Swing-specific base class for Anahata ASI containers.
 * <p>
 * This class bridges the gap between model-agnostic session logic and the 
 * Swing UI environment. It provides shared utilities for UI-based session 
 * imports and defines the hooks for environment-specific window/tab management.
 * </p>
 * 
 * @author anahata
 */
@Slf4j
@Getter
@Setter
public abstract class AbstractSwingAsiContainer extends AbstractAsiContainer {
    
    static {
        //Legengary Radio toolkit
        ToolkitUiRegistry.getInstance().register(Radio.class, RadioRenderer.class);
        
        //Default parameter renderers
        ParameterRendererFactory.register(FullTextFileCreate.class, FullTextFileCreateRenderer.class);
        ParameterRendererFactory.registerById("uri", UriParameterRenderer.class);
        ParameterRendererFactory.registerById("resource", ResourceUUIDParameterRenderer.class);
        ParameterRendererFactory.registerById("path", PathParameterRenderer.class);

        // Provider UI Panel Registry
        AiProviderUiRegistry.getInstance().register(GeminiAiProvider.class, GeminiAiProviderPanel.class);
        AiProviderUiRegistry.getInstance().register(AnthropicProvider.class, AnthropicProviderPanel.class);
        AiProviderUiRegistry.getInstance().register(OpenAiChatCompletionsProvider.class, OpenAiChatCompletionsProviderPanel.class);
        AiProviderUiRegistry.getInstance().register(OpenAiResponsesProvider.class, OpenAiResponsesProviderPanel.class);
    }
    
    /**
     * List of all known AI Providers.
     */
    public static final List<Class<? extends AbstractAiProvider>> AVAILABLE_PROVIDER_CLASSES = List.of(OpenAiChatCompletionsProvider.class,
        uno.anahata.asi.openai.OpenAiResponsesProvider.class,
        uno.anahata.asi.anthropic.AnthropicProvider.class,
        uno.anahata.asi.minimax.MinimaxAnthropicProvider.class,
        uno.anahata.asi.mistral.MistralAiProvider.class,
        uno.anahata.asi.gemini.GeminiAiProvider.class,
        uno.anahata.asi.gemini.GeminiGoogleCloudExpressAIProvider.class,
        uno.anahata.asi.huggingface.HuggingFaceProvider.class,
        uno.anahata.asi.modal.ModalProvider.class,
        uno.anahata.asi.novarouteai.NovaRouteAiProvider.class,
        uno.anahata.asi.nvidia.NvidiaAiProvider.class
    );


    /**
     * The single-instance Settings Command Center frame for this container.
     */
    private AsiContainerSettingsFrame settingsFrame;

    /**
     * Constructs a new Swing ASI container.
     *
     * @param hostApplicationId The unique ID of the host application.
     */
    public AbstractSwingAsiContainer(String hostApplicationId) throws IOException{
        super(hostApplicationId);

        if (getProvider("GeminiGCExpress") == null) {
            registerProvider(new GeminiGoogleCloudExpressAIProvider());
        }

        if (getProvider("Gemini") == null) {
            GeminiAiProvider g = new GeminiAiProvider("Gemini", "Google AI Studio", false);
            registerProvider(g);
        }

        if (getProvider("GeminiVertex") == null) {
            GeminiAiProvider g = new GeminiAiProvider("GeminiVertex", "Google Cloud (Vertex)", true);
            registerProvider(g);
        }

        if (getProvider("NovaRouteAI") == null) {
            registerProvider(new NovaRouteAiProvider());
        }

        if (getProvider("OpenAI") == null) {
            log.info("Registering OpenAI");
            registerProvider(new OpenAiResponsesProvider());
        }

        if (getProvider("Anthropic") == null) {
            log.info("Registering Anthropic");
            AnthropicProvider anthropic = new AnthropicProvider();
            registerProvider(anthropic);
        }

        if (getProvider("Minimax") == null) {
            log.info("Registering MiniMax (Anthropic)");
            registerProvider(new MinimaxAnthropicProvider());
        }

        if (getProvider("Modal") == null) {
            log.info("Registering Modal");
            registerProvider(new ModalProvider());
        }

        if (getProvider("Mistral") == null) {
            log.info("Registering Mistral AI");
            registerProvider(new MistralAiProvider());
        }

        if (getProvider("HuggingFace") == null) {
            log.info("Registering HF");
            HuggingFaceProvider hf = new HuggingFaceProvider();
            registerProvider(hf);
        }

        if (getProvider("Nvidia") == null) {
            log.info("Registering NVIDIA");
            registerProvider(new NvidiaAiProvider());
        }

        // Background Model Discovery for effectively enabled providers
        for (AbstractAiProvider provider : getEffectivelyEnabledProviders()) {
            new DiscoverModelsTask(provider, false).start();
        }
    }

    /**
     * {@inheritDoc}
     * <p>
     * Intercepts container directory resolution prior to creation to detect predecessor version
     * directories. Prompts the user via Swing UI dialog and automatically migrates persistent
     * settings if requested.
     * </p>
     */
    @Override
    public synchronized Path getDirectory() throws IOException {
        Path base = getWorkDirSubDir(getHostApplicationId());
        String version = getContainerVersion();
        if (version == null || version.isBlank()) {
            return base;
        }
        Path targetDir = base.resolve(version);

        if (!Files.exists(targetDir)) {
            Version currentVer = Version.parse(version).orElse(null);
            if (currentVer != null) {
                Optional<Path> predecessorOpt = AsiContainerUpgrade.findPredecessor(base, currentVer);
                if (predecessorOpt.isPresent()) {
                    Path predecessorDir = predecessorOpt.get();
                    Version prevVer = Version.parse(predecessorDir.getFileName().toString()).orElse(null);
                    String prevVerStr = prevVer != null ? prevVer.getCleanVersion() : predecessorDir.getFileName().toString();

                    boolean userWantsImport = promptUpgrade(prevVerStr, currentVer.getCleanVersion());
                    if (userWantsImport) {
                        ensureDir(targetDir);
                        int count = AsiContainerUpgrade.copySettings(predecessorDir, targetDir);
                        log.info("Successfully imported {} settings from version {} to {}", count, prevVerStr, currentVer);
                        showImportSuccess(count, prevVerStr);
                    }
                }
            }

            ensureDir(targetDir);
            if (!AsiContainerProperties.exists(targetDir)) {
                AsiContainerProperties.save(targetDir, version, getHostApplicationId(), Instant.now());
            }
        }

        return targetDir;
    }

    /**
     * Prompts the user via a native Swing dialog asking whether they would like to import
     * persistent settings from an earlier detected version.
     *
     * @param previousVersion The predecessor version string.
     * @param currentVersion The running container version string.
     * @return {@code true} if the user elected to import, {@code false} to start fresh.
     */
    protected boolean promptUpgrade(String previousVersion, String currentVersion) {
        if (GraphicsEnvironment.isHeadless()) {
            return false;
        }
        AtomicBoolean accepted = new AtomicBoolean(false);
        try {
            SwingUtils.runInEDTAndWait(() -> {
                String title = "Import Settings from Previous Version";
                String message = "Anahata ASI found settings from an earlier version (" + previousVersion + ").\n\n"
                        + "Would you like to import your AI providers, templates, and sessions into version " + currentVersion + "?";
                Object[] options = {"Import", "Start Fresh"};
                int choice = JOptionPane.showOptionDialog(
                        null,
                        message,
                        title,
                        JOptionPane.YES_NO_OPTION,
                        JOptionPane.QUESTION_MESSAGE,
                        null,
                        options,
                        options[0]
                );
                accepted.set(choice == JOptionPane.YES_OPTION);
            });
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            log.warn("Upgrade prompt interrupted: {}", e.getMessage());
        } catch (Exception e) {
            log.error("Failed to display upgrade prompt: {}", e.getMessage(), e);
        }
        return accepted.get();
    }

    /**
     * Displays an informational dialog confirming that settings were successfully
     * imported from an earlier version.
     *
     * @param count The number of entities imported.
     * @param prevVerStr The predecessor version string.
     */
    private void showImportSuccess(int count, String prevVerStr) {
        if (GraphicsEnvironment.isHeadless()) {
            return;
        }
        try {
            SwingUtils.runInEDTAndWait(() -> {
                JOptionPane.showMessageDialog(
                        null,
                        "Successfully imported " + count + " settings from version " + prevVerStr + ".\n\n"
                        + "Your AI providers, templates, and sessions are ready.",
                        "Settings Imported",
                        JOptionPane.INFORMATION_MESSAGE
                );
            });
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            log.warn("Import success dialog interrupted: {}", e.getMessage());
        } catch (Exception e) {
            log.error("Failed to display import success dialog: {}", e.getMessage(), e);
        }
    }
    
    /**
     * Displays the global ASI settings Command Center in maximized mode.
     */
    public void showSettings() {
        showSettings(0);
    }

    /**
     * Displays the global ASI settings Command Center with a specific tab selected.
     * <p>
     * Reuses the existing {@link AsiContainerSettingsFrame} instance if already open,
     * bringing it to front and selecting the requested tab index.
     * </p>
     *
     * @param initialTabIndex The index of the tab to open.
     */
    public synchronized void showSettings(int initialTabIndex) {
        if (settingsFrame == null || !settingsFrame.isDisplayable()) {
            settingsFrame = new AsiContainerSettingsFrame(this, initialTabIndex);
        } else {
            settingsFrame.getSettingsPanel().selectTab(initialTabIndex);
        }
        settingsFrame.setExtendedState(JFrame.MAXIMIZED_BOTH);
        settingsFrame.toFront();
        settingsFrame.requestFocus();
        settingsFrame.setVisible(true);
    }

    /**
     * Retrieves the AgiPanel associated with a specific Agi session.
     * 
     * @param agi The session.
     * @return The AgiPanel instance.
     */
    public AgiPanel getAgiPanel(Agi agi) {
        Object ui = getUI(agi);
        if (ui instanceof AgiPanel panel) {
            return panel;
        }
        return null;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Marshals the logical 'open' intent to the Event Dispatch Thread (EDT) and delegates to the environment-specific {@link #focusUI(Agi)} method. This guarantees safe UI manipulation regardless of the calling thread.
     * </p>
     */
    @Override
    protected void onAgiOpened(Agi agi) {
        SwingUtils.runInEDT(() -> focusUI(agi));
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Marshals the logical 'close' intent to the Event Dispatch Thread (EDT) and delegates to the environment-specific {@link #closeUI(Agi)} method. This guarantees safe UI manipulation regardless of the calling thread.
     * </p>
     */
    @Override
    protected void onAgiClosed(Agi agi) {
        SwingUtils.runInEDT(() -> closeUI(agi));
    }

    /**
     * Environment-specific logic to visually focus or select the UI component 
     * associated with the given session.
     * 
     * @param agi The session to focus.
     */
    protected abstract void focusUI(Agi agi);

    /**
     * Environment-specific logic to visually close or hide the UI component 
     * associated with the given session.
     * 
     * @param agi The session to close.
     */
    protected abstract void closeUI(Agi agi);



    /**
     * Opens a standard Swing {@link JFileChooser} to allow the user to select 
     * a saved session (.kryo) for import.
     * 
     * @param parent The parent component for the dialog.
     */
    public void importSessionWithUI(Component parent) {
        Path savedDir = getSavedSessionsDir();
        JFileChooser chooser = new JFileChooser(savedDir.toFile());
        chooser.setDialogTitle("Import Anahata Session");
        chooser.setFileFilter(new FileNameExtensionFilter("Anahata Sessions (*.kryo)", "kryo"));

        if (chooser.showOpenDialog(parent) == JFileChooser.APPROVE_OPTION) {
            File selectedFile = chooser.getSelectedFile();
            log.info("User selected file for import: {}", selectedFile);
            try {
                Agi imported = importSession(selectedFile.toPath());
                open(imported);
            } catch (IOException ex) {
                log.error("Could not import session with UI for " + selectedFile, ex);
                ExceptionDialog.show(null, "Import AGI", "Import AGI failed", ex);
            }
        }
    }
}
