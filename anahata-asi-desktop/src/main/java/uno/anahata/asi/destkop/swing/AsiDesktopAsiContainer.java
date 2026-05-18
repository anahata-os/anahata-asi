/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.destkop.swing;

import java.util.concurrent.ConcurrentHashMap;
import java.util.Map;
import lombok.Setter;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.agi.AgiConfig;
import uno.anahata.asi.gemini.GeminiAiProvider;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.anthropic.AnthropicProvider;
import uno.anahata.asi.gemini.vertex.GeminiGoogleCloudExpressAIProvider;
import uno.anahata.asi.huggingface.HuggingFaceProvider;
import uno.anahata.asi.minimax.MinimaxAnthropicProvider;
import uno.anahata.asi.modal.ModalProvider;
import uno.anahata.asi.openai.OpenAiProvider;
import uno.anahata.asi.openai.compatible.OpenAiCompatibleProvider;
import uno.anahata.asi.swing.AbstractSwingAsiContainer;
import uno.anahata.asi.swing.agi.AgiPanel;
import uno.anahata.asi.swing.agi.resources.DefaultResourceUI;
import uno.anahata.asi.swing.agi.resources.ResourceUiRegistry;

/**
 * A specialized {@link uno.anahata.asi.AbstractAsiContainer} for the standalone
 * Swing application. It manages the lifecycle of sessions within a standalone
 * UI environment.
 *
 * @author anahata
 */
@Slf4j
public class AsiDesktopAsiContainer extends AbstractSwingAsiContainer {

    static {
        log.info("Performing global Standalone environment configuration...");
        // Register the universal/standalone resource UI strategy
        ResourceUiRegistry.getInstance().setResourceUI(new DefaultResourceUI());
    }

    /**
     * Cache of UI panels for active sessions.
     */
    private final Map<String, AgiPanel> agiPanels = new ConcurrentHashMap<>();

    /**
     * Reference to the main UI panel for tab management.
     */
    @Setter
    private AsiDesktopMainPanel mainPanel;

    /**
     * Constructs a new StandaloneAsiContainer.
     */
    public AsiDesktopAsiContainer() {
        super("AsiDesktop");
        
        // Ensure Gemini is registered with stable UUID
        //AbstractAiProvider gemini = getProviderByClass(GeminiAiProvider.class);
        if (getProvider("GeminiGCExpress") == null) {
            registerProvider(new GeminiGoogleCloudExpressAIProvider());
        }

        if (getProvider("Gemni") == null) {
            registerProvider(new GeminiAiProvider("Gemini", "Google AI Studio", false));
        }

        if (getProvider("GeminiVertex") == null) {
            registerProvider(new GeminiAiProvider("GeminiVertex", "Google Cloud (Vertex)", true));
        }

        if (getProvider("OpenAI") == null) {
            log.info("Registering OpenAI");
            registerProvider(new OpenAiProvider());
        }
        
        if (getProvider("Anthropic") == null) {
            log.info("Registering OpenAI");
            registerProvider(new AnthropicProvider());
        }

        if (getProvider("Minimax") == null) {
            log.info("Registering MiniMax (Anthropic)");
            registerProvider(new MinimaxAnthropicProvider());
        }
        
        if (getProvider("Modal") == null) {
            log.info("Registering Modal");
            registerProvider(new ModalProvider());
        }

        if (getProvider("HuggingFace") == null) {
            log.info("Registering HF");
            registerProvider(new HuggingFaceProvider());
        }
        
        if (getProvider("Anahata") == null) {
            log.info("Registering Anahata");
            registerProvider(new OpenAiCompatibleProvider(
                    "Anahata", "Anahata (no SSL)", "http://a.anahata.uno:1234/v1", "Anahata", "https://discord.com/invite/gwGWWxPUXE"));
        }
        
                /*
        /*
        if (getProvider("Z_1") == null) {
            registerProvider(new OpenAiCompatibleProvider(                    
                    "Z_1", "Z ", " https://api.z.ai/api/paas/v4", "Z"));
        }
        
        if (getProvider("Z_2") == null) {
            registerProvider(new OpenAiCompatibleProvider(                    
                    "Z_2", "Z Coding (OpenAI)", "https://api.z.ai/api/coding/paas/v4", "Z"));
        }
         
         
        if (getProvider("MinimaxOpenAI") == null) {
            log.info("Registering MiniMaxOpenAI");
            String folder = AbstractAsiContainer.getWorkDirSubDir("Minimax").toString();
            OpenAiCompatibleProvider provider = new OpenAiCompatibleProvider(
                    "MinimaxOpenAI", "Minimax (OpenAI)", "https://api.minimax.io/v1", folder, "https://platform.minimax.io/user-center/basic-information/interface-key");
            registerProvider(provider);
        }*/
        
        


    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Delegates focus to the main UI panel to ensure
     * the corresponding tab is selected.
     * </p>
     */
    @Override
    protected void focusUI(Agi agi) {
        if (mainPanel != null) {
            mainPanel.ensureTabAndSelect(agi);
        }
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Delegates tab removal to the main UI panel when a
     * session is closed.
     * </p>
     */
    @Override
    protected void closeUI(Agi agi) {
        if (mainPanel != null) {
            mainPanel.removeTab(agi);
        }
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Lazily creates and caches the {@link AgiPanel}
     * for the requested session.
     * </p>
     */
    @Override
    public AgiPanel getUI(Agi agi) {
        return agiPanels.computeIfAbsent(agi.getConfig().getSessionId(), id -> {
            AgiPanel panel = new AgiPanel(agi);
            panel.setName(id);
            panel.initComponents();
            return panel;
        });
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Instantiates a desktop-specific configuration
     * pre-registered with standard providers.
     * </p>
     */
    @Override
    public AgiConfig createNewAgiConfig() {
        return new AsiDesktopAgiConfig(this);
    }
}
