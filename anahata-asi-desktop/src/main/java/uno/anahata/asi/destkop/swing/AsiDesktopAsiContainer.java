/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.destkop.swing;

import java.util.HashMap;
import java.util.Map;
import lombok.Setter;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.agi.AgiConfig;
import uno.anahata.asi.gemini.GeminiAiProvider;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.provider.AbstractAiProvider;
import uno.anahata.asi.huggingface.HuggingFaceProvider;
import uno.anahata.asi.openai.OpenAiCompatibleProvider;
import uno.anahata.asi.swing.AbstractSwingAsiContainer;
import uno.anahata.asi.swing.agi.AgiPanel;
import uno.anahata.asi.swing.agi.resources.DefaultResourceUI;
import uno.anahata.asi.swing.agi.resources.ResourceUiRegistry;

/**
 * A specialized {@link uno.anahata.asi.AbstractAsiContainer} for the standalone Swing application.
 * It manages the lifecycle of sessions within a standalone UI environment.
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

    /** Cache of UI panels for active sessions. */
    private final Map<String, AgiPanel> agiPanels = new HashMap<>();
    
    /** Reference to the main UI panel for tab management. */
    @Setter
    private AsiDesktopMainPanel mainPanel;
    
    /**
     * Constructs a new StandaloneAsiContainer.
     */
    public AsiDesktopAsiContainer() {
        super("AsiDesktop");
        
        // Ensure Gemini is registered with stable UUID
        AbstractAiProvider gemini = getProviderByClass(GeminiAiProvider.class);
        if (gemini == null) {
            registerProvider(new GeminiAiProvider());
        } else if (!"Gemini".equals(gemini.getUuid())) {
            log.info("Migrating legacy Gemini provider ({}) to stable ID", gemini.getUuid());
            unregisterProvider(gemini.getUuid());
            registerProvider(new GeminiAiProvider());
            /*
            gemini.setUuid("Gemini");
            gemini.setFolderName("Gemini");
            registerProvider(gemini);
            */
        }
        /*
        if (getProvider("Z_1") == null) {
            registerProvider(new OpenAiCompatibleProvider(                    
                    "Z_1", "Z ", " https://api.z.ai/api/paas/v4", "Z"));
        }
        
        if (getProvider("Z_2") == null) {
            registerProvider(new OpenAiCompatibleProvider(                    
                    "Z_2", "Z Coding (OpenAI)", "https://api.z.ai/api/coding/paas/v4", "Z"));
        }
        */
        log.info("Registering Modal");
        if (getProvider("Modal") == null) {
            registerProvider(new uno.anahata.asi.modal.ModalProvider());
        }
        
        log.info("Registering HF");
        if (getProvider("HuggingFace") == null) {
            registerProvider(new HuggingFaceProvider());
        }
        
        log.info("Registering Anahata");
        if (getProvider("Anahata") == null) {
            registerProvider(new OpenAiCompatibleProvider(                    
                    "Anahata", "Anahata (no SSL)", "http://a.anahata.uno:1234/v1", "Anahata", "https://asi.anahata.uno"));
        }
        
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
     * Implementation details: Delegates tab removal to the main UI panel when
     * a session is closed.
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
