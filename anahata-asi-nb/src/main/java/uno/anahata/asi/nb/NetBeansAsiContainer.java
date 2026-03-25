/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb;

import java.beans.PropertyChangeListener;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.AsiContainer;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.agi.AgiConfig;
import uno.anahata.asi.nb.tools.files.nb.AnahataAnnotationProvider;
import uno.anahata.asi.nb.ui.render.FullTextResourceUpdateRenderer;
import uno.anahata.asi.nb.ui.render.TextResourceReplacementsRenderer;
import uno.anahata.asi.nb.ui.render.TextResourceLineUpdatesRenderer;
import uno.anahata.asi.nb.ui.render.TextResourceLineEditsRenderer;
import uno.anahata.asi.toolkit.files.lines.TextResourceLineEdits;
import uno.anahata.asi.nb.ui.resources.NbResourceUI;
import uno.anahata.asi.nb.util.ElementHandleModule;
import uno.anahata.asi.swing.agi.message.part.tool.param.ParameterRendererFactory;
import uno.anahata.asi.swing.agi.resources.ResourceUiRegistry;
import uno.anahata.asi.agi.tool.schema.SchemaProvider;
import uno.anahata.asi.toolkit.files.FullTextResourceUpdate;
import uno.anahata.asi.toolkit.files.TextResourceReplacements;
import uno.anahata.asi.toolkit.files.TextResourceLineBasedUpdates;

/**
 * NetBeans-specific configuration for the Anahata ASI.
 * <p>
 * Handles IDE-specific initialization and session management. Global environment 
 * configuration (parameter renderers, JSON modules) is performed once during 
 * static initialization.
 * </p>
 * 
 * @author anahata
 */
@Slf4j
public class NetBeansAsiContainer extends AsiContainer {

    static {
        log.info("Performing global NetBeans environment configuration...");
        
        // 1. Register specialized parameter renderers for file operations
        ParameterRendererFactory.register(FullTextResourceUpdate.class, FullTextResourceUpdateRenderer.class);        
        ParameterRendererFactory.register(TextResourceReplacements.class, TextResourceReplacementsRenderer.class);
        ParameterRendererFactory.register(TextResourceLineBasedUpdates.class, TextResourceLineUpdatesRenderer.class);
        ParameterRendererFactory.register(TextResourceLineEdits.class, TextResourceLineEditsRenderer.class);
        
        // 2. Register the ElementHandle module for global JSON support in the IDE
        SchemaProvider.OBJECT_MAPPER.registerModule(new ElementHandleModule());
        
        // 3. Register the NetBeans-native resource UI strategy
        ResourceUiRegistry.getInstance().setResourceUI(new NbResourceUI());
    }

    /** 
     * Map to track the resource listeners for each session to ensure cleanup on disposal.
     */
    private final Map<String, PropertyChangeListener> sessionListeners = new ConcurrentHashMap<>();

    /**
     * Default constructor for the NetBeans container.
     */
    public NetBeansAsiContainer() {
        super("netbeans");
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Creates a NetBeans-aware AGI configuration blueprint.
     * </p>
     */
    @Override
    protected AgiConfig createNewAgiConfig() {
        return new NetBeansAgiConfig(this);
    }

    /**
     * {@inheritDoc}
     * <p>
     * Initializes the session environment with NetBeans defaults.
     * </p>
     */
    @Override
    public void onAgiCreated(Agi agi) {
        log.info("Initializing NetBeans defaults for new agi session: {}", agi.getShortId());

        // Default model configuration for NetBeans
        if (agi.getSelectedModel() == null) {
            agi.setProviderAndModel("Gemini", "models/gemini-3-flash-preview");
        }
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Establishes a reactive bridge between the core 
     * Resource Manager and the NetBeans Annotation system.
     * </p>
     */
    @Override
    public void onAgiRegistered(Agi agi) {
        log.info("Attaching reactive annotation pulse for agi session: {}", agi.getShortId());

        // REACTIVE BRIDGE: Listen for resource changes and nickname updates to trigger IDE refreshes
        PropertyChangeListener listener = evt -> {
            log.info("Reactive pulse trigger ('{}') in session '{}'. Firing IDE annotation refresh.", evt.getPropertyName(), agi.getDisplayName());
            AnahataAnnotationProvider.fireRefresh(null, null);
        };

        agi.getResourceManager().addPropertyChangeListener("resources", listener);
        agi.addPropertyChangeListener("nickname", listener);
        sessionListeners.put(agi.getConfig().getSessionId(), listener);
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Detaches all reactive pulse listeners during session disposal.
     * </p>
     */
    @Override
    public void onAgiUnregistered(Agi agi) {
        PropertyChangeListener listener = sessionListeners.remove(agi.getConfig().getSessionId());
        if (listener != null) {
            log.info("Cleaning up annotation pulse for agi session: {}", agi.getShortId());
            agi.getResourceManager().removePropertyChangeListener("resources", listener);
            agi.removePropertyChangeListener("nickname", listener);
        }
    }

    /**
     * Finds an existing active agi by its session ID, or creates a new one
     * if the ID is null or not found.
     * 
     * @param sessionId The session ID to find.
     * @return The found or newly created agi session.
     */
    public Agi findOrCreateAgi(String sessionId) {
        if (sessionId != null) {
            for (Agi agi : getActiveAgis()) {
                if (agi.getConfig().getSessionId().equals(sessionId)) {
                    return agi;
                }
            }
        }
        return createNewAgi();
    }
}
