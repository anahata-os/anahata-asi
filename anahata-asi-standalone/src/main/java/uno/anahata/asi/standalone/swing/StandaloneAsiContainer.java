/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.standalone.swing;

import java.util.HashMap;
import java.util.Map;
import lombok.Setter;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.agi.AgiConfig;
import uno.anahata.asi.cli.CommandLineArgs;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.swing.AbstractSwingAsiContainer;
import uno.anahata.asi.swing.agi.AgiPanel;
import uno.anahata.asi.swing.agi.resources.DefaultResourceUI;
import uno.anahata.asi.swing.agi.resources.ResourceUiRegistry;

/**
 * A specialized {@link uno.anahata.asi.AbstractAsiContainer} for the standalone Swing application.
 * It handles the storage and parsing of command-line arguments to configure
 * initial agi sessions.
 * 
 * @author anahata
 */
@Slf4j
public class StandaloneAsiContainer extends AbstractSwingAsiContainer {
    
    static {
        log.info("Performing global Standalone environment configuration...");
        // Register the universal/standalone resource UI strategy
        ResourceUiRegistry.getInstance().setResourceUI(new DefaultResourceUI());
    }

    /** The raw command-line arguments passed to the application. */
    private final String[] cmdLineArgs;
    
    /** Cache of UI panels for active sessions. */
    private final Map<String, AgiPanel> agiPanels = new HashMap<>();
    
    /** Reference to the main UI panel for tab management. */
    @Setter
    private StandaloneMainPanel mainPanel;
    
    /**
     * Constructs a new StandaloneAsiContainer.
     * 
     * @param cmdLineArgs The command-line arguments from the main entry point.
     */
    public StandaloneAsiContainer(String[] cmdLineArgs) {
        super("swing-standalone");
        this.cmdLineArgs = cmdLineArgs;
    }

    /**
     * {@inheritDoc}
     * <p>
     * In the standalone container, this hook is used to parse command-line 
     * arguments and apply them to the newly created agi session.
     * </p>
     * 
     * @param agi The newly created agi session.
     */
    @Override
    protected void configureNewAgi(Agi agi) {
        log.info("Parsing command-line arguments for new standalone agi.");
        CommandLineArgs.parse(agi, cmdLineArgs);
    }

    @Override
    protected void focusUI(Agi agi) {
        if (mainPanel != null) {
            mainPanel.ensureTabAndSelect(agi);
        }
    }

    @Override
    protected void closeUI(Agi agi) {
        if (mainPanel != null) {
            mainPanel.removeTab(agi);
        }
    }

    @Override
    public AgiPanel getUI(Agi agi) {
        return agiPanels.computeIfAbsent(agi.getConfig().getSessionId(), id -> {
            AgiPanel panel = new AgiPanel(agi);
            panel.setName(id);
            panel.initComponents();
            return panel;
        });
    }

    /** {@inheritDoc} */
    @Override
    public AgiConfig createNewAgiConfig() {
        return new StandaloneAgiConfig(this);
    }
}
