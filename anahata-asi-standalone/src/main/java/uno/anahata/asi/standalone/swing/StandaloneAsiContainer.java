/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.standalone.swing;

import java.io.File;
import uno.anahata.asi.AsiContainer;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.cli.CommandLineArgs;
import uno.anahata.asi.model.resource.AbstractPathResource;
import uno.anahata.asi.model.resource.AbstractResource;
import lombok.extern.slf4j.Slf4j;

/**
 * A specialized {@link AsiContainer} for the standalone Swing application.
 * It handles the storage and parsing of command-line arguments to configure
 * initial agi sessions.
 * 
 * @author anahata
 */
@Slf4j
public class StandaloneAsiContainer extends AsiContainer {
    
    /** The raw command-line arguments passed to the application. */
    private final String[] cmdLineArgs;
    
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
    public void onAgiCreated(Agi agi) {
        super.onAgiCreated(agi);
        log.info("Parsing command-line arguments for new standalone agi.");
        CommandLineArgs.parse(agi, cmdLineArgs);
    }

    /**
     * {@inheritDoc}
     * <p>
     * Creates a new agi session using the {@link StandaloneAgiConfig}.
     * </p>
     */
    @Override
    public Agi createNewAgi() {
        return new Agi(new StandaloneAgiConfig(this));
    }

    /**
     * {@inheritDoc}
     * <p>
     * Opens the file associated with the resource using the system's default 
     * application via {@link java.awt.Desktop}.
     * </p>
     */
    @Override
    public void openResource(AbstractResource<?, ?> resource) {
        if (resource instanceof AbstractPathResource<?> apr) {
            try {
                log.info("Opening resource via system desktop: {}", apr.getPath());
                java.awt.Desktop.getDesktop().open(new File(apr.getPath()));
            } catch (Exception e) {
                log.error("Failed to open resource via system desktop: " + apr.getPath(), e);
            }
        }
    }
}
