/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.standalone.swing;

import uno.anahata.asi.AsiContainer;
import uno.anahata.asi.chat.Chat;
import uno.anahata.asi.cli.CommandLineArgs;

/**
 * A specialized {@link AsiContainer} for the standalone Swing application.
 * It handles the storage and parsing of command-line arguments to configure
 * initial chat sessions.
 * 
 * @author anahata
 */
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
     * arguments and apply them to the newly created chat session.
     * 
     * @param chat The newly created chat session.
     */
    @Override
    public void onChatCreated(Chat chat) {
        super.onChatCreated(chat); 
        CommandLineArgs.parse(chat, cmdLineArgs);
    }
}
