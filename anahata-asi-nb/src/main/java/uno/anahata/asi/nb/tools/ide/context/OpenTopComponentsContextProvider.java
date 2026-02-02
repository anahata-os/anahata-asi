/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi.nb.tools.ide.context;

import java.util.Collections;
import java.util.List;
import uno.anahata.asi.context.BasicContextProvider;
import uno.anahata.asi.model.core.RagMessage;
import uno.anahata.asi.nb.tools.ide.IDE;

/**
 * Provides a real-time Markdown table of all open TopComponents (windows) in the IDE.
 */
public class OpenTopComponentsContextProvider extends BasicContextProvider {

    private final IDE ideToolkit;

    public OpenTopComponentsContextProvider(IDE ideToolkit) {
        super("netbeans-open-topcomponents", "Open TopComponents", "A real-time Markdown table of all open TopComponents (windows) in the IDE.");
        this.ideToolkit = ideToolkit;
    }

    @Override
    public void populateMessage(RagMessage ragMessage) throws Exception {
        String openFiles = ideToolkit.getOpenTopComponentsMarkdown();
        ragMessage.addTextPart(openFiles);
    }
}
