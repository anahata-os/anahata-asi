/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi.nb.tools.ide.context;

import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.context.BasicContextProvider;
import uno.anahata.asi.model.core.RagMessage;
import uno.anahata.asi.nb.tools.ide.NetBeansTopComponents;

/**
 * Provides a real-time Markdown table of all open TopComponents (windows) in the IDE.
 * 
 * @author anahata
 */
@Slf4j
public class OpenTopComponentsContextProvider extends BasicContextProvider {

    /**
     * Constructs a new provider for open TopComponents.
     */
    public OpenTopComponentsContextProvider() {
        super("netbeans-open-topcomponents", "Open TopComponents", "A real-time Markdown table of all open TopComponents (windows) in the IDE.");
    }

    @Override
    public void populateMessage(RagMessage ragMessage) throws Exception {
        ragMessage.addTextPart(NetBeansTopComponents.getMarkdownReport());
    }
}
