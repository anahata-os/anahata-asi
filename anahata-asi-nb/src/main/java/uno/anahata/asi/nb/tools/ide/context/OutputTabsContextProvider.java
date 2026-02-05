/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi.nb.tools.ide.context;

import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.context.BasicContextProvider;
import uno.anahata.asi.model.core.RagMessage;
import uno.anahata.asi.nb.tools.ide.NetBeansOutput;

/**
 * Provides a list of all open tabs in the NetBeans Output Window,
 * including a tail of the content for each tab.
 * 
 * @author anahata
 */
@Slf4j
public class OutputTabsContextProvider extends BasicContextProvider {

    /**
     * Constructs a new provider for output tabs.
     */
    public OutputTabsContextProvider() {
        super("netbeans-open-output-tabs", "Open Output Tabs", "A list of all open tabs in the NetBeans Output Window with a tail of their content.");
    }

    @Override
    public void populateMessage(RagMessage ragMessage) throws Exception {
        ragMessage.addTextPart(NetBeansOutput.getMarkdownReport());
    }
}
