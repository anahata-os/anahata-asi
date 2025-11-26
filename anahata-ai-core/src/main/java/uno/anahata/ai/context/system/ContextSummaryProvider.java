/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.ai.context.system;

import java.util.Collections;
import java.util.List;
import uno.anahata.ai.chat.Chat;
import uno.anahata.ai.model.core.AbstractPart;
import uno.anahata.ai.model.core.TextPart;

/**
 * A context provider that generates a high-level summary of the conversation context.
 * This is a direct port of the proven V1 provider.
 * 
 * @author pablo
 */
public class ContextSummaryProvider extends AbstractSystemInstructionsProvider {

    public ContextSummaryProvider(Chat chat) {
        super(chat, "core-context-summary", "Context Summary", "Provides a summary of the conversation context.");
    }

    @Override
    public List<String> getSystemInstructions() throws Exception {
        StringBuilder sb = new StringBuilder();
        sb.append("# Context Summary\n");
        sb.append("This is a summary of the current conversation context.\n\n");
        // TODO: Implement the full summary logic from V1
        
        return Collections.singletonList(sb.toString());
    }
}
