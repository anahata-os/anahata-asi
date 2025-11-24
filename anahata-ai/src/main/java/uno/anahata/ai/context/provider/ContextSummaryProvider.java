/*
 * Copyright 2025 Anahata.
 *
 * Licensed under the Anahata Software License (ASL) V2.0;
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      https://github.com/pablo-anahata/anahata-ai-parent/blob/main/LICENSE
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * Fora Bara!
 */
package uno.anahata.ai.context.provider;

import java.util.Collections;
import java.util.List;
import uno.anahata.ai.Chat;
import uno.anahata.ai.model.core.AbstractPart;
import uno.anahata.ai.model.core.TextPart;

/**
 * A context provider that generates a high-level summary of the conversation context.
 * This is a direct port of the proven V1 provider.
 * 
 * @author pablo
 */
public class ContextSummaryProvider extends AbstractContextProvider {

    public ContextSummaryProvider() {
        super("core-context-summary", "Context Summary", "Provides a summary of the conversation context.", ContextPosition.PROMPT_AUGMENTATION);
    }

    @Override
    public List<AbstractPart> getParts(Chat chat) throws Exception {
        StringBuilder sb = new StringBuilder();
        sb.append("# Context Summary\n");
        sb.append("This is a summary of the current conversation context.\n\n");
        // TODO: Implement the full summary logic from V1
        
        return Collections.singletonList(new TextPart(sb.toString()));
    }
}
