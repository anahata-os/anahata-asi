package uno.anahata.ai.gemini.adapter;

import com.google.genai.types.FunctionCall;
import java.util.Map;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import uno.anahata.ai.model.tool.AbstractToolCall;
import uno.anahata.ai.tool.ToolManager;

/**
 * A focused adapter responsible for converting a Google GenAI FunctionCall into
 * our model-agnostic AbstractToolCall.
 *
 * @author pablo
 */
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public final class FunctionCallAdapter {

    /**
     * Converts a Google GenAI FunctionCall to an Anahata AbstractToolCall.
     *
     * @param googleFc The FunctionCall received from the Google API.
     * @param toolManager The ToolManager instance, used to find the correct
     * tool definition.
     * @return A new AbstractToolCall, which will be a BadToolCall if the tool
     * was not found.
     */
    public static AbstractToolCall toAnahata(FunctionCall googleFc, ToolManager toolManager) {
        // Fix: name() and args() return Optional in the Google GenAI API.
        String name = googleFc.name().orElse("");
        Map<String, Object> args = googleFc.args().orElse(Map.of());
        String id = googleFc.id().orElse(null);

        return toolManager.createToolCall(id, name, args);
    }
}
