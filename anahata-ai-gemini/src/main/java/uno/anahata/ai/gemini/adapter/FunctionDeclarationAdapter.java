package uno.anahata.ai.gemini.adapter;

import com.google.genai.types.FunctionDeclaration;
import com.google.genai.types.Schema;
import com.google.genai.types.Type;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.ai.gemini.schema.GeminiSchemaAdapter;
import uno.anahata.ai.model.tool.AbstractTool;
import uno.anahata.ai.model.tool.ToolParameter;

/**
 * A focused adapter responsible for converting our model-agnostic AbstractTool
 * into a Google GenAI FunctionDeclaration.
 *
 * @author pablo
 */
@Slf4j
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public final class FunctionDeclarationAdapter {

    /**
     * Converts an Anahata AbstractTool to a Google GenAI FunctionDeclaration.
     *
     * @param tool The tool to convert.
     * @return A new FunctionDeclaration, or null if conversion fails.
     */
    public static FunctionDeclaration toGoogle(AbstractTool<?, ?> tool) {
        try {
            FunctionDeclaration.Builder builder = FunctionDeclaration.builder()
                .name(tool.getName())
                .description(tool.getDescription());

            Map<String, Schema> properties = new LinkedHashMap<>();
            List<String> required = new ArrayList<>();

            for (ToolParameter p : tool.getParameters()) {
                Schema paramSchema = GeminiSchemaAdapter.getGeminiSchema(p.getJsonSchema());
                properties.put(p.getName(), paramSchema);
                if (p.isRequired()) {
                    required.add(p.getName());
                }
            }

            if (!properties.isEmpty()) {
                Schema paramsSchema = Schema.builder()
                    .type(Type.Known.OBJECT)
                    .properties(properties)
                    .required(required)
                    .build();
                builder.parameters(paramsSchema);
            }

            // Fix: Use the correct method name from the refactored AbstractTool
            Schema responseSchema = GeminiSchemaAdapter.getGeminiSchema(tool.getResponseJsonSchema());
            if (responseSchema != null) {
                builder.response(responseSchema);
            }

            return builder.build();
        } catch (Exception e) {
            log.error("Failed to convert Anahata tool to Gemini FunctionDeclaration: {}", tool.getName(), e);
            return null;
        }
    }
}
