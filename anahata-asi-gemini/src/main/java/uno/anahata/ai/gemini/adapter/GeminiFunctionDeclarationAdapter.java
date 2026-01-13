/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.ai.gemini.adapter;

import com.google.genai.types.FunctionDeclaration;
import com.google.genai.types.Schema;
import com.google.genai.types.Type;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import uno.anahata.ai.gemini.schema.GeminiSchemaAdapter;
import uno.anahata.ai.model.tool.AbstractTool;
import uno.anahata.ai.model.tool.AbstractToolParameter;

/**
 * An object-oriented adapter that converts a single Anahata AbstractTool into a
 * native Google GenAI FunctionDeclaration.
 *
 * @author anahata-ai
 */
@Slf4j
@RequiredArgsConstructor
public class GeminiFunctionDeclarationAdapter {

    private final AbstractTool<?, ?> anahataTool;

    /**
     * Performs the conversion from the Anahata tool to a Google GenAI FunctionDeclaration.
     * @return The corresponding FunctionDeclaration, or null if conversion fails.
     */
    public FunctionDeclaration toGoogle() {
        try {
            FunctionDeclaration.Builder builder = FunctionDeclaration.builder()
                .name(anahataTool.getName())
                .description(anahataTool.getDescription());

            Map<String, Schema> properties = new LinkedHashMap<>();
            List<String> required = new ArrayList<>();

            for (AbstractToolParameter p : anahataTool.getParameters()) {
                Schema baseSchema = GeminiSchemaAdapter.getGeminiSchema(p.getJsonSchema());
                String paramDescription = p.getDescription();
                String schemaDescription = baseSchema.description().orElse("");

                String finalDescription = StringUtils.isBlank(schemaDescription)
                    ? paramDescription
                    : paramDescription + "\n\n(Details: " + schemaDescription + ")";

                Schema finalSchema = baseSchema.toBuilder()
                    .description(finalDescription)
                    .build();

                properties.put(p.getName(), finalSchema);
                
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

            Schema responseSchema = GeminiSchemaAdapter.getGeminiSchema(anahataTool.getResponseJsonSchema());
            if (responseSchema != null) {
                builder.response(responseSchema);
            }

            return builder.build();
        } catch (Exception e) {
            log.error("Failed to convert Anahata tool to Gemini FunctionDeclaration: {}", anahataTool.getName(), e);
            return null;
        }
    }
}
