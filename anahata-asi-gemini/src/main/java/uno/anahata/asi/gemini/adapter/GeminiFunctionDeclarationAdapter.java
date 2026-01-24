/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.gemini.adapter;

import com.google.genai.types.FunctionDeclaration;
import com.google.genai.types.Schema;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import uno.anahata.asi.internal.JacksonUtils;
import uno.anahata.asi.model.tool.AbstractTool;
import uno.anahata.asi.model.tool.AbstractToolParameter;

/**
 * An object-oriented adapter that converts a single Anahata AbstractTool into a
 * native Google GenAI FunctionDeclaration. This adapter leverages the
 * {@code parametersJsonSchema} and {@code responseJsonSchema} fields to provide
 * full OpenAPI 3.0-compliant schemas to the model.
 *
 * @author anahata-ai
 */
@Slf4j
@RequiredArgsConstructor
public class GeminiFunctionDeclarationAdapter {

    private final AbstractTool<?, ?> anahataTool;
    private final boolean useNativeSchemas;

    /**
     * Performs the conversion from the Anahata tool to a Google GenAI FunctionDeclaration.
     * @return The corresponding FunctionDeclaration, or null if conversion fails.
     */
    public FunctionDeclaration toGoogle() {
        try {
            FunctionDeclaration.Builder builder = FunctionDeclaration.builder()
                .name(anahataTool.getName())
                .description(anahataTool.getDescription());

            if (useNativeSchemas) {
                return toGoogleNative(builder);
            } else {
                return toGoogleJson(builder);
            }
        } catch (Exception e) {
            log.error("Failed to convert Anahata tool to Gemini FunctionDeclaration: {}", anahataTool.getName(), e);
            return null;
        }
    }

    private FunctionDeclaration toGoogleNative(FunctionDeclaration.Builder builder) {
        Schema.Builder paramsBuilder = Schema.builder()
            .type("object");

        Map<String, Schema> properties = new LinkedHashMap<>();
        List<String> required = new ArrayList<>();

        for (AbstractToolParameter p : anahataTool.getParameters()) {
            Schema paramSchema = JacksonUtils.parse(p.getJsonSchema(), Schema.class);
            properties.put(p.getName(), paramSchema);
            if (p.isRequired()) {
                required.add(p.getName());
            }
        }

        if (!properties.isEmpty()) {
            paramsBuilder.properties(properties);
            paramsBuilder.required(required);
            builder.parameters(paramsBuilder.build());
        }

        if (anahataTool.getResponseJsonSchema() != null) {
            builder.response(JacksonUtils.parse(anahataTool.getResponseJsonSchema(), Schema.class));
        }

        return builder.build();
    }

    private FunctionDeclaration toGoogleJson(FunctionDeclaration.Builder builder) {
        // 1. Build the parameters JSON Schema object
        Map<String, Object> parametersSchema = new LinkedHashMap<>();
        parametersSchema.put("type", "object");

        Map<String, Object> properties = new LinkedHashMap<>();
        List<String> required = new ArrayList<>();

        for (AbstractToolParameter p : anahataTool.getParameters()) {
            // Parse our core JSON schema string into a Map
            Map<String, Object> paramSchema = JacksonUtils.parse(p.getJsonSchema(), Map.class);
            
            // PURIFY: Strip redundant fields to save tokens
            JacksonUtils.purifySchema(paramSchema);

            String paramDescription = p.getDescription();
            String schemaDescription = (String) paramSchema.get("description");

            // Merge descriptions if both are present
            String finalDescription = StringUtils.isBlank(schemaDescription)
                ? paramDescription
                : paramDescription + "\n\n(Details: " + schemaDescription + ")";

            paramSchema.put("description", finalDescription);
            properties.put(p.getName(), paramSchema);

            if (p.isRequired()) {
                required.add(p.getName());
            }
        }

        if (!properties.isEmpty()) {
            parametersSchema.put("properties", properties);
            parametersSchema.put("required", required);
            // Inject the raw Map into the parametersJsonSchema field
            builder.parametersJsonSchema(parametersSchema);
        }

        // 2. Build the response JSON Schema object
        if (anahataTool.getResponseJsonSchema() != null) {
            Map<String, Object> responseSchema = JacksonUtils.parse(anahataTool.getResponseJsonSchema(), Map.class);
            JacksonUtils.purifySchema(responseSchema);
            builder.responseJsonSchema(responseSchema);
        }

        return builder.build();
    }
}
