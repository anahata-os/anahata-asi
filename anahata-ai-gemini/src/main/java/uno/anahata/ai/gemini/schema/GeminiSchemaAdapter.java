/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.ai.gemini.schema;

import com.google.genai.types.Content;
import com.google.genai.types.FunctionCall;
import com.google.genai.types.Part;
import com.google.genai.types.Schema;
import com.google.genai.types.Type;
import com.google.gson.Gson;
import com.google.gson.reflect.TypeToken;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.ai.tool.schema.SchemaProvider;

/**
 * Adapts the core framework's rich JSON schema generation into the simpler,
 * native Google GenAI {@code Schema} objects required for function declarations.
 * It also provides utilities for sanitizing incoming {@code Content} to ensure
 * all {@code FunctionCall} parts have a stable ID.
 *
 * @author anahata-gemini-pro-2.5
 */
@Slf4j
public class GeminiSchemaAdapter {

    private static final Gson GSON = new Gson();
    private static final Map<Class<?>, Type.Known> PRIMITIVE_MAP = new HashMap<>();
    private static final Schema VOID_SCHEMA = Schema.builder().build();
    private static final AtomicInteger functionCallIdCounter = new AtomicInteger(1);

    static {
        PRIMITIVE_MAP.put(String.class, Type.Known.STRING);
        PRIMITIVE_MAP.put(Integer.class, Type.Known.INTEGER);
        PRIMITIVE_MAP.put(int.class, Type.Known.INTEGER);
        PRIMITIVE_MAP.put(Long.class, Type.Known.INTEGER);
        PRIMITIVE_MAP.put(long.class, Type.Known.INTEGER);
        PRIMITIVE_MAP.put(Float.class, Type.Known.NUMBER);
        PRIMITIVE_MAP.put(float.class, Type.Known.NUMBER);
        PRIMITIVE_MAP.put(Double.class, Type.Known.NUMBER);
        PRIMITIVE_MAP.put(double.class, Type.Known.NUMBER);
        PRIMITIVE_MAP.put(Boolean.class, Type.Known.BOOLEAN);
        PRIMITIVE_MAP.put(boolean.class, Type.Known.BOOLEAN);
    }

    public static Schema getGeminiSchema(java.lang.reflect.Type type) throws Exception {
        return getGeminiSchema(type, false);
    }
    
    public static Schema getGeminiSchema(java.lang.reflect.Type type, boolean includeJsonSchemaId) throws Exception {
        if (type == null || type.equals(void.class) || type.equals(Void.class)) {
            return VOID_SCHEMA;
        }

        String inlinedSchema = SchemaProvider.generateInlinedSchemaString(type);
        return getGeminiSchema(inlinedSchema);
    }
    
    public static Schema getGeminiSchema(String jsonSchema) {
        if (jsonSchema == null || jsonSchema.trim().isEmpty() || jsonSchema.trim().equals("{}")) {
            return null; // Return null for void/empty schemas
        }
        Map<String, Object> schemaMap = GSON.fromJson(jsonSchema, new TypeToken<Map<String, Object>>() {}.getType());
        return buildSchemaFromMap(schemaMap);
    }
    
    public static Schema getGeminiSchema(Class<?> clazz) throws Exception {
        return getGeminiSchema((java.lang.reflect.Type) clazz, false);
    }

    private static Schema buildSchemaFromMap(Map<String, Object> map) {
        if (map == null || map.isEmpty()) return null;

        Schema.Builder builder = Schema.builder();

        String fqn = (String) map.get("title");
        if (fqn == null) {
            fqn = "N/A";
        }
        
        // For now, just use the FQN as the title as requested.
        // The logic can be expanded later to include the JSON ID if needed.
        builder.title(fqn);

        if (map.containsKey("description")) {
            builder.description((String) map.get("description"));
        }

        if (map.containsKey("type")) {
            String typeStr = (String) map.get("type");
            switch (typeStr.toUpperCase()) {
                case "STRING":  builder.type(Type.Known.STRING);  break;
                case "NUMBER":  builder.type(Type.Known.NUMBER);  break;
                case "INTEGER": builder.type(Type.Known.INTEGER); break;
                case "BOOLEAN": builder.type(Type.Known.BOOLEAN); break;
                case "ARRAY":   builder.type(Type.Known.ARRAY);   break;
                case "OBJECT":  builder.type(Type.Known.OBJECT);  break;
            }
        }

        if (map.containsKey("_enum")) {
            List<?> rawList = (List<?>) map.get("_enum");
            List<String> enumValues = rawList.stream().map(Object::toString).collect(Collectors.toList());
            builder.enum_(enumValues);
        }

        if (map.containsKey("properties")) {
            Map<String, Map<String, Object>> propertiesMap = (Map<String, Map<String, Object>>) map.get("properties");
            Map<String, Schema> schemaProperties = propertiesMap.entrySet().stream()
                    .collect(Collectors.toMap(Map.Entry::getKey, e -> buildSchemaFromMap(e.getValue())));
            builder.properties(schemaProperties);
        }

        if (map.containsKey("required")) {
            builder.required((List<String>) map.get("required"));
        }

        if (map.containsKey("items")) {
            Schema itemsSchema = buildSchemaFromMap((Map<String, Object>) map.get("items"));
            builder.items(itemsSchema);
        }

        return builder.build();
    }
    
    /**
     * Extracts the tool call ID from a Part, checking both FunctionCall and FunctionResponse.
     *
     * @param part The part to inspect.
     * @return An Optional containing the ID if found, otherwise empty.
     */
    public static Optional<String> getToolCallId(Part part) {
        if (part.functionCall().isPresent()) {
            return part.functionCall().get().id();
        }
        if (part.functionResponse().isPresent()) {
            return part.functionResponse().get().id();
        }
        return Optional.empty();
    }
    
    /**
     * Inspects a Content object from the model and ensures every FunctionCall part has a stable ID.
     * If a FunctionCall is missing an ID, this method generates one and reconstructs the
     * entire Content object to include it, preserving all other metadata.
     *
     * @param originalContent The raw content received from the model.
     * @return The original content if no changes were needed, or a new, patched Content object.
     */
    public static Content sanitize(Content originalContent) {
        if (originalContent == null || !originalContent.parts().isPresent()) {
            return originalContent;
        }

        List<Part> originalParts = originalContent.parts().get();
        List<Part> newParts = new ArrayList<>(originalParts.size());
        boolean wasModified = false;

        for (Part originalPart : originalParts) {
            if (originalPart.functionCall().isPresent() && originalPart.functionCall().get().id().isEmpty()) {
                wasModified = true;
                FunctionCall originalFc = originalPart.functionCall().get();
                String newId = String.valueOf(functionCallIdCounter.getAndIncrement());

                // Refactored to use toBuilder() for robustness
                FunctionCall newFc = originalFc.toBuilder()
                        .id(newId)
                        .build();

                Part newPart = originalPart.toBuilder()
                        .functionCall(newFc)
                        .build();
                newParts.add(newPart);
                log.info("Sanitized FunctionCall '{}' with new generated ID '{}'", newFc.name().get(), newId);
            } else {
                newParts.add(originalPart);
            }
        }

        if (!wasModified) {
            return originalContent;
        }

        return originalContent.toBuilder()
                .parts(newParts)
                .build();
    }
}