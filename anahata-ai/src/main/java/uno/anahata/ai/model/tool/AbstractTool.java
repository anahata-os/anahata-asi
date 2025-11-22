/* Licensed under the Anahata Software License, Version 108 - https://github.com/anahata-os/anahata-ai/blob/main/LICENSE */
package uno.anahata.ai.model.tool;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.reflect.TypeToken;
import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import lombok.Getter;
import lombok.NonNull;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.ai.tool.schema.SchemaProvider;

/**
 * The abstract base class for a tool, now generic on its Parameter and Call types.
 * @author anahata-gemini-pro-2.5
 * @param <P> The specific subclass of AbstractToolParameter this tool uses.
 * @param <C> The specific subclass of AbstractToolCall this tool creates.
 */
@Getter
@Slf4j
public abstract class AbstractTool<P extends AbstractToolParameter, C extends AbstractToolCall> {
    private static final Gson GSON = new GsonBuilder().setPrettyPrinting().create();
    
    /** The default retention policy for tool calls, in number of user turns. */
    public static final int DEFAULT_RETENTION_TURNS = 5;
    
    /** The fully qualified name of the tool, e.g., "LocalFiles.readFile". This is immutable. */
    @NonNull
    protected final String name;

    /** A detailed description of what the tool does. */
    protected String description;

    /** A reference to the parent toolkit that owns this tool. Can be null for standalone tools. */
    protected AbstractToolkit toolkit;

    /** The user's configured preference for this tool, determining its execution behavior. */
    @Setter
    protected ToolPermission permission;

    /** The number of turns this tool call should be retained in the context. */
    @Setter
    private int retentionTurns;

    /** A rich, ordered list of the tool's parameters. */
    private final List<P> parameters = new ArrayList<>();
    
    /** A pre-generated, language-agnostic JSON schema for the tool's return type. Can be null for void methods. */
    @Getter
    protected String responseJsonSchema;

    protected AbstractTool(@NonNull String name) {
        this.name = name;
    }
    
    /**
     * Factory method to create a tool-specific call object from raw model data.
     * @param id The call ID.
     * @param args The raw arguments from the model.
     * @return A new tool call instance.
     */
    public abstract C createCall(String id, Map<String, Object> args);
    
    /**
     * Template method hook for subclasses to provide their specific Response type.
     * @return The reflection Type of the corresponding AbstractToolResponse subclass.
     */
    public abstract Type getResponseType();

    /**
     * Dynamically generates a rich JSON schema for the tool's *entire response*,
     * including status, errors, and the specific schema of the 'result' field.
     * @return A JSON schema string, or null on failure.
     */
    /*
    public String getResponseJsonSchema() {
        try {
            Type responseType = getResponseType();
            if (responseType == null || responseType.equals(void.class)) {
                return null;
            }

            // 1. Get base schema for the responseType (e.g., JavaMethodToolResponse)
            String baseSchemaJson = SchemaProvider.generateInlinedSchemaString(responseType);
            if (baseSchemaJson == null) {
                log.warn("Could not generate base schema for response type: {}", responseType.getTypeName());
                return null;
            }

            // 2. Parse the base schema into a mutable Map
            Type mapType = new TypeToken<Map<String, Object>>() {}.getType();
            Map<String, Object> baseSchemaMap = GSON.fromJson(baseSchemaJson, mapType);

            // 3. Get the schema for the actual result (e.g., String, FileInfo)
            String resultSchemaJson = getReturnTypeJsonSchema();
            
            // 4. Get the properties map from the base schema, failing fast if it's missing.
            Object propertiesObj = baseSchemaMap.get("properties");
            if (!(propertiesObj instanceof Map)) {
                throw new IllegalStateException("SchemaProvider generated a base schema without a 'properties' map for type: " + responseType.getTypeName());
            }
            Map<String, Object> propertiesMap = (Map<String, Object>) propertiesObj;

            if (resultSchemaJson == null) {
                // This is a void method. Surgically remove the 'result' property.
                propertiesMap.remove("result");
            } else {
                // This is a non-void method. Surgically inject the result schema.
                Map<String, Object> resultSchemaMap = GSON.fromJson(resultSchemaJson, mapType);
                if (propertiesMap.containsKey("result")) {
                    propertiesMap.put("result", resultSchemaMap);
                }
            }

            // 5. Return the final, modified schema string
            return GSON.toJson(baseSchemaMap);

        } catch (Exception e) {
            log.error("Error generating response schema for tool {}", getName(), e);
            return null;
        }
    }
*/
}