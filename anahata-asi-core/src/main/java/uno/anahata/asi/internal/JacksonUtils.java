package uno.anahata.asi.internal;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.lang.reflect.Type;
import java.util.Collections;
import java.util.Map;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.tool.schema.SchemaProvider;

/**
 * A generic utility class for Jackson-based JSON operations.
 * It uses the centrally configured ObjectMapper from SchemaProvider to ensure
 * consistency across the application.
 */
@NoArgsConstructor(access = AccessLevel.PRIVATE)
@Slf4j
public final class JacksonUtils {

    private static final ObjectMapper MAPPER = SchemaProvider.OBJECT_MAPPER;
    private static final ObjectMapper DEFAULT_MAPPER = new ObjectMapper();

    /**
     * Converts any Java object (POJO, Map, List, Primitive) into a "pure" JSON-safe 
     * structure containing only standard Java types (Map, List, String, Number, Boolean, null).
     * <p>
     * This is the "Final Gate" used to ensure that external libraries with "greedy" 
     * reflection-based serializers (like the Google GenAI library) never encounter 
     * a complex POJO that might trigger forbidden API calls or recursion.
     * </p>
     * 
     * @param o The object to purify.
     * @return A pure JSON-safe representation of the object.
     */
    public static Object toJsonPrimitives(Object o) {
        if (o == null) return null;
        // 1. Use OUR configured mapper to turn the object into a JSON tree.
        // This handles ElementHandle and @JsonIgnore correctly.
        JsonNode node = MAPPER.valueToTree(o);
        // 2. Convert that tree back into a plain Java Object (Maps/Lists/Primitives).
        return MAPPER.convertValue(node, Object.class);
    }

    /**
     * Converts a "pure" JSON-safe structure (Map, List, etc.) into a rich Java POJO.
     * 
     * @param <T> The target type.
     * @param o The source object (usually a Map or List from the API).
     * @param type The target reflection Type.
     * @return The enriched POJO.
     */
    public static <T> T toPojo(Object o, Type type) {
        if (o == null) return null;
        return MAPPER.convertValue(o, MAPPER.constructType(type));
    }

    /**
     * Serializes an object to a pretty-printed JSON string.
     *
     * @param o The object to serialize.
     * @return A formatted JSON string, or an error message if serialization fails.
     */
    public static String prettyPrint(Object o) {
        if (o == null) {
            return "null";
        }
        try {
            return MAPPER.writerWithDefaultPrettyPrinter().writeValueAsString(o);
        } catch (JsonProcessingException e) {
            log.warn("Failed to pretty print object of type {}", o.getClass().getName(), e);
            return "Error: Could not serialize object to JSON. " + e.getMessage();
        }
    }

    /**
     * Pretty-prints a JSON string.
     *
     * @param jsonString The JSON string to pretty-print.
     * @return A formatted JSON string, or an error message if parsing or serialization fails.
     */
    public static String prettyPrintJsonString(String jsonString) {
        if (jsonString == null || jsonString.trim().isEmpty()) {
            return jsonString;
        }
        try {
            // Use DEFAULT_MAPPER for parsing to avoid any custom configurations from SchemaProvider
            JsonNode jsonNode = DEFAULT_MAPPER.readTree(jsonString);
            return MAPPER.writerWithDefaultPrettyPrinter().writeValueAsString(jsonNode);
        } catch (JsonProcessingException e) {
            log.warn("Failed to pretty print JSON string: {}", jsonString, e);
            return "Error: Could not parse or pretty-print JSON string. " + e.getMessage();
        }
    }
}
