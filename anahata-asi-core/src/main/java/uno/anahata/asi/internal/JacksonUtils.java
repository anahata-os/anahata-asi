package uno.anahata.asi.internal;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
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
    private static final ObjectMapper DEFAULT_MAPPER = new ObjectMapper(); // New: Default ObjectMapper

    /**
     * Converts an object to a {@code Map<String, Object>}, replicating the logic required by the
     * FunctionResponse type.
     * - If the object naturally serializes to a JSON Object (e.g., a POJO or a Map), it is
     *   converted into a {@code Map<String, Object>}.
     * - If the object serializes to any other JSON type (e.g., an array, a string, a number),
     *   it is wrapped in a Map under the given field name.
     *
     * @param primitiveFieldName The key to use when wrapping a non-POJO type.
     * @param o The object to convert.
     * @return A Map representation of the object, ready for use in a FunctionResponse.
     */
    public static Map<String, Object> convertObjectToMap(String primitiveFieldName, Object o) {
        if (o == null) {
            return Collections.emptyMap();
        }

        // Use Jackson's tree model to inspect the JSON structure without full serialization.
        JsonNode node = MAPPER.valueToTree(o);

        if (node.isObject()) {
            // It's a POJO or a Map, convert it to the required Map type.
            //return MAPPER.convertValue(o, new TypeReference<Map<String, Object>>() {});
            return MAPPER.convertValue(o, new TypeReference<>() {});
        } else {
            // It's a primitive, String, array, or collection. Wrap the original object.
            // The final serialization of the FunctionResponse will correctly handle this structure.
            return Collections.singletonMap(primitiveFieldName, o);
        }
    }
    
    /**
     * Deserializes a {@code Map<String, Object>} back into a specific POJO type.
     *
     * @param <T>   The target type.
     * @param map   The map to convert.
     * @param clazz The class of the target type.
     * @return An instance of the target type, or null if the input map is null or empty.
     */
    public static <T> T convertMapToObject(Map<String, Object> map, Class<T> clazz) {
        if (map == null || map.isEmpty()) {
            return null;
        }
        return MAPPER.convertValue(map, clazz);
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
