/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Fora Bara! */
package uno.anahata.ai.internal;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonSyntaxException;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;

/**
 * Utility class for JSON operations, primarily pretty printing.
 * This class is placed in the core module to be accessible by all other modules.
 *
 * @author anahata-ai
 */
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public final class JsonUtils {

    private static final Gson PRETTY_PRINT_GSON = new GsonBuilder().setPrettyPrinting().create();
    private static final Gson GSON = new Gson();

    /**
     * Pretty prints a JSON string. If the input is not valid JSON, it returns
     * the original string.
     *
     * @param jsonString The string to pretty print.
     * @return The pretty printed JSON string or the original string.
     */
    public static String prettyPrint(String jsonString) {
        if (jsonString == null || jsonString.trim().isEmpty()) {
            return "";
        }
        try {
            // Parse with the non-pretty-printing Gson to check for validity
            Object json = GSON.fromJson(jsonString, Object.class);
            // Re-serialize with pretty printing
            return PRETTY_PRINT_GSON.toJson(json);
        } catch (JsonSyntaxException e) {
            // Not a JSON string, return it as is
            return jsonString;
        }
    }
}
