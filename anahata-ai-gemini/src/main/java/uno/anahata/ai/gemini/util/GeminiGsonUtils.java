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
 * Força Barça!
 */
package uno.anahata.ai.gemini.util;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.TypeAdapter;
import com.google.gson.reflect.TypeToken;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;
import java.io.IOException;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.Optional;

/**
 * Utility class for creating a Gson instance configured to correctly handle
 * the Google GenAI SDK's types, specifically Java 8 Optional.
 * This is a temporary utility to inspect the raw Model JSON.
 */
public final class GeminiGsonUtils {

    private static final Gson GSON = createGson(false);
    private static final Gson PRETTY_PRINT_GSON = createGson(true);

    public static Gson getGson() {
        return GSON;
    }
    
    public static Gson getPrettyPrintGson() {
        return PRETTY_PRINT_GSON;
    }

    /**
     * Pretty prints an object to a JSON string. If the object is a string
     * that is not a valid JSON, it returns the original string.
     *
     * @param value The object to pretty print.
     * @return The pretty printed JSON string or the original string.
     */
    public static String prettyPrint(Object value) {
        if (value instanceof String) {
            String stringValue = (String) value;
            try {
                // Check if it's valid JSON by parsing it with the main GSON instance
                Object json = GSON.fromJson(stringValue, Object.class);
                // Re-serialize with pretty printing to ensure formatting
                return PRETTY_PRINT_GSON.toJson(json);
            } catch (com.google.gson.JsonSyntaxException e) {
                // Not a JSON string, return it as is
                return stringValue;
            }
        }
        // Not a string, so serialize it directly
        return PRETTY_PRINT_GSON.toJson(value);
    }

    private static Gson createGson(boolean prettyPrinting) {
        GsonBuilder builder = new GsonBuilder()
                .registerTypeAdapterFactory(new OptionalTypeAdapterFactory());

        if (prettyPrinting) {
            builder.setPrettyPrinting();
        }
        
        builder.setLenient();

        return builder.create();
    }

    private static class OptionalTypeAdapterFactory implements com.google.gson.TypeAdapterFactory {
        @Override
        public <T> TypeAdapter<T> create(Gson gson, TypeToken<T> typeToken) {
            if (typeToken.getRawType() != Optional.class) {
                return null;
            }
            final Type type = typeToken.getType();
            final Type valueType = ((ParameterizedType) type).getActualTypeArguments()[0];
            final TypeAdapter<?> valueAdapter = gson.getAdapter(TypeToken.get(valueType));

            return (TypeAdapter<T>) new OptionalTypeAdapter(valueAdapter);
        }
    }

    private static class OptionalTypeAdapter<E> extends TypeAdapter<Optional<E>> {
        private final TypeAdapter<E> valueAdapter;

        public OptionalTypeAdapter(TypeAdapter<E> valueAdapter) {
            this.valueAdapter = valueAdapter;
        }

        @Override
        public void write(JsonWriter out, Optional<E> value) throws IOException {
            if (value != null && value.isPresent()) {
                valueAdapter.write(out, value.get());
            } else {
                out.nullValue();
            }
        }

        @Override
        public Optional<E> read(JsonReader in) throws IOException {
            if (in.peek() == com.google.gson.stream.JsonToken.NULL) {
                in.nextNull();
                return Optional.empty();
            }
            E value = valueAdapter.read(in);
            return Optional.ofNullable(value);
        }
    }
}
