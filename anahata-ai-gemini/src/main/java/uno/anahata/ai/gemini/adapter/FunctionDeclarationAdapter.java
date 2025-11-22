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
import org.apache.commons.lang3.StringUtils;
import uno.anahata.ai.gemini.schema.GeminiSchemaAdapter;
import uno.anahata.ai.model.tool.AbstractTool;
import uno.anahata.ai.model.tool.AbstractToolParameter;

/**
 * A focused adapter responsible for converting our model-agnostic AbstractTool
 * into a Google GenAI FunctionDeclaration.
 *
 * @author anahata-gemini-pro-2.5
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

            for (AbstractToolParameter p : tool.getParameters()) {
                // Step 1: Build the base schema. This contains the type ("What") and the @Schema description ("How").
                Schema baseSchema = GeminiSchemaAdapter.getGeminiSchema(p.getJsonSchema());

                // Step 2: Get the two description parts.
                String paramDescription = p.getDescription(); // The "Why" from @AIToolParam
                String schemaDescription = baseSchema.description().orElse(""); // The "How" from @Schema

                // Step 3: Prepend the "Why" to the "How" to create the final, rich description.
                String finalDescription;
                if (StringUtils.isBlank(schemaDescription)) {
                    finalDescription = paramDescription;
                } else {
                    finalDescription = paramDescription + "\n\n(Details: " + schemaDescription + ")";
                }

                // Step 4: Create a new schema, overwriting the description with our composite one.
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