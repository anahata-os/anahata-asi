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

import com.google.genai.types.FunctionCall;
import com.google.genai.types.FunctionResponse;
import com.google.genai.types.Part;
import com.google.gson.Gson;
import com.google.gson.reflect.TypeToken;
import java.util.Map;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.ai.model.core.AbstractPart;
import uno.anahata.ai.model.core.TextPart;
import uno.anahata.ai.model.tool.AbstractToolCall;
import uno.anahata.ai.model.tool.AbstractToolResponse;
import uno.anahata.ai.tool.ToolManager;

/**
 * A focused adapter for bidirectional conversion between Anahata and Google GenAI Part objects.
 * @author anahata-gemini-pro-2.5
 */
@Slf4j
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public final class PartAdapter {
    private static final Gson GSON = new Gson();

    /**
     * Converts an Anahata AbstractPart to a Google GenAI Part.
     * @param anahataPart The Anahata part to convert.
     * @return The corresponding Google GenAI Part, or null if unsupported.
     */
    public static Part toGoogle(AbstractPart anahataPart) {
        if (anahataPart instanceof TextPart) {
            return Part.fromText(((TextPart) anahataPart).getText());
        }
        if (anahataPart instanceof AbstractToolCall) {
            return toGoogleFunctionCallPart((AbstractToolCall) anahataPart);
        }
        if (anahataPart instanceof AbstractToolResponse) {
            return toGoogleFunctionResponsePart((AbstractToolResponse) anahataPart);
        }
        log.warn("Unsupported Anahata Part type for Google conversion, skipping: {}", anahataPart.getClass().getSimpleName());
        return null;
    }

    /**
     * Converts a Google GenAI Part to an Anahata AbstractPart.
     * @param googlePart The Google part to convert.
     * @param toolManager The ToolManager, required for creating tool calls.
     * @return The corresponding Anahata AbstractPart, or null if unsupported.
     */
    public static AbstractPart toAnahata(Part googlePart, ToolManager toolManager) {
        if (googlePart.text().isPresent()) {
            return new TextPart(googlePart.text().get());
        }
        if (googlePart.functionCall().isPresent()) {
            return FunctionCallAdapter.toAnahata(googlePart.functionCall().get(), toolManager);
        }
        log.warn("Unsupported Gemini Part type for Anahata conversion, skipping: {}", googlePart);
        return null;
    }

    private static Part toGoogleFunctionCallPart(AbstractToolCall anahataCall) {
        // Re-serialize the converted Java objects back into a generic Map for Google
        String json = GSON.toJson(anahataCall.getArgs());
        Map<String, Object> argsMap = GSON.fromJson(json, new TypeToken<Map<String, Object>>() {}.getType());
        
        FunctionCall fc = FunctionCall.builder()
            .name(anahataCall.getName())
            .args(argsMap)
            .build();
        
        return Part.builder().functionCall(fc).build();
    }

    private static Part toGoogleFunctionResponsePart(AbstractToolResponse anahataResponse) {
        // Serialize the result object into a generic Map for Google
        String json = GSON.toJson(anahataResponse.getResult());
        Map<String, Object> responseMap = GSON.fromJson(json, new TypeToken<Map<String, Object>>() {}.getType());

        FunctionResponse fr = FunctionResponse.builder()
            .name(anahataResponse.getCall().getName())
            .response(responseMap)
            .build();
        
        return Part.builder().functionResponse(fr).build();
    }
}
