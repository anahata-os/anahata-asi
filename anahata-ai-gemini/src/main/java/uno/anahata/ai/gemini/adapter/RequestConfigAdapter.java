package uno.anahata.ai.gemini.adapter;

import com.google.genai.types.FunctionDeclaration;
import com.google.genai.types.GenerateContentConfig;
import com.google.genai.types.Tool;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import uno.anahata.ai.model.core.RequestConfig;

/**
 * A focused adapter responsible for converting our model-agnostic RequestConfig
 * into a Google GenAI GenerateContentConfig.
 *
 * @author pablo
 */
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public final class RequestConfigAdapter {

    /**
     * Converts an Anahata RequestConfig to a Google GenAI GenerateContentConfig.
     *
     * @param anahataConfig The Anahata config to convert.
     * @return The corresponding GenerateContentConfig, or null if the input is null.
     */
    public static GenerateContentConfig toGoogle(RequestConfig anahataConfig) {
        if (anahataConfig == null) {
            return null;
        }

        GenerateContentConfig.Builder builder = GenerateContentConfig.builder();

        Optional.ofNullable(anahataConfig.getTemperature()).ifPresent(builder::temperature);
        Optional.ofNullable(anahataConfig.getMaxOutputTokens()).ifPresent(builder::maxOutputTokens);
        
        // Fix: topK and topP are Floats in the Gemini API, but Integer/Float in our core model.
        // We must convert Integer topK to Float for the builder.
        Optional.ofNullable(anahataConfig.getTopK()).map(Integer::floatValue).ifPresent(builder::topK);
        Optional.ofNullable(anahataConfig.getTopP()).ifPresent(builder::topP);

        if (anahataConfig.getTools() != null && !anahataConfig.getTools().isEmpty()) {
            List<FunctionDeclaration> declarations = anahataConfig.getTools().stream()
                .map(FunctionDeclarationAdapter::toGoogle)
                .filter(Objects::nonNull)
                .collect(Collectors.toList());

            if (!declarations.isEmpty()) {
                Tool tool = Tool.builder().functionDeclarations(declarations).build();
                builder.tools(tool);
            }
        }

        return builder.build();
    }
}
