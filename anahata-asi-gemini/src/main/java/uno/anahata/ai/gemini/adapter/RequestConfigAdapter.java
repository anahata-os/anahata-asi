/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.ai.gemini.adapter;

import com.google.genai.types.ComputerUse;
import com.google.genai.types.Content;
import com.google.genai.types.EnterpriseWebSearch;
import com.google.genai.types.FileSearch;
import com.google.genai.types.FunctionCallingConfig;
import com.google.genai.types.FunctionCallingConfigMode;
import com.google.genai.types.FunctionDeclaration;
import com.google.genai.types.GenerateContentConfig;
import com.google.genai.types.GoogleMaps;
import com.google.genai.types.GoogleSearch;
import com.google.genai.types.GoogleSearchRetrieval;
import com.google.genai.types.Part;
import com.google.genai.types.ThinkingConfig;
import com.google.genai.types.Tool;
import com.google.genai.types.ToolCodeExecution;
import com.google.genai.types.ToolConfig;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.ai.model.core.RequestConfig;
import uno.anahata.ai.model.provider.ServerTool;

/**
 * A focused adapter responsible for converting our model-agnostic RequestConfig
 * into a Google GenAI GenerateContentConfig.
 *
 * @author anahata-gemini-pro-2.5
 */
@NoArgsConstructor(access = AccessLevel.PRIVATE)
@Slf4j
public final class RequestConfigAdapter {

    /**
     * Converts an Anahata RequestConfig to a Google GenAI
     * GenerateContentConfig.
     *
     * @param anahataConfig The Anahata config to convert.
     * @return The corresponding GenerateContentConfig, or null if the input is
     * null.
     */
    public static GenerateContentConfig toGoogle(RequestConfig anahataConfig) {
        if (anahataConfig == null) {
            return null;
        }
        
        log.info("Generating GenerateContentConfig for " + anahataConfig);

        GenerateContentConfig.Builder builder = GenerateContentConfig.builder();
        
        if (!anahataConfig.getSystemInstructions().isEmpty()) {
            List<Part> parts = new ArrayList<>();
            for (String si : anahataConfig.getSystemInstructions()) {
               parts.add(Part.fromText(si));
            }
            
            //builder.systemInstruction(Content.builder().role("system").parts(anahataConfig.getSystemInstructions().stream().map(p -> Part.fromText(p))));
            Content sysInstContet = Content.builder().role("system").parts(parts).build();
            builder.systemInstruction(sysInstContet);
        }
        
        List<String> modalities = anahataConfig.getResponseModalities();
        if (modalities != null && !modalities.isEmpty()) {
            builder.responseModalities(modalities);
        } else {
            builder.responseModalities("TEXT");
        }
        
        builder.thinkingConfig(ThinkingConfig.builder().includeThoughts(true).build());

        Optional.ofNullable(anahataConfig.getTemperature()).ifPresent(builder::temperature);
        Optional.ofNullable(anahataConfig.getMaxOutputTokens()).ifPresent(builder::maxOutputTokens);

        // Fix: topK and topP are Floats in the Gemini API, but Integer/Float in our core model.
        // We must convert Integer topK to Float for the builder.
        Optional.ofNullable(anahataConfig.getTopK()).map(Integer::floatValue).ifPresent(builder::topK);
        Optional.ofNullable(anahataConfig.getTopP()).ifPresent(builder::topP);
        
        if (anahataConfig.getCandidateCount() != null) {
            builder.candidateCount(anahataConfig.getCandidateCount());
        }

        if (anahataConfig.getLocalTools() != null && !anahataConfig.getLocalTools().isEmpty()) {
            log.info("Local tools enabled, adding " + anahataConfig.getLocalTools().size() + " tools");
            List<FunctionDeclaration> declarations = anahataConfig.getLocalTools().stream()
                    .map(tool -> new GeminiFunctionDeclarationAdapter(tool).toGoogle())
                    .filter(Objects::nonNull)
                    .collect(Collectors.toList());

            if (!declarations.isEmpty()) {
                Tool tool = Tool.builder().functionDeclarations(declarations).build();
                builder.tools(tool);
                ToolConfig tc = ToolConfig.builder()
                        .functionCallingConfig(FunctionCallingConfig.builder()
                                .mode(FunctionCallingConfigMode.Known.VALIDATED)).build();
                builder.toolConfig(tc);
            }
        } else if (anahataConfig.isServerToolsEnabled()) {
            List<ServerTool> enabledTools = anahataConfig.getEnabledServerTools();
            if (enabledTools != null && !enabledTools.isEmpty()) {
                log.info("Server tools enabled, adding {} tools", enabledTools.size());
                Tool.Builder toolBuilder = Tool.builder();
                for (ServerTool st : enabledTools) {
                    Object id = st.getId();
                    if (id == GoogleSearch.class) {
                        toolBuilder.googleSearch(GoogleSearch.builder().build());
                    } else if (id == GoogleSearchRetrieval.class) {
                        toolBuilder.googleSearchRetrieval(GoogleSearchRetrieval.builder().build());
                    } else if (id == ToolCodeExecution.class) {
                        toolBuilder.codeExecution(ToolCodeExecution.builder().build());
                    } else if (id == GoogleMaps.class) {
                        toolBuilder.googleMaps(GoogleMaps.builder().build());
                    } else if (id == EnterpriseWebSearch.class) {
                        toolBuilder.enterpriseWebSearch(EnterpriseWebSearch.builder().build());
                    } else if (id == FileSearch.class) {
                        toolBuilder.fileSearch(FileSearch.builder().build());
                    } else if (id == ComputerUse.class) {
                        toolBuilder.computerUse(ComputerUse.builder().build());
                    }
                }
                builder.tools(toolBuilder.build());
            }
        } else {
            log.info("Both local and server tools are disabled.");
        }

        return builder.build();
    }
}
