/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.mistral;

import com.fasterxml.jackson.databind.JsonNode;
import java.util.ArrayList;
import java.util.List;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.openai.compatible.OpenAiCompatibleModel;
import uno.anahata.asi.openai.compatible.OpenAiCompatibleReasoningStyle;

/**
 * A specialized model implementation for Mistral AI that parses capability flags,
 * max context length, FIM (Fill-In-The-Middle), and reasoning configurations from Mistral's /v1/models JSON.
 * 
 * @author anahata
 */
@Slf4j
@Getter
public class MistralModel extends OpenAiCompatibleModel {

    private final boolean completionChat;
    private final boolean completionFim;
    private final boolean reasoning;
    private final boolean vision;
    private final boolean audio;
    private final boolean audioTranscription;
    private final boolean audioSpeech;
    private final boolean ocr;
    private final boolean moderation;
    private final String descriptionText;

    /**
     * Constructs a new MistralModel by parsing its capabilities and limits from the JSON node.
     * 
     * @param provider The parent MistralAiProvider instance.
     * @param node The raw JSON node returned by Mistral's /v1/models endpoint.
     */
    public MistralModel(MistralAiProvider provider, JsonNode node) {
        super(provider, node);

        // 1. Context Length & Description
        if (node.has("max_context_length")) {
            setMaxInputTokens(node.path("max_context_length").asInt(262144));
        }

        this.descriptionText = node.path("description").asText("");

        // 2. Parse Capabilities
        JsonNode caps = node.path("capabilities");
        if (caps.isObject()) {
            this.completionChat = caps.path("completion_chat").asBoolean(true);
            this.completionFim = caps.path("completion_fim").asBoolean(false);
            this.reasoning = caps.path("reasoning").asBoolean(false);
            this.vision = caps.path("vision").asBoolean(false);
            this.audio = caps.path("audio").asBoolean(false);
            this.audioTranscription = caps.path("audio_transcription").asBoolean(false);
            this.audioSpeech = caps.path("audio_speech").asBoolean(false);
            this.ocr = caps.path("ocr").asBoolean(false);
            this.moderation = caps.path("moderation").asBoolean(false);

            setSupportsFunctionCalling(caps.path("function_calling").asBoolean(true));

            // Reasoning Extraction Configuration
            if (this.reasoning) {
                setReasoningStyle(OpenAiCompatibleReasoningStyle.TAGS);
                setReasoningTags(List.of("<think>", "</think>"));
            }
        } else {
            this.completionChat = true;
            this.completionFim = false;
            this.reasoning = false;
            this.vision = false;
            this.audio = false;
            this.audioTranscription = false;
            this.audioSpeech = false;
            this.ocr = false;
            this.moderation = false;
        }
    }

    /**
     * {@inheritDoc}
     * <p>Maps Mistral capabilities to supported action endpoints.</p>
     */
    @Override
    public List<String> getSupportedActions() {
        List<String> actions = new ArrayList<>();
        if (completionChat) {
            actions.add("chat/completions");
        }
        if (completionFim) {
            actions.add("fim/completions");
        }
        if (audioTranscription) {
            actions.add("audio/transcriptions");
        }
        if (audioSpeech) {
            actions.add("audio/speech");
        }
        if (ocr) {
            actions.add("ocr");
        }
        if (moderation) {
            actions.add("moderations");
        }
        return actions.isEmpty() ? List.of("chat/completions") : actions;
    }

    /**
     * {@inheritDoc}
     * <p>Maps Mistral vision/audio flags to supported response modalities.</p>
     */
    @Override
    public List<String> getSupportedResponseModalities() {
        List<String> modalities = new ArrayList<>();
        modalities.add("TEXT");
        if (vision) {
            modalities.add("IMAGE");
        }
        if (audio || audioSpeech) {
            modalities.add("AUDIO");
        }
        return modalities;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getDescription() {
        return descriptionText.isBlank() ? super.getDescription() : descriptionText;
    }
}
