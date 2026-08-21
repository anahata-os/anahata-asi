/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.yam.tools.benchmarks;

import lombok.Builder;
import uno.anahata.asi.agi.provider.ThinkingLevel;

/**
 * Encapsulates the identity and inference parameters of an aspirant model (candidate participant) in a benchmark test.
 * <p>
 * Binds the target AI provider UUID, specific model ID, and reasoning mode / thinking level
 * into a single reusable descriptor.
 * </p>
 *
 * @param providerUuid The unique UUID of the AI provider hosting the model (e.g. {@code "Gemini"}, {@code "NovaRouteAI"}).
 * @param modelId The exact model identifier string (e.g. {@code "models/gemini-flash-latest"}, {@code "kimi/Kimi-K3"}).
 * @param thinkingLevel The thinking level/mode configured for the candidate session. Defaults to {@link ThinkingLevel#THINKING_LEVEL_UNSPECIFIED}.
 * 
 * @author anahata
 */
@Builder
public record BenchmarkParticipant(
        String providerUuid,
        String modelId,
        ThinkingLevel thinkingLevel
) {

    /**
     * Canonical constructor normalizing null thinking levels to {@link ThinkingLevel#THINKING_LEVEL_UNSPECIFIED}.
     *
     * @param providerUuid The AI provider UUID.
     * @param modelId The model ID.
     * @param thinkingLevel The thinking level.
     */
    public BenchmarkParticipant {
        if (thinkingLevel == null) {
            thinkingLevel = ThinkingLevel.THINKING_LEVEL_UNSPECIFIED;
        }
    }

    /**
     * Creates a candidate participant with {@link ThinkingLevel#THINKING_LEVEL_UNSPECIFIED},
     * letting the provider/endpoint decide the default reasoning behavior.
     *
     * @param providerUuid The AI provider UUID.
     * @param modelId The model identifier string.
     * @return The configured {@link BenchmarkParticipant}.
     */
    public static BenchmarkParticipant of(String providerUuid, String modelId) {
        return new BenchmarkParticipant(providerUuid, modelId, ThinkingLevel.THINKING_LEVEL_UNSPECIFIED);
    }

    /**
     * Creates a candidate participant with an explicit thinking mode level.
     *
     * @param providerUuid The AI provider UUID.
     * @param modelId The model identifier string.
     * @param thinkingLevel The thinking mode level.
     * @return The configured {@link BenchmarkParticipant}.
     */
    public static BenchmarkParticipant of(String providerUuid, String modelId, ThinkingLevel thinkingLevel) {
        return new BenchmarkParticipant(providerUuid, modelId, thinkingLevel != null ? thinkingLevel : ThinkingLevel.THINKING_LEVEL_UNSPECIFIED);
    }
}
