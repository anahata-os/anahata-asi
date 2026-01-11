/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.ai.internal;

import com.knuddels.jtokkit.Encodings;
import com.knuddels.jtokkit.api.Encoding;
import com.knuddels.jtokkit.api.EncodingRegistry;
import com.knuddels.jtokkit.api.EncodingType;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;

/**
 * A centralized utility class for token counting, powered by the jtokkit library.
 * This provides a single, efficient, and reusable tokenizer instance for the entire application.
 *
 * @author anahata
 */
@Slf4j
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public final class TokenizerUtils {

    private static final EncodingRegistry REGISTRY = Encodings.newDefaultEncodingRegistry();
    
    /**
     * A robust, general-purpose tokenizer. CL100K_BASE is the encoding used by
     * gpt-4, gpt-3.5-turbo, and text-embedding-ada-002. It serves as a high-quality
     * default for token counting.
     */
    private static final Encoding TOKENIZER = REGISTRY.getEncoding(EncodingType.CL100K_BASE);

    /**
     * Counts the number of tokens in the given text using the application's default tokenizer.
     *
     * @param text The text to count tokens for. Can be null or empty.
     * @return The number of tokens, or 0 if the text is null or empty.
     */
    public static int countTokens(String text) {
        if (text == null || text.isEmpty()) {
            return 0;
        }
        try {
            return TOKENIZER.countTokens(text);
        } catch (Exception e) {
            log.error("Failed to count tokens for text snippet: '{}'",
                      text.substring(0, Math.min(text.length(), 100)), e);
            // Fallback to a rough estimate if the tokenizer fails unexpectedly.
            return text.length() / 4;
        }
    }
}
