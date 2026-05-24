/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.internal;

import com.knuddels.jtokkit.Encodings;
import com.knuddels.jtokkit.api.Encoding;
import com.knuddels.jtokkit.api.EncodingRegistry;
import com.knuddels.jtokkit.api.EncodingType;
import com.sentencepiece.Model;
import java.io.File;
import java.io.InputStream;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.StandardCopyOption;
import java.util.List;
import uno.anahata.asi.agi.provider.TokenizerType;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;

/**
 * A centralized utility class for token counting, powered by the jtokkit
 * library. This provides a single, efficient, and reusable tokenizer instance
 * for the entire application.
 *
 * @author anahata
 */
@Slf4j
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public final class TokenizerUtils {
    /**
     * Tokkit default registry.
     */
    private static final EncodingRegistry REGISTRY = Encodings.newDefaultEncodingRegistry();

    /**
     * A robust, general-purpose tokenizer. CL100K_BASE is the encoding used by
     * gpt-4, gpt-3.5-turbo, and text-embedding-ada-002. It serves as a
     * high-quality default for token counting.
     */
    private static final Encoding TOKENIZER = REGISTRY.getEncoding(EncodingType.CL100K_BASE);

    /**
     * The in-memory cached Gemini SentencePiece model. This is lazily loaded on
     * the first tokenization request.
     */
    private static Model geminiModel = null;

    /**
     * Flag tracking if a load attempt of the Gemini BPE model has occurred,
     * preventing redundant disk IO.
     */
    private static boolean geminiModelLoadAttempted = false;

    /**
     * Lazily and thread-safely resolves the Gemini SentencePiece model. It
     * attempts to locate the model file on the classpath resource files under
     * {@code /gemini.bpe.model}, {@code /gemma.bpe.model}, or
     * {@code /tokenizer.model}. If found, it copies the resource to a temporary
     * file to load it. If not found, it falls back to {@code O200K_BASE}.
     *
     * @return The parsed SentencePiece model, or {@code null} if loading fails.
     */
    private static synchronized Model getGeminiModel() {
        if (!geminiModelLoadAttempted) {
            geminiModelLoadAttempted = true;
            try {
                URL resource = TokenizerUtils.class.getResource("/gemini.bpe.model");
                if (resource == null) {
                    resource = TokenizerUtils.class.getResource("/gemma.bpe.model");
                }
                if (resource == null) {
                    resource = TokenizerUtils.class.getResource("/tokenizer.model");
                }
                if (resource != null) {
                    log.info("Found Gemini BPE model resource, loading...");
                    File tempFile = File.createTempFile("gemini-bpe-", ".model");
                    tempFile.deleteOnExit();
                    try (InputStream in = resource.openStream()) {
                        if (in != null) {
                            Files.copy(in, tempFile.toPath(), StandardCopyOption.REPLACE_EXISTING);
                            geminiModel = Model.parseFrom(tempFile.toPath());
                            log.info("Successfully loaded Gemini SentencePiece model from classpath resource");
                        }
                    }
                } else {
                    log.warn("Gemini BPE model resource (/gemini.bpe.model) not found on classpath. Falling back to O200K_BASE for token estimation.");
                }
            } catch (Exception e) {
                log.error("Failed to load Gemini BPE model, falling back to O200K_BASE", e);
            }
        }
        return geminiModel;
    }

    /**
     * Counts the number of tokens in the given text using a specific tokenizer
     * strategy.
     *
     * @param text The text to count tokens for.
     * @param type The tokenizer strategy to use.
     * @return The number of tokens.
     */
    public static int countTokens(String text, TokenizerType type) {
        if (text == null || text.isEmpty()) {
            return 0;
        }
        try {
            if (type == TokenizerType.GEMINI) {
                Model model = getGeminiModel();
                if (model != null) {
                    List<?> tokens = model.encode(text);
                    return tokens != null ? tokens.size() : 0;
                }
                type = TokenizerType.O200K_BASE;
            }

            Encoding encoding = switch (type) {
                case CL100K_BASE ->
                    REGISTRY.getEncoding(EncodingType.CL100K_BASE);
                case O200K_BASE ->
                    REGISTRY.getEncoding(EncodingType.O200K_BASE);
                default ->
                    TOKENIZER; // Fallback to CL100K_BASE for estimation
            };
            return encoding.countTokens(text);
        } catch (Exception e) {
            log.error("Failed to count tokens for text snippet using {}: '{}'", type,
                    text.substring(0, Math.min(text.length(), 100)), e);
            // Fallback to a rough estimate if the tokenizer fails unexpectedly.
            return text.length() / 4;
        }
    }

}
