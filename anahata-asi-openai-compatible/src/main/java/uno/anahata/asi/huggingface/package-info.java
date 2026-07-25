/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
/**
 * Provides the provider implementation for the Hugging Face Inference API.
 * <p>
 * This module extends the OpenAI-compatible stack but adds a "Deep Inspection" layer.
 * On model discovery, it queries the Hugging Face Hub directly to fetch:
 * </p>
 * <ul>
 *   <li><b>Architecture</b>: To detect reasoning fields (e.g. DeepSeek's {@code reasoning_content}).</li>
 *   <li><b>Tokenizers</b>: To detect R1-style tags (e.g. {@code <think>}) in templates.</li>
 *   <li><b>Configuration</b>: To derive accurate context windows and output limits.</li>
 * </ul>
 * <p>
 * This ensures that even generic inference endpoints gain full ASI capability 
 * (reasoning, tool calling) by understanding the underlying model's DNA.
 * </p>
 * 
 * @author anahata
 */
package uno.anahata.asi.huggingface;
