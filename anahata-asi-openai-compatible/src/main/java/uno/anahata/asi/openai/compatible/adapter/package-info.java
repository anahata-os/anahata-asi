/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */

/**
 * Provides specialized adapters for translating between Anahata's model-agnostic 
 * domain objects and OpenAI-compatible wire formats.
 * <p>
 * This package handles the "Turn Synthesis" logic required by the OpenAI Chat 
 * Completion API, where Anahata's interleaved model messages and tool responses 
 * must be flattened into specific {@code assistant} and {@code tool} message sequences.
 * </p>
 * <p>
 * Key Functional Areas:
 * </p>
 * <ul>
 *   <li><b>Message Mapping</b>: Direct translation of {@link uno.anahata.asi.agi.message.Role} 
 *       to OpenAI-compliant role strings (user, assistant, system, tool).</li>
 *   <li><b>Multimodal Adaptation</b>: Conversion of {@link uno.anahata.asi.agi.message.BlobPart} 
 *       into Base64-encoded Data URIs for the OpenAI vision blocks.</li>
 *   <li><b>Metadata Interleaving</b>: Implementation of Anahata's V2 in-band 
 *       metadata strategy within the constraints of the OpenAI payload structure.</li>
 * </ul>
 * 
 * @author anahata
 */
package uno.anahata.asi.openai.compatible.adapter;
