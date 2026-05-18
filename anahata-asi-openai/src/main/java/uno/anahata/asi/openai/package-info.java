/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */

/**
 * High-fidelity provider implementation for the OpenAI Responses API (/v1/responses).
 * <p>
 * This package contains the core logic for interacting with modern OpenAI models
 * (gpt-4o and successors) using a stateful, item-based architecture that supports
 * native tool execution, web search, and code interpretation.
 * </p>
 * <p>
 * Key architectural patterns:
 * <ul>
 *   <li><b>Partitioned Construction:</b> Payload assembly that separates Identity (Config) 
 *   from Memory (History) for UI transparency.</li>
 *   <li><b>Multimodal Harvesting:</b> Real-time extraction of citations, search results, 
 *   and generated media from API output items.</li>
 * </ul>
 * </p>
 * 
 * @author anahata
 */
package uno.anahata.asi.openai;
