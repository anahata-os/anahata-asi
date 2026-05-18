/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
/**
 * Provides the provider implementation for Anthropic's Claude API.
 * <p>
 * This package implements the Anthropic messages API, supporting both 
 * standard content generation and streaming. It handles Anthropic's 
 * unique "Thinking" blocks and provides native support for their 
 * versioned API contract.
 * </p>
 * <p>
 * Key Components:
 * </p>
 * <ul>
 *   <li><b>Provider</b>: {@link uno.anahata.asi.anthropic.AnthropicProvider} 
 *       manages the HTTP client and versioned request headers.</li>
 *   <li><b>Model Logic</b>: {@link uno.anahata.asi.anthropic.AnthropicModel} 
 *       handles payload preparation and tool declaration mapping.</li>
 *   <li><b>Response Mapping</b>: {@link uno.anahata.asi.anthropic.AnthropicResponse} 
 *       and {@link uno.anahata.asi.anthropic.AnthropicMessage} parse the 
 *       Anthropic-specific response format into the unified Anahata domain model.</li>
 * </ul>
 * 
 * @author anahata
 */
package uno.anahata.asi.anthropic;
