/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */

/**
 * Provides a universal, OpenAI-compatible provider implementation for the 
 * Anahata ASI framework.
 * <p>
 * This package implements the standard OpenAI Chat Completion specification, 
 * allowing the framework to interact with any backend that supports the 
 * OpenAI API contract. This includes the official OpenAI service as well as 
 * compatible alternatives like Groq, DeepSeek, and local inference servers 
 * (Ollama, vLLM, etc.).
 * </p>
 * <p>
 * Key Components:
 * </p>
 * <ul>
 *   <li><b>Universal Provider</b>: {@link uno.anahata.asi.openai.compatible.OpenAiChatCompletionsProvider} 
 *       enables custom base URL configuration for multi-endpoint support.</li>
 *   <li><b>Model Logic</b>: {@link uno.anahata.asi.openai.compatible.OpenAiCompatibleModel} handles 
 *       payload synthesis, tool declaration mapping, and standard JDK-based 
 *       HTTP communication.</li>
 *   <li><b>Response Mapping</b>: {@link uno.anahata.asi.openai.compatible.OpenAiCompatibleResponse} 
 *       and {@link uno.anahata.asi.openai.compatible.OpenAiCompatibleModelMessage} parse complex 
 *       OpenAI-style JSON responses (including tool calls and usage metadata) 
 *       into the unified Anahata domain model.</li>
 * </ul>
 * 
 * @author anahata
 */
package uno.anahata.asi.openai.compatible;
