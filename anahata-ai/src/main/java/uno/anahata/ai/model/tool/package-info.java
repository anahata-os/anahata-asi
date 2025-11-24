/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
/**
 * Defines the model-agnostic domain model for the tool (function) calling subsystem.
 * <p>
 * This package provides a set of model-agnostic POJOs and enums that represent
 * the entire lifecycle of a tool call, from its declaration to its final execution
 * result. By creating this abstraction layer, we decouple the core logic of the
 * {@code ToolManager} and the UI from the specific data types of any single AI
 * provider (like Google's {@code FunctionCall} or {@code FunctionResponse}).
 *
 * @author anahata-gemini-pro-2.5
 */
package uno.anahata.ai.model.tool;
