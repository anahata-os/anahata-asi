/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */

/**
 * Provides the "Tooling Proving Grounds" for verifying the core tool execution 
 * engine, thread-safety, and context propagation.
 * <p>
 * This package contains high-salience test subjects and unit tests designed 
 * to validate the architectural integrity of the Anahata ASI tooling framework.
 * </p>
 * <p>
 * Key Functional Areas:
 * </p>
 * <ul>
 *   <li><b>Surgical Stress Testing</b>: {@link uno.anahata.ai.tool.BigTestClass} 
 *       provides a massive, stable target for verifying coordinate-aware 
 *       line mutations and multi-turn refactoring.</li>
 *   <li><b>Context Propagation</b>: Tests for verifying that {@code ToolContext} 
 *       and {@code JavaMethodToolResponse} states are correctly shared across 
 *       thread boundaries and tool calls.</li>
 *   <li><b>Mock Infrastructure</b>: Lightweight implementations of 
 *       {@link uno.anahata.asi.AbstractAsiContainer} and toolkits for isolated 
 *       unit testing of the manager and SPI layers.</li>
 *   <li><b>Schema Validation</b>: Interaction with the tool-declaration engine 
 *       to ensure Java-to-OpenAPI mapping is deterministic.</li>
 * </ul>
 * 
 * @author anahata
 */
package uno.anahata.ai.tool;
