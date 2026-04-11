/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */

/**
 * Provides comprehensive validation suites for the AI schema generation engine.
 * <p>
 * This package is dedicated to verifying the deterministic mapping of Java types 
 * and class hierarchies to OpenAI-compatible JSON schemas. It ensures that 
 * complex structures, recursive relationships, and tool response wrappers 
 * are correctly translated for LLM consumption.
 * </p>
 * <p>
 * Key Validation Areas:
 * </p>
 * <ul>
 *   <li><b>Structural Integrity</b>: Verifying the correct generation of JSON properties, 
 *       types, and nested objects.</li>
 *   <li><b>Recursive Mapping</b>: Ensuring that self-referencing data structures (like trees) 
 *       are handled safely without infinite loops.</li>
 *   <li><b>Response Wrapping</b>: Validating the {@link uno.anahata.asi.agi.tool.spi.java.JavaMethodToolResponse} 
 *       envelope injection logic.</li>
 *   <li><b>Type Coercion</b>: Testing the mapping of Java primitives, collections, and 
 *       maps to their OpenAPI equivalents.</li>
 * </ul>
 * 
 * @author anahata
 */
package uno.anahata.ai.tool.schema;
