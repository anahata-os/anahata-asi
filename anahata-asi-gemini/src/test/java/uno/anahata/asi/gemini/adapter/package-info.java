/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */

/**
 * Provides unit tests and verification subjects for the Gemini adapter layer.
 * <p>
 * This package is dedicated to ensuring that the translation logic between 
 * the Anahata model-agnostic domain and the native Google GenAI structures 
 * (like {@code FunctionDeclaration}) is deterministic and compliant with 
 * the target API specifications.
 * </p>
 * <p>
 * It includes torture-tests for:
 * </p>
 * <ul>
 *   <li><b>Function Declaration Mapping</b>: Verifying method names, 
 *       descriptions, and parameter schemas.</li>
 *   <li><b>Response Synthesis</b>: Ensuring the {@code JavaMethodToolResponse} 
 *       wrapper is correctly represented in both native and JSON modes.</li>
 *   <li><b>Void Handling</b>: Validating the surgical omission of return 
 *       metadata for void-returning tools.</li>
 * </ul>
 * 
 * @author anahata
 */
package uno.anahata.asi.gemini.adapter;
