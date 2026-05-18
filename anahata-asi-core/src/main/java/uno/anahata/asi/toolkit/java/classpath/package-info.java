/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
/**
 * Provides metadata-aware classpath inspection and token-optimized formatting.
 * <p>
 * This package is responsible for "understanding" the environment's classpath. 
 * It uses a set of specialized {@link uno.anahata.asi.toolkit.java.classpath.JarHandler}s 
 * to look inside JAR files and extract semantic metadata (Artifact IDs, 
 * versions, vendors).
 * </p>
 * <p>
 * The {@link uno.anahata.asi.toolkit.java.classpath.VeryPrettyClassPathPrinter} 
 * then uses this metadata to generate a highly compressed, tree-based 
 * representation of the classpath, ensuring that even environments with 
 * thousands of libraries fit within the LLM's context window.
 * </p>
 * 
 * @author anahata
 */
package uno.anahata.asi.toolkit.java.classpath;
