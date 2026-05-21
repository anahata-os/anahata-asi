/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
/**
 * Provides utility classes and helper components for the Anahata ASI - NetBeans integration.
 * <p>This package includes specialized utilities for handling NetBeans-specific types and 
 * infrastructure, such as:</p>
 * <ul>
 *     <li><b>JSON Interoperability</b>: Custom Jackson modules (e.g., {@code ElementHandleModule}) 
 *     for serializing and deserializing opaque NetBeans API handles.</li>
 *     <li><b>IO Management</b>: Decorators for the NetBeans {@code InputOutput} system 
 *     (e.g., {@code TeeInputOutput}) that allow capturing and mirroring output streams 
 *     for programmatic inspection.</li>
 * </ul>
 * 
 * @author anahata
 */
package uno.anahata.asi.nb.util;
