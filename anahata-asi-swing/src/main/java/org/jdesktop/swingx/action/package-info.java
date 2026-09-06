/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */

/**
 * Modernized SwingX action framework and callback-based action bindings (such as {@link org.jdesktop.swingx.action.BoundAction}).
 * <p>
 * <b>Architectural Rationale for In-Tree Modernization:</b>
 * Extracted and maintained directly within {@code anahata-asi-swing} to eliminate transitive dependencies on the legacy
 * SwingX library which referenced {@code java.applet.Applet.class} (removed in modern JDKs) and legacy reflective invocation
 * patterns, ensuring seamless integration with modern Swing UI pipelines.
 * </p>
 *
 * @author anahata
 */
package org.jdesktop.swingx.action;
