/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */

/**
 * Modernized, standalone SwingX core components (including {@link org.jdesktop.swingx.JXTable},
 * {@link org.jdesktop.swingx.JXTree}, and {@link org.jdesktop.swingx.JXLabel}).
 * <p>
 * <b>Architectural Rationale for In-Tree Modernization:</b>
 * The legacy upstream SwingX library ({@code org.swinglabs.swingx:swingx-all}) contains hard-coded references
 * to {@code java.applet.Applet.class} and legacy SecurityManager APIs that were terminally deprecated and completely
 * removed in modern Java (JDK 21+ / JDK 25+ / JDK 26+). To ensure long-term stability, thread safety, and zero runtime
 * linkage errors when running on modern Java runtimes, these essential high-performance components have been cloned,
 * decoupled from obsolete applet infrastructures, and modernized directly within {@code anahata-asi-swing}.
 * </p>
 *
 * @author anahata
 */
package org.jdesktop.swingx;
