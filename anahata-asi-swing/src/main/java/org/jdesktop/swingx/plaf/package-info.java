/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */

/**
 * Pluggable Look and Feel (PLAF) addon infrastructure and component UI delegates (including {@link org.jdesktop.swingx.plaf.LookAndFeelAddons}).
 * <p>
 * <b>Architectural Rationale for In-Tree Modernization:</b>
 * This package provides dynamic Look and Feel addon registration for custom SwingX components. It has been modernized
 * in {@code anahata-asi-swing} to remove obsolete {@code java.applet.Applet.class} dependencies and deprecated
 * {@code AccessController}/{@code SecurityManager} checks, providing clean classloader resolution and native theme
 * support across FlatLaf, NetBeans, and IntelliJ IDEA environments on JDK 21-26+.
 * </p>
 *
 * @author anahata
 */
package org.jdesktop.swingx.plaf;
