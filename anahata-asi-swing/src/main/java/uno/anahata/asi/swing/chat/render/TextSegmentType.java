/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.chat.render;

/**
 * Defines the type of a text segment within a {@link AbstractTextSegmentRenderer}.
 * This helps in determining which specific segment renderer to use.
 *
 * @author anahata
 */
public enum TextSegmentType {
    /** A regular markdown text segment. */
    TEXT,
    /** A code block segment. */
    CODE
}
