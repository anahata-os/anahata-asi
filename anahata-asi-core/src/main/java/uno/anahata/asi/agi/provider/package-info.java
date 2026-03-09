/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */

/**
 * Provides the core domain model for Anahata's context management system.
 * <p>
 * This package contains the V2 simplified, hierarchical representation of conversation history.
 * At its heart is the {@link uno.anahata.asi.model.core.AbstractMessage}, which acts as a container
 * for multiple {@link uno.anahata.asi.model.core.AbstractPart} objects.
 * </p>
 * <p>
 * This Domain-Driven model enables sophisticated features like:
 * <ul>
 *   <li><b>Context Window Garbage Collection (CwGC):</b> Atomic, depth-based pruning of individual message parts.</li>
 *   <li><b>In-Band Metadata Injection:</b> Improving model self-awareness through interleaved invisible headers.</li>
 *   <li><b>Multi-Modal Context:</b> Support for mixed text and binary content within a single conversation turn.</li>
 *   <li><b>Reactive UI:</b> Event-driven updates via {@link uno.anahata.asi.model.core.PropertyChangeSource}.</li>
 * </ul>
 * </p>
 * 
 * @author anahata
 */
package uno.anahata.asi.agi.provider;
