/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
/**
 * Provides the V2 Universal Resource Pipeline (URP) infrastructure.
 * <p>
 * This package implements a capability-based resource model that decouples the 
 * source of data ({@link uno.anahata.asi.resource.v2.ResourceHandle}) from its 
 * interpretation by the AI model ({@link uno.anahata.asi.resource.v2.ResourceView}).
 * </p>
 * <p>
 * <b>Core Components:</b>
 * </p>
 * <ul>
 *   <li>{@link uno.anahata.asi.resource.v2.Resource}: The central orchestrator 
 *       that manages lifecycle and RAG population.</li>
 *   <li>{@link uno.anahata.asi.resource.v2.ResourceHandle}: Abstract source for 
 *       raw data and metadata (FileObject, Path, URL).</li>
 *   <li>{@link uno.anahata.asi.resource.v2.ResourceView}: Abstract perspective 
 *       (Text, Media) that processes content for the prompt.</li>
 * </ul>
 */
package uno.anahata.asi.agi.resource;
