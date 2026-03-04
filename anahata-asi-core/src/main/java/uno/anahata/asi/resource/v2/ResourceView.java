/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.resource.v2;

import java.util.Collections;
import java.util.List;
import uno.anahata.asi.model.core.RagMessage;

/**
 * The perspective through which a resource is viewed by the model.
 * <p>
 * Implementations manage content processing (like tail/grep) and model presentation.
 * Views are responsible for the "Semantic" interpretation.
 * </p>
 */
public interface ResourceView {
    /** 
     * Reloads and processes the content from the handle. 
     * This is called by the Resource orchestrator when the resource is stale or dirty.
     * 
     * @param handle The source handle to read from.
     * @throws Exception if processing or reading fails.
     */
    void reload(ResourceHandle handle) throws Exception;

    /** 
     * Populates the RAG message with the appropriate parts (Text or Blob). 
     * 
     * @param ragMessage The target RAG message.
     * @param handle The source handle for metadata context.
     * @throws Exception if population fails.
     */
    void populateRag(RagMessage ragMessage, ResourceHandle handle) throws Exception;

    /** 
     * Provides system instructions if the resource is in that position. 
     * Returns a list of processed text blocks.
     * 
     * @param handle The source handle for metadata context.
     * @return A list of instruction strings.
     * @throws Exception if instruction generation fails.
     */
    default List<String> getInstructions(ResourceHandle handle) throws Exception {
        return Collections.emptyList();
    }
    
    /** 
     * Returns an estimated token count for the current processed state of the view. 
     * 
     * @param handle The source handle for metadata context.
     * @return The estimated token count.
     */
    int getTokenCount(ResourceHandle handle);

    /**
     * Associates this view with its parent resource.
     * @param owner The owning Resource.
     */
    void setOwner(Resource owner);
}
