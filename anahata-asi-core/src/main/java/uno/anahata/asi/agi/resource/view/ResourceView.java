/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.agi.resource.view;

import java.util.Collections;
import java.util.List;
import uno.anahata.asi.agi.message.RagMessage;
import uno.anahata.asi.agi.resource.Resource;

/**
 * The perspective through which a resource is viewed by the model.
 * <p>
 * Implementations manage content processing (like tail/grep) and model presentation.
 * Views are responsible for the "Semantic" interpretation.
 * </p>
 */
public interface ResourceView {
    /** 
     * Reloads and processes the content from the source. 
     * This is called by the Resource orchestrator when the resource is stale or dirty.
     * 
     * @throws Exception if processing or reading fails.
     */
    void reload() throws Exception;

    /** 
     * Populates the RAG message with the appropriate parts (Text or Blob). 
     * 
     * @param ragMessage The target RAG message.
     * @throws Exception if population fails.
     */
    void populateRag(RagMessage ragMessage) throws Exception;

    /** 
     * Provides system instructions if the resource is in that position. 
     * Returns a list of processed text blocks.
     * 
     * @return A list of instruction strings.
     * @throws Exception if instruction generation fails.
     */
    default List<String> getInstructions() throws Exception {
        return Collections.emptyList();
    }
    
    /** 
     * Returns an estimated token count for the current processed state of the view. 
     * 
     * @return The estimated token count.
     */
    int getTokenCount();

    /**
     * Returns a machine-readable header summarizing the interpretation state.
     * <p>
     * <b>Technical Purity:</b> The first line always contains the implementation 
     * class FQN. The second line provides the salient interpretation details 
     * (e.g., viewport metrics).
     * </p>
     * @return The header string.
     */
    default String getHeader() {
        return "View fqn: " + getClass().getName();
    }

    /**
     * Associates this view with its parent resource.
     * @param owner The owning Resource.
     */
    void setOwner(Resource owner);
}
