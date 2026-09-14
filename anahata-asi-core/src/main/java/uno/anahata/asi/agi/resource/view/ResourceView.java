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
     * Returns the token count for the current processed state of this view.
     *      * 
     *      * @return The token count.
     */
    int getTokenCount();

    /**
     * Resets the cached token count of this view, forcing a lazy recalculation on the next query.
     */
    void resetTokenCount();
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
     * Returns the percentage of the underlying resource content currently visible
     * or provided by this view to the model.
     * <p>
     * For unclipped or complete views (e.g. full-view text or complete media), this returns 100.0.
     * For paginated, clipped, or truncated views, returns a value between 0.0 and 100.0.
     * </p>
     *
     * @return The percentage of visible content, from 0.0 to 100.0.
     */
    double getVisiblePercentage();

    /**
     * Checks whether this view's content is currently truncated or clipped
     * (i.e. less than 100% of the underlying resource is visible).
     *
     * @return true if truncated, false if 100% complete.
     */
    default boolean isTruncated() {
        return getVisiblePercentage() < 100.0;
    }

    /**
     * Associates this view with its parent resource.
     * @param owner The owning Resource.
     */
    void setOwner(Resource owner);
}
