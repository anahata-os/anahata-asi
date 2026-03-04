/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.resource.v2;

import java.util.Collections;
import java.util.List;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.internal.TokenizerUtils;
import uno.anahata.asi.model.core.RagMessage;

/**
 * A resource view that interprets content as plain text.
 * <p>
 * This view integrates the V2 {@link TextViewport} for high-fidelity 
 * streaming of large files.
 * </p>
 */
@Slf4j
@Getter
@Setter
@NoArgsConstructor
public class TextView extends AbstractResourceView {

    /** The viewport engine for processing text. */
    private final TextViewport viewport = new TextViewport();

    /** Cached processed output from the last reload. */
    private String processedCache;

    /**
     * Constructs a TextView and links it to its parent resource.
     * @param owner The owning resource.
     */
    public TextView(Resource owner) {
        this.owner = owner;
    }

    /**
     * Constructs a TextView with specific initial settings.
     * @param owner The owning resource.
     * @param settings The initial viewport configuration.
     */
    public TextView(Resource owner, TextViewportSettings settings) {
        this.owner = owner;
        this.viewport.setSettings(settings);
    }

    /**
     * Updates the viewport settings and triggers a reactive reload 
     * via the parent resource.
     * 
     * @param settings The new settings to apply.
     */
    public void setViewportSettings(TextViewportSettings settings) {
        this.viewport.setSettings(settings);
        markViewDirty();
    }

    /** {@inheritDoc} 
     * Performs a memory-efficient stream processing using the viewport engine.
     */
    @Override
    public void reload(ResourceHandle handle) throws Exception {
        log.debug("Reloading TextView (Streaming) for: {}", handle.getUri());
        this.processedCache = viewport.process(handle);
    }

    /** {@inheritDoc} 
     * Adds the processed text chunk to the RAG message, wrapped in markdown.
     */
    @Override
    public void populateRag(RagMessage ragMessage, ResourceHandle handle) throws Exception {
        ragMessage.addTextPart("```\n" + (processedCache != null ? processedCache : "") + "\n```");
    }

    /** {@inheritDoc} 
     * Returns the processed text for system instruction injection.
     */
    @Override
    public List<String> getInstructions(ResourceHandle handle) throws Exception {
        return Collections.singletonList("```\n" + (processedCache != null ? processedCache : "") + "\n```");
    }

    /** {@inheritDoc} */
    @Override
    public int getTokenCount(ResourceHandle handle) {
        return TokenizerUtils.countTokens(processedCache != null ? processedCache : "") + 20;
    }

    /** {@inheritDoc} */
    @Override
    public String toString() {
        return viewport.toString();
    }
}
