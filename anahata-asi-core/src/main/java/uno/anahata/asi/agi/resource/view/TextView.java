/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.agi.resource.view;

import uno.anahata.asi.agi.resource.handle.ResourceHandle;
import java.beans.PropertyChangeListener;
import java.util.Collections;
import java.util.List;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.message.RagMessage;
import uno.anahata.asi.agi.provider.AbstractModel;
import uno.anahata.asi.persistence.Rebindable;
import uno.anahata.asi.agi.resource.Resource;

/**
 * A resource view that interprets content as plain text.
 * <p>
 * This view integrates the V2 {@link TextViewport} for high-fidelity streaming
 * of large files and implements self-aware reactivity to viewport settings
 * changes.
 * </p>
 */
@Slf4j
@Getter
@Setter
public class TextView extends AbstractResourceView {

    /**
     * The viewport engine for processing text.
     */
    private final TextViewport viewport;

    /**
     * Constructs a default TextView and links the viewport engine.
     */
    public TextView() {
        this.viewport = new TextViewport(this);
    }

    /**
     * Constructs a TextView and links it to its parent resource.
     *
     * @param owner The owning resource.
     */
    public TextView(Resource owner) {
        this();
        this.owner = owner;
    }

    /**
     * Constructs a TextView with specific initial settings.
     *
     * @param owner The owning resource.
     * @param settings The initial viewport configuration.
     */
    public TextView(Resource owner, TextViewportSettings settings) {
        this();
        this.owner = owner;
        this.viewport.setSettings(settings);
    }

    /**
     * {@inheritDoc}
     * <p>
     * Performs memory-efficient stream processing using the viewport
     * engine.</p>
     */
    @Override
    public void reload() throws Exception {
        ResourceHandle handle = owner.getHandle();
        log.debug("Reloading TextView (Streaming) for: {}", handle.getUri());
        viewport.process(handle);
    }

    /**
     * {@inheritDoc}
     * <p>
     * Adds the processed text chunk to the RAG message, wrapped in
     * markdown.</p>
     */
    @Override
    public void populateRag(RagMessage ragMessage) throws Exception {
        String content = viewport.getVisibleContent();
        ragMessage.addTextPart("```\n" + (content != null ? content : "") + "\n```");
    }

    /**
     * {@inheritDoc}
     * <p>
     * Returns the processed text for system instruction injection.</p>
     */
    @Override
    public List<String> getInstructions() throws Exception {
        String content = viewport.getVisibleContent();
        return Collections.singletonList("```\n" + (content != null ? content : "") + "\n```");
    }

    @Override
    public String getHeader() {
        return super.getHeader() + "\nViewPort: " + viewport;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String toString() {
        return viewport.toString();
    }

    /**
     * Checks whether the text viewport is currently truncated in prompt view
     * (i.e. not in Full View mode, or has start offset / page size bounds active).
     *
     * @return true if the viewport content in prompt is truncated.
     */
    public boolean isTruncated() {
        TextViewportSettings settings = viewport.getSettings();
        if (settings.isFullView()) {
            return false;
        }
        return viewport.getTotalChars() > settings.getPageSizeInChars()
                || settings.getStartChar() > 0
                || (settings.getGrepPattern() != null && !settings.getGrepPattern().isBlank())
                || settings.isTail();
    }

    /**
     * Calculates the percentage of the total resource content currently visible in prompt.
     *
     * @return The percentage between 0.0 and 100.0.
     */
    public double getVisiblePercentage() {
        long total = viewport.getTotalChars();
        if (total <= 0) {
            return 100.0;
        }
        String visible = viewport.getVisibleContent();
        long visibleChars = (visible != null) ? visible.length() : 0;
        double pct = (visibleChars * 100.0) / total;
        return Math.min(100.0, pct);
    }

    /**
     * {@inheritDoc}
     * <p>
     * Performs a lazy, model-specific token calculation of the active viewport
     * content, caching the result to prevent redundant, CPU-intensive
     * tokenization.
     * </p>
     */
    @Override
    public int getTokenCount() {
        if (tokenCount == null) {
            AbstractModel model = getOwner() != null ? getOwner().getSelectedModel() : null;
            if (model == null) {
                return 0;
            }
            String content = viewport.getVisibleContent();
            tokenCount = model.countTokens(content != null ? content : "") + 20;
        }
        return tokenCount;
    }
}
