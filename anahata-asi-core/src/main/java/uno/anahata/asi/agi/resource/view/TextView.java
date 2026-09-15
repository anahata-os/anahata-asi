/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.agi.resource.view;

import uno.anahata.asi.agi.resource.handle.ResourceHandle;
import java.util.Collections;
import java.util.List;
import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.message.RagMessage;
import uno.anahata.asi.agi.provider.AbstractModel;
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
        resetTokenCount();
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
     * {@inheritDoc}
     * <p>
     * Evaluates viewport settings to determine if content is truncated,
     * checking full-view status, character limits, offsets, grep, and tailing.
     * </p>
     */
    @Override
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
     * {@inheritDoc}
     * <p>
     * Calculates the exact percentage of characters currently visible in prompt memory.
     * If even a single character is missing from EOF or pagination, ensures the value
     * never rounds up to 100.0%.
     * </p>
     */
    @Override
    public double getVisiblePercentage() {
        if (!isTruncated()) {
            return 100.0;
        }
        long total = viewport.getTotalChars();
        if (total <= 0) {
            return 100.0;
        }
        TextViewportSettings settings = viewport.getSettings();
        long rawVisibleChars;
        if (settings.getGrepPattern() != null && !settings.getGrepPattern().isBlank()) {
            String visible = viewport.getVisibleContent();
            rawVisibleChars = visible != null ? visible.length() : 0;
        } else if (settings.isTail()) {
            String visible = viewport.getVisibleContent();
            rawVisibleChars = visible != null ? visible.length() : 0;
        } else {
            long start = settings.getStartChar();
            long end = Math.min(total, (long) start + settings.getPageSizeInChars());
            rawVisibleChars = Math.max(0, end - start);
        }
        double pct = (rawVisibleChars * 100.0) / total;
        if (rawVisibleChars < total && pct >= 100.0) {
            pct = 99.9;
        }
        return Math.min(99.9, Math.max(0.0, pct));
    }

    /**
     * {@inheritDoc}
     * <p>
     * Returns true if the viewport has processed and cached visible content.
     * </p>
     */
    @Override
    public boolean hasContent() {
        return viewport.getVisibleContent() != null;
    }

    /**
     * Authoritatively retrieves the processed viewport text content for this view,
     * ensuring the owner resource has executed reloadIfNeeded() so that
     * content is guaranteed to be loaded.
     *
     * @return The visible viewport text content.
     * @throws Exception if reading fails.
     */
    public String getContent() throws Exception {
        owner.reloadIfNeeded();
        return viewport.getVisibleContent();
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
            try {
                String content = getContent();
                tokenCount = model.countTokens(content != null ? content : "") + 20;
            } catch (Exception e) {
                log.error("Failed to load text content in getTokenCount for {}", owner.getName(), e);
                tokenCount = 20;
            }
        }
        return tokenCount;
    }
}
