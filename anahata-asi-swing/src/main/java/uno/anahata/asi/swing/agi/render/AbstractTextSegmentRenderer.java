/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.agi.render;

import java.util.Objects;
import javax.swing.JComponent;
import lombok.Getter;
import lombok.Setter;
import uno.anahata.asi.swing.agi.AgiPanel;
import uno.anahata.asi.swing.agi.SwingAgiConfig;

/**
 * Abstract base class for rendering individual text segments (e.g., markdown text or code blocks).
 * It manages the component, current content, and provides a mechanism for diff-based updates.
 *
 * @author anahata
 */
@Getter
public abstract class AbstractTextSegmentRenderer {

    /** The parent agi panel. */
    protected final AgiPanel agiPanel;
    /** The agi configuration. */
    protected final SwingAgiConfig agiConfig;

    /** The actual Swing component being rendered. */
    protected JComponent component; 
    /** The raw content for this segment. */
    protected String currentContent; 
    /** The content that was last rendered into the component. */
    protected String lastRenderedContent; 

    /** 
     * Whether the segment is fully closed/complete. 
     * For code blocks, this is true if the closing backticks are present.
     */
    @Setter
    protected boolean closed = true;

    /**
     * Constructs a new AbstractTextSegmentRenderer.
     *
     * @param agiPanel The agi panel instance.
     * @param initialContent The initial raw content for this segment.
     */
    public AbstractTextSegmentRenderer(AgiPanel agiPanel, String initialContent) {
        this.agiPanel = agiPanel;
        this.agiConfig = agiPanel.getAgiConfig();
        this.currentContent = initialContent;
        this.lastRenderedContent = null; // Will be set after the first render
    }

    /**
     * Renders or updates the segment's JComponent based on the current content.
     * This method should be called to ensure the component reflects the latest content.
     * It should handle diffing internally to avoid unnecessary re-creation of components.
     *
     * @return True if a visual update occurred, false otherwise.
     */
    public abstract boolean render();

    /**
     * Updates the current content of this segment. The next call to {@link #render()}
     * will reflect this new content.
     *
     * @param newContent The new raw content for this segment.
     */
    public void updateContent(String newContent) {
        this.currentContent = newContent;
    }

    /**
     * Checks if the current content is different from the last rendered content.
     *
     * @return True if the content has changed, false otherwise.
     */
    protected boolean hasContentChanged() {
        return !Objects.equals(currentContent, lastRenderedContent);
    }

    /**
     * Sets the last rendered content to the current content.
     * This should be called after a successful render operation.
     */
    protected void contentRendered() {
        this.lastRenderedContent = this.currentContent;
    }

    /**
     * Determines if this renderer can handle the given segment descriptor.
     * Subclasses must implement this to specify their compatibility.
     *
     * @param descriptor The {@link TextSegmentDescriptor} to check.
     * @return True if this renderer can handle the descriptor, false otherwise.
     */
    public abstract boolean matches(TextSegmentDescriptor descriptor);
}
