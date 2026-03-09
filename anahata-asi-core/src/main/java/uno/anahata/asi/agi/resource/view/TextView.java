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
import uno.anahata.asi.internal.TokenizerUtils;
import uno.anahata.asi.agi.message.RagMessage;
import uno.anahata.asi.persistence.Rebindable;
import uno.anahata.asi.agi.resource.Resource;

/**
 * A resource view that interprets content as plain text.
 * <p>
 * This view integrates the V2 {@link TextViewport} for high-fidelity 
 * streaming of large files and implements self-aware reactivity to 
 * viewport settings changes.
 * </p>
 */
@Slf4j
@Getter
@Setter
@NoArgsConstructor
public class TextView extends AbstractResourceView implements Rebindable {

    /** The viewport engine for processing text. */
    private final TextViewport viewport = new TextViewport();

    /** 
     * Persistent listener for settings changes to trigger interpretation reloads. 
     * We keep a field reference to ensure single registration in the PropertyChangeSupport.
     */
    private transient PropertyChangeListener settingsListener;

    /**
     * Constructs a TextView and links it to its parent resource.
     * @param owner The owning resource.
     */
    public TextView(Resource owner) {
        this.owner = owner;
        setupListener();
    }

    /**
     * Constructs a TextView with specific initial settings.
     * @param owner The owning resource.
     * @param settings The initial viewport configuration.
     */
    public TextView(Resource owner, TextViewportSettings settings) {
        this.owner = owner;
        this.viewport.setSettings(settings);
        setupListener();
    }

    /**
     * Authoritatively registers the persistent listener on the viewport settings.
     */
    private void setupListener() {
        
        TextViewportSettings settings = viewport.getSettings();
        // Technical Purity: remove before add to prevent double-registration
        if (settingsListener == null) {
            settingsListener = evt -> markDirty();
        } else {
            settings.removePropertyChangeListener(settingsListener);
        }
        settings.addPropertyChangeListener(settingsListener);
    }

    /** 
     * {@inheritDoc} 
     * <p>Implementation details: Re-establishes the reactive listener 
     * for viewport settings after deserialization.</p>
     */
    @Override
    public void rebind() {
        setupListener();
    }

    /** 
     * {@inheritDoc} 
     * <p>Performs memory-efficient stream processing using the viewport engine.</p>
     */
    @Override
    public void reload() throws Exception {
        ResourceHandle handle = owner.getHandle();
        log.debug("Reloading TextView (Streaming) for: {}", handle.getUri());
        viewport.process(handle);
    }

    /** 
     * {@inheritDoc} 
     * <p>Adds the processed text chunk to the RAG message, wrapped in markdown.</p>
     */
    @Override
    public void populateRag(RagMessage ragMessage) throws Exception {
        String content = viewport.getVisibleContent();
        ragMessage.addTextPart("```\n" + (content != null ? content : "") + "\n```");
    }

    /** 
     * {@inheritDoc} 
     * <p>Returns the processed text for system instruction injection.</p>
     */
    @Override
    public List<String> getInstructions() throws Exception {
        String content = viewport.getVisibleContent();
        return Collections.singletonList("```\n" + (content != null ? content : "") + "\n```");
    }

    /** 
     * {@inheritDoc} 
     */
    @Override
    public int getTokenCount() {
        String content = viewport.getVisibleContent();
        return TokenizerUtils.countTokens(content != null ? content : "") + 20;
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
}
