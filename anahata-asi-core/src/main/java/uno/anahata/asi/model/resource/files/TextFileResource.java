/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.model.resource.files;

import uno.anahata.asi.toolkit.files.TextViewport;
import io.swagger.v3.oas.annotations.media.Schema;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.internal.TokenizerUtils;
import uno.anahata.asi.model.core.RagMessage;
import uno.anahata.asi.model.resource.AbstractPathResource;
import uno.anahata.asi.resource.ResourceManager;

/**
 * A concrete resource representing a text file loaded into the context.
 * It is a stateful, permanent resource by default and manages its own viewport.
 * It implements the 'Atomic Reload' architecture for self-healing updates.
 * 
 * @author anahata-ai
 */
@Getter
@Setter
@Slf4j
@Schema(description = "A resource representing a text file")
public class TextFileResource extends AbstractPathResource<String> {

    /** The current viewport settings and processed text for this file. */
    @Schema(description = "The current view port for the text file")
    private TextViewport viewport;

    /**
     * Creates a new TextFileResource with a default viewport.
     * @param manager The parent resource manager.
     * @param path The path to the file.
     * @throws Exception if an I/O error occurs reading from the stream
     */
    public TextFileResource(ResourceManager manager, Path path) throws Exception {
        this(manager, path, new TextViewport());
    }

    /**
     * Creates a new TextFileResource with a custom viewport.
     * @param manager The parent resource manager.
     * @param path The path to the file.
     * @param viewport The custom viewport to use.
     * @throws Exception if an I/O error occurs reading from the stream
     */
    public TextFileResource(ResourceManager manager, Path path, TextViewport viewport) throws Exception {
        super(manager, path.toAbsolutePath().toString());
        this.viewport = viewport;
    }
    
    /** {@inheritDoc} */
    @Override
    public void reload() throws Exception {
        // Optimization: Use streaming for tail or grep to avoid loading huge files into JVM heap
        if (viewport.getSettings().isTail() || (viewport.getSettings().getGrepPattern() != null && !viewport.getSettings().getGrepPattern().isBlank())) {
            log.info("Reloading text resource (streaming): {}", getPath());
            viewport.process(getResource(), getCharset());
            this.setLoadLastModified(getCurrentLastModified());
            this.onContentReloaded(viewport.getProcessedText());
        } else {
            super.reload();
        }
    }

    /**
     * Gets the charset for reading the file. Defaults to platform default.
     * NetBeans implementation overrides this to use IDE-specific encoding.
     * 
     * @return The Charset to use.
     */
    protected Charset getCharset() {
        return Charset.defaultCharset();
    }

    /** {@inheritDoc} */
    @Override
    protected String reloadContent() throws Exception {
        return Files.readString(getResource(), getCharset());
    }

    /** {@inheritDoc} */
    @Override
    protected void onContentReloaded(String newContent) {
        // If coming from super.reload(), it's the full text. Process it.
        // If coming from streaming reload(), it's already processed, but we check for logical changes.
        if (newContent != null && !newContent.equals(viewport.getProcessedText())) {
            this.viewport.process(newContent);
        }
        
        String oldCache = this.cache;
        this.cache = viewport.getProcessedText();
        if (oldCache != cache) {
            propertyChangeSupport.firePropertyChange("cache", oldCache, cache);
        }
    }

    /** {@inheritDoc} */
    @Override
    protected void populateFromCache(RagMessage rm) {
        StringBuilder sb = new StringBuilder();
        sb.append("```\n");
        sb.append(cache != null ? cache : "");
        sb.append("\n```");
        rm.addTextPart(sb.toString());
    }
    
    /** {@inheritDoc} */
    @Override
    public String getContentType() {
        return "text";
    }

    /** {@inheritDoc} */
    @Override
    public int getTokenCount() {
        int headerTokens = TokenizerUtils.countTokens(getHeader());
        int contentTokens = TokenizerUtils.countTokens(cache != null ? cache : "");
        return headerTokens + contentTokens + 10; // +10 for markdown formatting
    }

    /** {@inheritDoc} */
    @Override
    public String getHeader() {
        StringBuilder sb = new StringBuilder(super.getHeader());
        if (viewport != null) {
            sb.append(viewport.toString());
            sb.append("\n");
        }
        return sb.toString();
    }
}
