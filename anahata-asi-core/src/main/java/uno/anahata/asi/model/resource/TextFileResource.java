/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.model.resource;

import io.swagger.v3.oas.annotations.media.Schema;
import java.nio.file.Files;
import java.nio.file.Path;
import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.chat.Chat;
import uno.anahata.asi.internal.TokenizerUtils;
import uno.anahata.asi.model.core.RagMessage;
import uno.anahata.asi.model.core.TextPart;
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
public class TextFileResource extends AbstractPathResource<String, String> {

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
        super(manager);
        this.setResource(path); 
        this.setPath(path.toAbsolutePath().toString());
        this.setName(path.getFileName().toString());
        this.viewport = viewport;
        reload(); 
    }
    
    /** {@inheritDoc} */
    @Override
    public void reload() throws Exception {
        log.info("Reloading text file resource: {}", getPath());
        String content = Files.readString(getResource());
        this.setLoadLastModified(getCurrentLastModified());
        
        // Process the new content through the existing viewport
        this.viewport.process(content);
        
        // Cache only the processed text. The header is generated dynamically.
        this.cache = viewport.getProcessedText();
    }

    /** {@inheritDoc} */
    @Override
    protected void populateFromCache(RagMessage rm) {
        StringBuilder sb = new StringBuilder();
        sb.append(getHeader());
        sb.append("```\n");
        sb.append(cache);
        sb.append("\n```");
        new TextPart(rm, sb.toString());
    }
    
    /** {@inheritDoc} */
    @Override
    public String getContentType() {
        return "text";
    }

    /** {@inheritDoc} */
    @Override
    public Integer getTurnsRemaining() {
        return null; // Permanent
    }

    /** {@inheritDoc} */
    @Override
    public int getTokenCount() {
        int headerTokens = TokenizerUtils.countTokens(getHeader());
        int contentTokens = TokenizerUtils.countTokens(cache);
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
