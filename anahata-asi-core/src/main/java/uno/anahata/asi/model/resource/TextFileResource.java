package uno.anahata.asi.model.resource;

import io.swagger.v3.oas.annotations.media.Schema;
import java.nio.file.Files;
import java.nio.file.Path;
import lombok.Getter;
import lombok.Setter;
import uno.anahata.asi.model.core.RagMessage;
import uno.anahata.asi.model.core.TextPart;

/**
 * A concrete resource representing a text file loaded into the context.
 * It is a stateful, permanent resource by default and manages its own viewport.
 * It implements the 'Atomic Reload' architecture for self-healing updates.
 * 
 * @author anahata-ai
 */
@Getter
@Setter
@Schema(description = "A resource representing a text file")
public class TextFileResource extends AbstractPathResource<String, String> {

    @Schema(description = "The current view port for the text file")
    private TextViewport viewport;

    /**
     * Creates a new TextFileResource with a default viewport.
     * @param path The path to the file.
     * @throws Exception if an I/O error occurs reading from the stream
     */
    public TextFileResource(Path path) throws Exception {
        this(path, new TextViewport());
    }

    /**
     * Creates a new TextFileResource with a custom viewport.
     * @param path The path to the file.
     * @param viewport The custom viewport to use.
     * @throws Exception if an I/O error occurs reading from the stream
     */
    public TextFileResource(Path path, TextViewport viewport) throws Exception {
        this.setResource(path); // Store the Path as the handle
        this.setPath(path.toAbsolutePath().toString());
        this.setName(path.getFileName().toString());
        this.viewport = viewport;
        reload(); // Perform initial atomic load
    }
    
    @Override
    public void reload() throws Exception {
        // Atomic operation: read, process, and cache the view.
        String content = Files.readString(getResource());
        this.setLoadLastModified(getCurrentLastModified());
        
        // Process the new content through the existing viewport
        this.viewport.process(content);
        
        // Render and cache the new TextPart using the hierarchical header
        StringBuilder sb = new StringBuilder();
        sb.append(buildHeader()); // This now builds the complete, rich header
        sb.append("```\n"); // Start of markdown code block
        sb.append(viewport.getProcessedText());
        sb.append("\n```"); // End of markdown code block
        
        this.cache = sb.toString();
    }

    @Override
    protected void populateFromCache(RagMessage rm) {
        new TextPart(rm, cache);
    }
    
    @Override
    public String getContentType() {
        return "text";
    }

    @Override
    public Integer getTurnsRemaining() {
        // This is a stateful, permanent resource by default.
        return null; 
    }

    @Override
    protected String buildHeader() {
        StringBuilder sb = new StringBuilder(super.buildHeader());
        if (viewport != null) {
            sb.append(viewport.toString());
            sb.append("\n");
        }
        return sb.toString();
    }
}