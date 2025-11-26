package uno.anahata.ai.tool.files;

import java.nio.file.Paths;
import java.util.Optional;
import uno.anahata.ai.model.resource.AbstractPathResource;
import uno.anahata.ai.model.resource.TextFileResource;
import uno.anahata.ai.tool.AIToolParam;
import uno.anahata.ai.tool.AiTool;
import uno.anahata.ai.tool.AiToolException;
import uno.anahata.ai.tool.AiToolkit;
import uno.anahata.ai.tool.AbstractJavaTool;

/**
 * The definitive V2 toolkit for interacting with file-based resources.
 * @author anahata-ai
 */
@AiToolkit("A toolkit for loading and managing file-based resources.")
public class Files extends AbstractJavaTool {

    /**
     * Loads a text file into the context as a managed resource. The tool's response
     * is ephemeral and will be pruned from the context on the next turn.
     *
     * @param path The absolute path to the text file.
     * @return The newly created TextFileResource.
     * @throws Exception if the file does not exist, is already loaded, or an I/O error occurs.
     */
    @AiTool(value = "Loads a text file into the context as a managed resource.", retention = 0)
    public TextFileResource loadTextFile(
            @AIToolParam("The absolute path to the text file.") String path) throws Exception {
        
        if (findByPath(path).isPresent()) {
            throw new AiToolException("Resource already loaded for path: " + path);
        }
        
        if (!java.nio.file.Files.exists(Paths.get(path))) {
            throw new AiToolException("File not found: " + path);
        }
        
        TextFileResource resource = new TextFileResource(Paths.get(path));
        getResourceManager().register(resource);
        log("Successfully loaded and registered text file: " + path);
        return resource;
    }
    
    /**
     * Finds a managed resource by its absolute file path. This is a private helper
     * method that encapsulates the logic specific to this toolkit.
     *
     * @param path The path to search for.
     * @return An Optional containing the resource if found, otherwise empty.
     */
    private Optional<AbstractPathResource> findByPath(String path) {
        return getResourceManager().getResources().stream()
            .filter(r -> r instanceof AbstractPathResource)
            .map(r -> (AbstractPathResource) r)
            .filter(r -> r.getPath().equals(path))
            .findFirst();
    }
}
