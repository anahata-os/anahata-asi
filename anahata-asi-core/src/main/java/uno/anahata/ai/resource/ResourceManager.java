package uno.anahata.ai.resource;

import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import lombok.NonNull;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.ai.chat.Chat;
import uno.anahata.ai.context.AbstractContextProvider;
import uno.anahata.ai.context.ContextPosition;
import uno.anahata.ai.context.ContextProvider;
import uno.anahata.ai.model.core.RagMessage;
import uno.anahata.ai.model.core.TextPart;
import uno.anahata.ai.model.resource.AbstractResource;

/**
 * The central, unified manager for all V2 managed resources.
 * This class acts as the pure backend container for all {@link AbstractResource}
 * instances, preserving the order of registration.
 *
 * @author anahata-ai
 */
@Slf4j
public class ResourceManager {

    
    /**
     * A map of all tracked resources by resource id.
     */
    private final Map<String, AbstractResource> resources = Collections.synchronizedMap(new LinkedHashMap<>());

    /**
     * Registers a new resource, making it managed by the framework.
     * @param resource The resource to register.
     */
    public void register(@NonNull AbstractResource resource) {
        resources.put(resource.getId(), resource);
    }
    
    /**
     * Gets a resource for a given type.
     * 
     * @param <T> the type of the resource
     * @param id thre resource id
     * @return the resource 
     * @throws IllegalArgumentException - If no resource for that id is registered.
     */    
    public <T extends AbstractResource> T getResource(String id) throws IllegalArgumentException{
        if (!resources.containsKey(id)) {
            throw new IllegalArgumentException("Resource not registered: " + id);
        }
        return (T) resources.get(id);
    }

    /**
     * Unregisters a resource, removing it from framework management.
     * @param resourceId The ID of the resource to unregister.
     * @return The unregistered resource, or null if it was not found.
     */
    public AbstractResource unregister(@NonNull String resourceId) {
        return resources.remove(resourceId);
    }
    
    /**
     * Gets an unmodifiable view of all currently managed resources, preserving registration order.
     * @return A collection of all managed resources.
     */
    public Collection<AbstractResource> getResources() {
        return Collections.unmodifiableCollection(resources.values());
    }


}
