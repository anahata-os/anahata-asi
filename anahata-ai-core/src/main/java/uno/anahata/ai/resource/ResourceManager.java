package uno.anahata.ai.resource;

import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;
import lombok.NonNull;
import uno.anahata.ai.model.resource.AbstractResource;

/**
 * The central, unified manager for all V2 managed resources.
 * This class acts as the pure backend container for all {@link AbstractResource}
 * instances, preserving the order of registration.
 *
 * @author anahata-ai
 */
public class ResourceManager {

    private final Map<String, AbstractResource> resources = Collections.synchronizedMap(new LinkedHashMap<>());

    /**
     * Registers a new resource, making it managed by the framework.
     * @param resource The resource to register.
     */
    public void register(@NonNull AbstractResource resource) {
        resources.put(resource.getId(), resource);
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
