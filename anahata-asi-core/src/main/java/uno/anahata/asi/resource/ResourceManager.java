package uno.anahata.asi.resource;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import lombok.NonNull;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.chat.Chat;
import uno.anahata.asi.tool.AnahataToolkit;
import uno.anahata.asi.context.ContextPosition;
import uno.anahata.asi.context.ContextProvider;
import uno.anahata.asi.model.core.RagMessage;
import uno.anahata.asi.model.core.Rebindable;
import uno.anahata.asi.model.core.TextPart;
import uno.anahata.asi.model.resource.AbstractPathResource;
import uno.anahata.asi.model.resource.AbstractResource;

/**
 * The central, unified manager for all V2 managed resources. This class acts as
 * the pure backend container for all {@link AbstractResource} instances,
 * preserving the order of registration.
 *
 * @author anahata-ai
 */
@Slf4j
public class ResourceManager implements Rebindable {

    /**
     * A map of all tracked resources, keyed by their unique resource ID.
     * Uses a LinkedHashMap to preserve registration order. 
     * Access is manually synchronized to avoid Kryo serialization issues with JDK synchronized wrappers.
     */
    private final Map<String, AbstractResource> resources = new LinkedHashMap<>();

    /**
     * Registers a new resource, making it managed by the framework.
     *
     * @param resource The resource to register.
     */
    public void register(@NonNull AbstractResource resource) {
        synchronized (resources) {
            resources.put(resource.getId(), resource);
        }
    }

    /**
     * Gets a resource for a given type.
     *
     * @param <T> the type of the resource
     * @param id thre resource id
     * @return the resource
     * @throws IllegalArgumentException - If no resource for that id is
     * registered.
     */
    public <T extends AbstractResource> T getResource(String id) throws IllegalArgumentException {
        synchronized (resources) {
            if (!resources.containsKey(id)) {
                throw new IllegalArgumentException("Resource not registered: " + id);
            }
            return (T) resources.get(id);
        }
    }

    /**
     * Unregisters a resource, removing it from framework management.
     *
     * @param resourceId The ID of the resource to unregister.
     * @return The unregistered resource, or null if it was not found.
     */
    public AbstractResource unregister(@NonNull String resourceId) {
        synchronized (resources) {
            return resources.remove(resourceId);
        }
    }

    /**
     * Gets an unmodifiable view of all currently managed resources, preserving
     * registration order.
     *
     * @return A collection of all managed resources.
     */
    public Collection<AbstractResource> getResources() {
        synchronized (resources) {
            return new ArrayList<>(resources.values());
        }
    }

    /**
     * Finds a managed resource by its absolute file path. This is a private
     * helper method that encapsulates the logic specific to this toolkit.
     *
     * @param path The path to search for.
     * @return An Optional containing the resource if found, otherwise empty.
     */
    public Optional<AbstractPathResource> findByPath(String path) {
        return getResources().stream()
                .filter(r -> r instanceof AbstractPathResource)
                .map(r -> (AbstractPathResource) r)
                .filter(r -> r.getPath().equals(path))
                .findFirst();
    }

    @Override
    public void rebind() {
        log.info("Rebinding ResourceManager. Managed resources: {}", resources.size());
        // Resources themselves might need rebinding if they implement Rebindable,
        // but Kryo's RebindableWrapperSerializer handles the object graph recursively.
    }

}
