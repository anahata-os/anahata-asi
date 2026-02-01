/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.resource;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;
import lombok.Getter;
import lombok.NonNull;
import lombok.RequiredArgsConstructor;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.chat.Chat;
import uno.anahata.asi.context.ContextProvider;
import uno.anahata.asi.model.core.BasicPropertyChangeSource;
import uno.anahata.asi.model.core.RagMessage;
import uno.anahata.asi.model.core.Rebindable;
import uno.anahata.asi.model.core.TextPart;
import uno.anahata.asi.model.resource.AbstractPathResource;
import uno.anahata.asi.model.resource.AbstractResource;

/**
 * The central, unified manager for all V2 managed resources. This class acts as
 * the pure backend container for all {@link AbstractResource} instances,
 * preserving the order of registration.
 * <p>
 * This class implements {@link ContextProvider}, allowing it to participate 
 * directly in the hierarchical context injection process. It acts as a parent 
 * node for all registered {@link AbstractResource} instances.
 * </p>
 *
 * @author anahata-ai
 */
@Slf4j
@Getter
@RequiredArgsConstructor
public class ResourceManager extends BasicPropertyChangeSource implements Rebindable, ContextProvider {

    /** The parent chat session. */
    private final Chat chat;

    /**
     * A map of all tracked resources, keyed by their unique resource ID.
     * Uses a LinkedHashMap to preserve registration order. 
     * Access is manually synchronized to avoid Kryo serialization issues with JDK synchronized wrappers.
     */
    private final Map<String, AbstractResource<?, ?>> resources = new LinkedHashMap<>();

    /** Whether this manager is currently providing context augmentation. */
    @Setter
    private boolean providing = true;

    /**
     * Registers a new resource, making it managed by the framework.
     * Fires a property change event for the "resources" property.
     *
     * @param resource The resource to register.
     */
    public void register(@NonNull AbstractResource<?, ?> resource) {
        synchronized (resources) {
            resources.put(resource.getId(), resource);
        }
        log.info("Registered resource: {} ({})", resource.getName(), resource.getId());
        propertyChangeSupport.firePropertyChange("resources", null, getResources());
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
    @SuppressWarnings("unchecked")
    public <T extends AbstractResource<?, ?>> T getResource(String id) throws IllegalArgumentException {
        synchronized (resources) {
            if (!resources.containsKey(id)) {
                throw new IllegalArgumentException("Resource not registered: " + id);
            }
            return (T) resources.get(id);
        }
    }

    /**
     * Unregisters a resource, removing it from framework management.
     * Fires a property change event for the "resources" property.
     *
     * @param resourceId The ID of the resource to unregister.
     * @return The unregistered resource, or null if it was not found.
     */
    public AbstractResource<?, ?> unregister(@NonNull String resourceId) {
        AbstractResource<?, ?> removed;
        synchronized (resources) {
            removed = resources.remove(resourceId);
        }
        if (removed != null) {
            log.info("Unregistered resource: {}", resourceId);
            removed.dispose();
            propertyChangeSupport.firePropertyChange("resources", null, getResources());
        }
        return removed;
    }

    /**
     * Gets an unmodifiable view of all currently managed resources, preserving
     * registration order.
     *
     * @return A collection of all managed resources.
     */
    public Collection<AbstractResource<?, ?>> getResources() {
        synchronized (resources) {
            return Collections.unmodifiableList(new ArrayList<>(resources.values()));
        }
    }

    /**
     * Finds a managed resource by its absolute file path.
     *
     * @param path The path to search for.
     * @return An Optional containing the resource if found, otherwise empty.
     */
    public Optional<? extends AbstractPathResource<?, ?>> findByPath(String path) {
        return getResources().stream()
                .filter(r -> r instanceof AbstractPathResource)
                .map(r -> (AbstractPathResource<?, ?>) r)
                .filter(r -> r.getPath().equals(path))
                .findFirst();
    }

    /** {@inheritDoc} */
    @Override
    public void rebind() {
        super.rebind();
        log.info("Rebinding ResourceManager. Managed resources: {}", resources.size());
        // Automated rebind propagation is handled by RebindableWrapperSerializer.
    }

    // --- ContextProvider Implementation ---

    /** {@inheritDoc} */
    @Override
    public String getId() {
        return "resources";
    }

    /** {@inheritDoc} */
    @Override
    public String getName() {
        return "Resources";
    }

    /** {@inheritDoc} */
    @Override
    public String getDescription() {
        return "Managed stateful resources (files, screen devices, etc.)";
    }

    /** {@inheritDoc} */
    @Override
    public boolean isProviding() {
        return providing;
    }

    /** {@inheritDoc} */
    @Override
    public void populateMessage(RagMessage ragMessage) throws Exception {
        List<AbstractResource<?, ?>> disabled = getResources().stream()
                .filter(r -> !r.isEffectivelyProviding())
                .collect(Collectors.toList());
        
        if (!disabled.isEmpty()) {
            StringBuilder sb = new StringBuilder("**Disabled Resources** (Registered but not effectively providing context):\n");
            for (AbstractResource<?, ?> r : disabled) {
                sb.append("\n").append(r.getHeader());
            }
            sb.append("\nYou can suggest the user to enable these resources if you need them.");
            new TextPart(ragMessage, sb.toString());
        }
    }

    /** {@inheritDoc} */
    @Override
    public List<ContextProvider> getChildrenProviders() {
        synchronized (resources) {
            return resources.values().stream()
                    .map(r -> (ContextProvider) r)
                    .collect(Collectors.toList());
        }
    }
}
