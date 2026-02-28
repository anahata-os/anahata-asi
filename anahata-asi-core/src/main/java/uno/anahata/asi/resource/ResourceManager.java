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
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.context.ContextProvider;
import uno.anahata.asi.model.core.BasicPropertyChangeSource;
import uno.anahata.asi.model.core.RagMessage;
import uno.anahata.asi.model.core.Rebindable;
import uno.anahata.asi.model.resource.AbstractPathResource;
import uno.anahata.asi.model.resource.AbstractResource;

/**
 * The central, unified manager for all V2 managed resources.
 * <p>
 * This class acts as the pure backend container for all {@link AbstractResource} 
 * instances, preserving the order of registration. It implements {@link ContextProvider}, 
 * allowing it to participate directly in the hierarchical context injection process.
 * </p>
 *
 * @author anahata-ai
 */
@Slf4j
@Getter
@RequiredArgsConstructor
public class ResourceManager extends BasicPropertyChangeSource implements Rebindable, ContextProvider {

    /** The parent agi session. */
    private final Agi agi;

    /**
     * A map of all tracked resources, keyed by their unique resource ID.
     * <p>
     * Uses a LinkedHashMap to preserve registration order. Access is manually 
     * synchronized to ensure thread safety across the toolkit and UI layers 
     * while remaining compatible with Kryo serialization.
     * </p>
     */
    private final Map<String, AbstractResource<?, ?>> resources = new LinkedHashMap<>();

    /** Whether this manager is currently providing context augmentation. */
    @Setter
    private boolean providing = true;

    /**
     * Registers a new resource, making it managed by the framework.
     * <p>
     * This is a convenience wrapper around {@link #registerAll(Collection)}. 
     * It ensures that even single registrations follow the unified event-firing 
     * path to maintain UI consistency.
     * </p>
     *
     * @param resource The resource to register. Must not be null.
     */
    public void register(@NonNull AbstractResource<?, ?> resource) {
        registerAll(Collections.singletonList(resource));
    }

    /**
     * Registers multiple resources in a single atomic operation.
     * <p>
     * This method adds resources to the internal map within a synchronized block 
     * and fires exactly ONE property change event for the "resources" property. 
     * This prevents UI "event storms" where the tree table would otherwise 
     * rebuild for every individual resource.
     * </p>
     * 
     * @param toRegister The collection of resources to register. Must not be null.
     */
    public void registerAll(@NonNull Collection<AbstractResource<?, ?>> toRegister) {
        if (toRegister.isEmpty()) {
            return;
        }
        
        synchronized (resources) {
            for (AbstractResource<?, ?> res : toRegister) {
                resources.put(res.getId(), res);
                log.info("Registered resource: {} ({})", res.getName(), res.getId());
            }
        }
        propertyChangeSupport.firePropertyChange("resources", null, getResources());
    }

    /**
     * Gets a resource for a given ID.
     * <p>
     * Performs a type-safe lookup in the internal map. Uses manual synchronization 
     * to ensure visibility across threads.
     * </p>
     * 
     * @param <T> The specific type of the resource.
     * @param id The unique resource identifier.
     * @return The resource instance.
     * @throws IllegalArgumentException if no resource for that id is registered.
     */
    @SuppressWarnings("unchecked")
    public <T extends AbstractResource<?, ?>> T getResource(String id) throws IllegalArgumentException {
        synchronized (resources) {
            AbstractResource<?, ?> res = resources.get(id);
            if (res == null) {
                throw new IllegalArgumentException("Resource not registered: " + id);
            }
            return (T) res;
        }
    }

    /**
     * Unregisters a resource, removing it from framework management.
     * <p>
     * This is a convenience wrapper around {@link #unregisterAll(Collection)}. 
     * It returns the removed instance if found.
     * </p>
     * 
     * @param resourceId The ID of the resource to unregister. Must not be null.
     * @return The unregistered resource instance, or null if it was not found.
     */
    public AbstractResource<?, ?> unregister(@NonNull String resourceId) {
        List<AbstractResource<?, ?>> removed = unregisterAll(Collections.singletonList(resourceId));
        return removed.isEmpty() ? null : removed.get(0);
    }
    
    /**
     * Unregisters multiple resources in a single atomic operation.
     * <p>
     * This is the authoritative endpoint for resource removal. It removes 
     * resources from the internal map, calls {@link AbstractResource#dispose()} 
     * for cleanup, and fires exactly one property change event.
     * </p>
     * 
     * @param resourceIds The collection of IDs to unregister. Must not be null.
     * @return The list of {@link AbstractResource} instances that were actually removed.
     */
    public List<AbstractResource<?, ?>> unregisterAll(@NonNull Collection<String> resourceIds) {
        List<AbstractResource<?, ?>> removedResources = new ArrayList<>();
        synchronized (resources) {
            for (String id : resourceIds) {
                AbstractResource<?, ?> res = resources.remove(id);
                if (res != null) {
                    removedResources.add(res);
                }
            }
        }
        
        if (!removedResources.isEmpty()) {
            for (AbstractResource<?, ?> res : removedResources) {
                log.info("Unregistered resource: {} ({})", res.getName(), res.getId());
                res.dispose();
            }
            propertyChangeSupport.firePropertyChange("resources", null, getResources());
        }
        return removedResources;
    }

    /**
     * Gets an unmodifiable view of all currently managed resources.
     * <p>
     * Creates a defensive copy of the map values while holding the internal 
     * lock to ensure thread safety during iteration. Registration order 
     * is preserved.
     * </p>
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
     * <p>
     * Filters the current resource list for instances of {@link AbstractPathResource} 
     * and performs an absolute path comparison.
     * </p>
     * 
     * @param path The absolute path to search for.
     * @return An Optional containing the resource if found, otherwise empty.
     */
    public Optional<? extends AbstractPathResource<?>> findByPath(String path) {
        return getResources().stream()
                .filter(r -> r instanceof AbstractPathResource)
                .map(r -> (AbstractPathResource<?>) r)
                .filter(r -> r.getPath().equals(path))
                .findFirst();
    }

    /**
     * Rebinds the manager after deserialization.
     * <p>
     * This method is called by the framework to restore transient state or 
     * reconnect listeners after a session has been loaded from disk.
     * </p>
     */
    @Override
    public void rebind() {
        super.rebind();
        log.info("Rebinding ResourceManager. Managed resources: {}", resources.size());
    }

    /**
     * Returns the unique provider ID.
     * <p>
     * Returns the constant string "resources", which serves as the key for 
     * this manager in the hierarchical context tree.
     * </p>
     * 
     * @return The constant ID "resources".
     */
    @Override
    public String getId() {
        return "resources";
    }

    /**
     * Returns the human-readable provider name.
     * <p>
     * Returns "Resources", used for display in the UI and as a label in 
     * generated RAG messages.
     * </p>
     * 
     * @return The constant name "Resources".
     */
    @Override
    public String getName() {
        return "Resources";
    }

    /**
     * Returns a description of the manager's role in context.
     * <p>
     * Explains the manager's responsibility for stateful resources like 
     * files or devices within the AI context.
     * </p>
     * 
     * @return A descriptive string for the AI.
     */
    @Override
    public String getDescription() {
        return "Managed stateful resources (files, screen devices, etc.)";
    }

    /**
     * Checks if the manager is providing context to the RAG message.
     * <p>
     * Returns the value of the internal 'providing' flag. If false, this 
     * manager and all its children are excluded from context injection.
     * </p>
     * 
     * @return true if the manager is enabled.
     */
    @Override
    public boolean isProviding() {
        return providing;
    }

    /**
     * Populates the RAG message with a summary of disabled resources.
     * <p>
     * Identifies resources that are registered but have their individual 
     * 'providing' flag set to false. It injects their headers into the RAG 
     * message so the model knows they are available but currently silent.
     * </p>
     * 
     * @param ragMessage The target RAG message.
     * @throws Exception if population fails.
     */
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
            ragMessage.addTextPart(sb.toString());
        }
    }

    /**
     * Returns the managed resources as child context providers.
     * <p>
     * Maps the values of the internal resource map to the ContextProvider 
     * interface. This allows the ContextManager to recursively discover and 
     * inject individual resources.
     * </p>
     * 
     * @return A list of all registered resources as context providers.
     */
    @Override
    public List<ContextProvider> getChildrenProviders() {
        synchronized (resources) {
            return resources.values().stream()
                    .map(r -> (ContextProvider) r)
                    .collect(Collectors.toList());
        }
    }
}
