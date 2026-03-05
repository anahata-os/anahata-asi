/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.resource.v2;

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

/**
 * The next-generation V2 Resource Manager.
 * <p>
 * This manager is URI-centric and capability-based. It orchestrates V2 
 * {@link Resource} instances, providing a reactive and unified context source 
 * for the AI model. It fires property change events to maintain UI synchronization.
 * </p>
 */
@Slf4j
@Getter
@RequiredArgsConstructor
public class ResourceManager2 extends BasicPropertyChangeSource implements Rebindable, ContextProvider {

    /** The parent agi session. */
    private final Agi agi;

    /** Registry of V2 resources, keyed by UUID. */
    private final Map<String, Resource> resources = new LinkedHashMap<>();

    /** Whether this manager is currently providing context augmentation. */
    @Setter
    private boolean providing = true;

    /**
     * Registers a new V2 resource, making it managed by the framework.
     * <p>
     * This is a convenience wrapper around {@link #registerAll(Collection)}. 
     * </p>
     * @param resource The resource to register. Must not be null.
     */
    public void register(@NonNull Resource resource) {
        registerAll(Collections.singletonList(resource));
    }

    /**
     * Registers multiple resources in a single atomic operation.
     * <p>
     * This method filters out any resources whose URI is already registered 
     * to prevent duplicates. It performs the update within a synchronized block 
     * and fires exactly ONE property change event to maintain UI performance.
     * </p>
     * 
     * @param toRegister The collection of resources to register. Must not be null.
     */
    public void registerAll(@NonNull Collection<Resource> toRegister) {
        if (toRegister.isEmpty()) {
            return;
        }

        List<Resource> added = new ArrayList<>();
        synchronized (resources) {
            for (Resource res : toRegister) {
                String uri = res.getHandle().getUri().toString();
                if (findByUri(uri).isEmpty()) {
                    resources.put(res.getId(), res);
                    added.add(res);
                    log.info("Registered V2 resource: {} ({})", res.getName(), res.getId());
                } else {
                    log.debug("Skipping duplicate resource registration for URI: {}", uri);
                }
            }
        }
        
        if (!added.isEmpty()) {
            propertyChangeSupport.firePropertyChange("resources", null, getResourcesList());
        }
    }

    /**
     * Unregisters a resource and cleans up its handle.
     * @param id The resource UUID.
     */
    public void unregister(@NonNull String id) {
        Resource res;
        synchronized (resources) {
            res = resources.remove(id);
        }
        if (res != null) {
            res.dispose();
            log.info("Unregistered V2 resource: {} ({})", res.getName(), id);
            propertyChangeSupport.firePropertyChange("resources", null, getResourcesList());
        }
    }

    /**
     * Returns an unmodifiable list of currently managed resources.
     * @return The resource list.
     */
    public List<Resource> getResourcesList() {
        synchronized (resources) {
            return Collections.unmodifiableList(new ArrayList<>(resources.values()));
        }
    }

    /**
     * Finds a managed resource by its URI.
     * @param uri The URI to search for.
     * @return Optional containing the resource.
     */
    public Optional<Resource> findByUri(@NonNull String uri) {
        synchronized (resources) {
            return resources.values().stream()
                    .filter(r -> r.getHandle().getUri().toString().equals(uri))
                    .findFirst();
        }
    }

    /** {@inheritDoc} */
    @Override
    public List<ContextProvider> getChildrenProviders() {
        synchronized (resources) {
            return new ArrayList<>(resources.values());
        }
    }

    /** {@inheritDoc} */
    @Override
    public void rebind() {
        log.info("Rebinding ResourceManager2 for session: {}", agi.getConfig().getSessionId());
        // Internal link restoration is handled by Kryo + RebindableWrapperSerializer
    }

    /** {@inheritDoc} 
     * Injects a summary of disabled V2 resources into the RAG message.
     */
    @Override
    public void populateMessage(RagMessage ragMessage) throws Exception {
        List<Resource> disabled = getResourcesList().stream()
                .filter(r -> !r.isEffectivelyProviding())
                .collect(Collectors.toList());
        
        if (!disabled.isEmpty()) {
            StringBuilder sb = new StringBuilder("**Disabled Resources (V2)** (Registered but not effectively providing context):\n");
            for (Resource r : disabled) {
                sb.append("\n").append(r.getHeader());
            }
            sb.append("\nYou can suggest the user to enable these resources if you need them.");
            ragMessage.addTextPart(sb.toString());
        }
    }

    /** {@inheritDoc} */
    @Override
    public String getId() {
        return "resources2";
    }

    /** {@inheritDoc} */
    @Override
    public String getName() {
        return "Resources (V2)";
    }

    /** {@inheritDoc} */
    @Override
    public String getDescription() {
        return "Unified URI-centric resource management.";
    }

    /** {@inheritDoc} */
    @Override
    public ContextProvider getParentProvider() {
        return null; // Root level provider
    }
}
