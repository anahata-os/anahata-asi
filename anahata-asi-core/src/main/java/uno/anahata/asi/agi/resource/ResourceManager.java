/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.agi.resource;

import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
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
import uno.anahata.asi.agi.context.ContextProvider;
import uno.anahata.asi.agi.event.BasicPropertyChangeSource;
import uno.anahata.asi.agi.message.RagMessage;
import uno.anahata.asi.persistence.Rebindable;
import uno.anahata.asi.agi.resource.handle.ResourceHandle;

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
public class ResourceManager extends BasicPropertyChangeSource implements Rebindable, ContextProvider {

    /** The parent agi session. */
    private final Agi agi;

    /** Registry of V2 resources, keyed by UUID. */
    private final Map<String, Resource> resources = new LinkedHashMap<>();

    /** Whether this manager is currently providing context augmentation. */
    @Setter
    private boolean providing = true;

    /**
     * Registers a new V2 resource, making it managed by the framework.
     * 
     * @param resource The resource to register. Must not be null.
     * @param registeredBy A string describing who registered the resource.
     */
    public void register(@NonNull Resource resource, String registeredBy) {
        registerAll(Collections.singletonList(resource), registeredBy);
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
     * @param registeredBy A string describing who registered the resources.
     */
    public void registerAll(@NonNull Collection<Resource> toRegister, String registeredBy) {
        if (toRegister.isEmpty()) {
            return;
        }

        List<Resource> added = new ArrayList<>();
        synchronized (resources) {
            for (Resource res : toRegister) {
                String uri = res.getHandle().getUri().toString();
                if (findByUri(uri).isEmpty()) {
                    
                    // Authoritative Registration Metadata
                    res.setRegistrationTime(System.currentTimeMillis());
                    res.setDescription(registeredBy);

                    resources.put(res.getId(), res);
                    added.add(res);
                    log.info("Registered V2 resource: {} ({}) [By: {}]", res.getName(), res.getId(), registeredBy);
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
     * @param the Resource unregistered or null
     */
    public Resource unregister(@NonNull String id) {
        Resource res;
        synchronized (resources) {
            res = resources.remove(id);
        }
        if (res != null) {
            res.dispose();
            log.info("Unregistered V2 resource: {} ({})", res.getName(), id);
            propertyChangeSupport.firePropertyChange("resources", null, getResourcesList());
        }
        return res;
    }

    /**
     * Returns an unmodifiable list of currently managed resources.
     * @return an unmodifiable list of all managed resources.
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
        String search = uri.replace("file:/", "file:///").replace("file:////", "file:///");
        synchronized (resources) {
            return resources.values().stream()
                     .filter(r -> {
                        String rUri = r.getHandle().getUri().toString();
                        return rUri.replace("file:/", "file:///").replace("file:////", "file:///").equals(search);
                    })
                    .findFirst();
        }
    }

    /**
     * Finds a managed resource by its physical path.
     * @param path The absolute path to search for.
     * @return Optional containing the resource if found.
     */
    public Optional<Resource> findByPath(@NonNull String path) {
        return findByUri(Paths.get(path).toUri().toString());
    }

    /**
     * Registers multiple filesystem paths as managed resources.
     * 
     * @param paths The list of paths to register.
     * @param registeredBy A description of the origin.
     * @return The list of created resources.
     */
    public List<Resource> registerPaths(@NonNull List<Path> paths, String registeredBy) {
        List<Resource> toRegister = new ArrayList<>();
        for (Path p : paths) {
            ResourceHandle handle = agi.getConfig().createResourceHandle(p.toUri());
            Resource resource = new Resource(handle);
            toRegister.add(resource);
        }
        registerAll(toRegister, registeredBy);
        return toRegister;
    }

    /**
     * Registers a pre-created resource handle directly.
     * <p>
     * <b>Technical Purity:</b> This convenience method avoids redundant 
     * handle creation cycles when a host-aware toolkit (like NetBeans) 
     * already has an authoritative object (like a FileObject).
     * </p>
     * 
     * @param handle The source handle.
     * @param registeredBy A description of the origin.
     * @return The created and registered Resource orchestrator.
     */
    public Resource registerHandle(@NonNull ResourceHandle handle, String registeredBy) {
        Resource res = new Resource(handle);
        register(res, registeredBy);
        return res;
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
        log.info("Rebinding ResourceManager for session: {}", agi.getConfig().getSessionId());
        super.rebind();
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
