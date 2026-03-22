/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.agi.resources;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;

/**
 * A singleton registry that holds the active {@link ResourceUI} strategy for 
 * the host environment.
 * <p>
 * This allows the Core Swing UI to remain host-agnostic while enabling 
 * specialized modules (like NetBeans) to register their own high-fidelity 
 * resource management logic.
 * </p>
 * 
 * @author anahata
 */
@Slf4j
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class ResourceUiRegistry {

    /** The singleton instance of the registry. */
    private static final ResourceUiRegistry INSTANCE = new ResourceUiRegistry();

    /**
     * Gets the singleton instance.
     * @return The registry instance.
     */
    public static ResourceUiRegistry getInstance() {
        return INSTANCE;
    }

    /** 
     * The active UI strategy provider. 
     * Defaults to null; host applications must register their preferred UI.
     */
    @Getter
    @Setter
    private ResourceUI resourceUI;

}
