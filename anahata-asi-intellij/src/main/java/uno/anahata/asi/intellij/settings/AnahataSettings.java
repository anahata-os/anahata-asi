/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.intellij.settings;

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.components.PersistentStateComponent;
import com.intellij.openapi.components.State;
import com.intellij.openapi.components.Storage;
import com.intellij.util.xmlb.XmlSerializerUtil;
import org.jetbrains.annotations.NotNull;

/**
 * Application-level persisted settings for the Anahata ASI plugin.
 * <p>
 * Holds the defaults applied to newly created sessions (provider and model). Persisted to
 * {@code anahata-asi.xml} in the IDE config directory and edited through {@code AnahataConfigurable}
 * (Settings → Tools → Anahata ASI). {@code IntellijAgiConfig} reads these on session creation.
 * </p>
 *
 * @author anahata
 */
@State(name = "AnahataAsiSettings", storages = @Storage("anahata-asi.xml"))
public final class AnahataSettings implements PersistentStateComponent<AnahataSettings> {

    /**
     * The UUID of the provider applied to new sessions by default.
     */
    public String defaultProviderUuid = "Gemini";

    /**
     * The model id applied to new sessions by default.
     */
    public String defaultModelId = "models/gemini-flash-latest";

    /**
     * Constructs the settings service (instantiated by the platform via its public no-arg constructor).
     */
    public AnahataSettings() {
    }

    /**
     * Returns the application-wide settings instance.
     *
     * @return the singleton settings service.
     */
    public static AnahataSettings getInstance() {
        return ApplicationManager.getApplication().getService(AnahataSettings.class);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public AnahataSettings getState() {
        return this;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Copies the persisted bean fields onto this instance.
     * </p>
     */
    @Override
    public void loadState(@NotNull AnahataSettings state) {
        XmlSerializerUtil.copyBean(state, this);
    }
}
