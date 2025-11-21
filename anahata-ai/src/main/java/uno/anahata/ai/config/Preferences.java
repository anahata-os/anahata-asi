/*
 * Copyright 2025 Anahata.
 *
 * Licensed under the Anahata Software License (ASL) V2.0;
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      https://github.com/pablo-anahata/anahata-ai-parent/blob/main/LICENSE
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * Força Barça!
 */
package uno.anahata.ai.config;

import uno.anahata.ai.model.tool.ToolPreferences;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.ai.AiConfig;
import uno.anahata.ai.internal.kryo.KryoUtils;

/**
 * A unified, serializable POJO that acts as a container for all user-configured preferences.
 * This is the root object that gets persisted to disk and is responsible for its own persistence.
 */
@Getter
@Setter
@Slf4j
public class Preferences {
    private static final String PREFERENCES_FILE_NAME = "preferences.kryo";

    /**
     * Holds all preferences related to tool execution permissions.
     */
    private ToolPreferences toolPreferences = new ToolPreferences();

    // Other preference categories (e.g., ModelPreferences, UIPreferences) can be added here in the future.

    /**
     * Saves the current state of this preferences object to disk.
     *
     * @param config The application-wide configuration, used to determine the correct storage location.
     */
    public synchronized void save(AiConfig config) {
        Path preferencesFile = getPreferencesFile(config);
        try {
            Files.createDirectories(preferencesFile.getParent());
            log.info("Saving preferences to {}", preferencesFile);
            try (OutputStream os = Files.newOutputStream(preferencesFile)) {
                byte[] bytes = KryoUtils.serialize(this);
                os.write(bytes);
            }
        } catch (IOException e) {
            log.error("Error saving preferences to {}", preferencesFile, e);
        }
    }

    /**
     * Loads the preferences for a given host application from disk.
     *
     * @param config The application-wide configuration.
     * @return The loaded Preferences object, or a new empty one if not found or on error.
     */
    public static synchronized Preferences load(AiConfig config) {
        Path preferencesFile = getPreferencesFile(config);
        if (Files.exists(preferencesFile)) {
            log.info("Loading preferences from {}", preferencesFile);
            try (InputStream is = Files.newInputStream(preferencesFile)) {
                byte[] bytes = is.readAllBytes();
                return KryoUtils.deserialize(bytes, Preferences.class);
            } catch (Exception e) {
                log.error("Error loading preferences from {}", preferencesFile, e);
            }
        } else {
            log.info("Preferences file not found at {}, creating new preferences.", preferencesFile);
        }
        return new Preferences();
    }

    private static Path getPreferencesFile(AiConfig config) {
        Path appWorkDir = config.getAppDir();
        return appWorkDir.resolve(PREFERENCES_FILE_NAME);
    }
}
