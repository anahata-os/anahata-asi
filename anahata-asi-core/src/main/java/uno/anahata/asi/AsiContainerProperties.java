/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Instant;
import java.time.format.DateTimeParseException;
import java.util.Objects;
import java.util.Optional;
import java.util.Properties;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;

/**
 * Domain entity and I/O manager for the persistent {@code anahata.asi.properties} metadata file
 * stored in each ASI container version directory.
 * <p>
 * This file unambiguously marks a filesystem directory as a genuine Anahata ASI container directory
 * (distinguishing it from non-container directories such as {@code screenshots} or temporary scratchpads)
 * and records vital lifecycle attributes including container version, host application ID, and creation timestamp.
 * </p>
 *
 * @author anahata
 */
@Slf4j
public class AsiContainerProperties {

    /**
     * The standard filename for the container metadata file within a container directory.
     */
    public static final String PROPERTIES_FILE_NAME = "anahata.asi.properties";

    /**
     * The property key storing the container version string (e.g. {@code "1.2.0"}).
     */
    public static final String KEY_VERSION = "version";

    /**
     * The property key storing the host application identifier (e.g. {@code "netbeans"}, {@code "intellij"}, {@code "AsiDesktop"}).
     */
    public static final String KEY_HOST_APPLICATION = "hostApplication";

    /**
     * The property key storing the ISO-8601 creation timestamp when the container directory was initialized.
     */
    public static final String KEY_CREATED = "created";

    /**
     * The underlying Java {@link Properties} store.
     */
    @Getter
    private final Properties properties;

    /**
     * Constructs a new instance wrapping an existing {@link Properties} object.
     *
     * @param properties The non-null properties object.
     */
    public AsiContainerProperties(Properties properties) {
        this.properties = Objects.requireNonNull(properties, "properties cannot be null");
    }

    /**
     * Constructs a new instance with explicit initial metadata values.
     *
     * @param version The clean container version.
     * @param hostApplication The host application ID.
     * @param created The creation timestamp.
     */
    public AsiContainerProperties(String version, String hostApplication, Instant created) {
        this.properties = new Properties();
        if (version != null && !version.isBlank()) {
            this.properties.setProperty(KEY_VERSION, version);
        }
        if (hostApplication != null && !hostApplication.isBlank()) {
            this.properties.setProperty(KEY_HOST_APPLICATION, hostApplication);
        }
        if (created != null) {
            this.properties.setProperty(KEY_CREATED, created.toString());
        }
    }

    /**
     * Resolves the creation timestamp recorded when this container directory was initialized.
     *
     * @return The creation {@link Instant}, or {@code null} if unrecorded or unparseable.
     */
    public Instant getCreationTime() {
        String createdStr = properties.getProperty(KEY_CREATED);
        if (createdStr == null || createdStr.isBlank()) {
            return null;
        }
        try {
            return Instant.parse(createdStr);
        } catch (DateTimeParseException e) {
            log.warn("Failed to parse creation timestamp '{}' from {}: {}", createdStr, PROPERTIES_FILE_NAME, e.getMessage());
            return null;
        }
    }

    /**
     * Resolves the container version as a parsed {@link Version} object.
     *
     * @return The {@link Version}, or {@code null} if missing or invalid.
     */
    public Version getContainerVersion() {
        String verStr = properties.getProperty(KEY_VERSION);
        if (verStr == null || verStr.isBlank()) {
            return null;
        }
        return Version.parse(verStr).orElse(null);
    }

    /**
     * Resolves the host application identifier (e.g. {@code "netbeans"}, {@code "intellij"}, {@code "AsiDesktop"}).
     *
     * @return The host application ID, or {@code null} if missing.
     */
    public String getHostApplication() {
        return properties.getProperty(KEY_HOST_APPLICATION);
    }

    /**
     * Checks if the {@code anahata.asi.properties} file exists within the specified directory.
     *
     * @param containerDir The directory to inspect.
     * @return {@code true} if the properties file exists and is a regular file.
     */
    public static boolean exists(Path containerDir) {
        if (containerDir == null) {
            return false;
        }
        Path propFile = containerDir.resolve(PROPERTIES_FILE_NAME);
        return Files.isRegularFile(propFile);
    }

    /**
     * Loads the {@code anahata.asi.properties} file from the specified container directory if present.
     *
     * @param containerDir The directory containing the properties file.
     * @return An {@link Optional} containing the loaded properties entity, or empty if missing or unreadable.
     */
    public static Optional<AsiContainerProperties> load(Path containerDir) {
        if (!exists(containerDir)) {
            return Optional.empty();
        }
        Path propFile = containerDir.resolve(PROPERTIES_FILE_NAME);
        Properties props = new Properties();
        try (InputStream in = Files.newInputStream(propFile)) {
            props.load(in);
            return Optional.of(new AsiContainerProperties(props));
        } catch (IOException e) {
            log.warn("Could not read {} from {}: {}", PROPERTIES_FILE_NAME, containerDir, e.getMessage());
            return Optional.empty();
        }
    }

    /**
     * Creates and saves an {@code anahata.asi.properties} metadata file in the target directory.
     *
     * @param containerDir The container directory path.
     * @param version The container version string.
     * @param hostApplicationId The host application identifier.
     * @param created The creation timestamp.
     * @return The newly saved {@link AsiContainerProperties} entity.
     * @throws IOException If saving fails.
     */
    public static AsiContainerProperties save(Path containerDir, String version, String hostApplicationId, Instant created) throws IOException {
        if (!Files.exists(containerDir)) {
            Files.createDirectories(containerDir);
        }
        Path propFile = containerDir.resolve(PROPERTIES_FILE_NAME);
        Properties props = new Properties();
        if (version != null && !version.isBlank()) {
            props.setProperty(KEY_VERSION, version);
        }
        if (hostApplicationId != null && !hostApplicationId.isBlank()) {
            props.setProperty(KEY_HOST_APPLICATION, hostApplicationId);
        }
        if (created != null) {
            props.setProperty(KEY_CREATED, created.toString());
        }

        try (OutputStream out = Files.newOutputStream(propFile)) {
            props.store(out, "Anahata ASI Container Metadata");
        }
        log.info("Saved container properties to: {}", propFile);
        return new AsiContainerProperties(props);
    }
}
