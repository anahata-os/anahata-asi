/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi;

import java.io.IOException;
import java.nio.file.DirectoryStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.stream.Stream;
import lombok.extern.slf4j.Slf4j;

/**
 * Service orchestrator for discovering predecessor ASI container installations and migrating persistent settings.
 * <p>
 * Evaluates sibling version directories in the host application working directory (e.g. {@code ~/.anahata/asi/netbeans/}),
 * identifies genuine prior container directories while ignoring non-version folders (like {@code screenshots} or {@code scratch}),
 * and migrates serialized {@code *.kryo} provider configurations, templates, and sessions.
 * </p>
 *
 * @author anahata
 */
@Slf4j
public class AsiContainerUpgrade {

    /**
     * Checks if a directory is a genuine Anahata ASI container directory holding settings.
     * <p>
     * A directory is recognized as an ASI container if it contains an {@code anahata.asi.properties} metadata file,
     * or (as a legacy fallback for pre-1.2.0 versions) if it contains a {@code providers}, {@code templates},
     * or {@code sessions} subdirectory containing at least one {@code *.kryo} entity file.
     * </p>
     *
     * @param dir The candidate directory path to inspect.
     * @return {@code true} if the directory contains ASI container data, {@code false} otherwise.
     */
    public static boolean isContainerDirectory(Path dir) {
        if (dir == null || !Files.isDirectory(dir)) {
            return false;
        }

        // Tier 1: Authoritative metadata file check
        if (AsiContainerProperties.exists(dir)) {
            return true;
        }

        // Tier 2: Legacy fallback check for pre-1.2.0 directories
        return hasKryoFiles(dir.resolve("providers"))
                || hasKryoFiles(dir.resolve("templates"))
                || hasKryoFiles(dir.resolve("sessions"));
    }

    /**
     * Inspects a directory to determine if it exists and contains at least one {@code *.kryo} file.
     *
     * @param dir The directory to inspect.
     * @return {@code true} if at least one regular {@code *.kryo} file is present.
     */
    private static boolean hasKryoFiles(Path dir) {
        if (!Files.isDirectory(dir)) {
            return false;
        }
        try (Stream<Path> stream = Files.list(dir)) {
            return stream.anyMatch(p -> !Files.isDirectory(p) && p.toString().endsWith(".kryo"));
        } catch (IOException e) {
            log.debug("Could not list directory {}: {}", dir, e.getMessage());
            return false;
        }
    }

    /**
     * Scans the base host application directory for predecessor version directories older than the running version.
     * <p>
     * Only considers directories whose names parse to a valid {@link Version}, are strictly older than {@code currentVersion},
     * and contain genuine container settings (verified via {@link #isContainerDirectory(Path)}). Returns the highest
     * predecessor version directory found.
     * </p>
     *
     * @param baseDir The parent host application directory (e.g. {@code ~/.anahata/asi/netbeans/}).
     * @param currentVersion The active container version to compare against.
     * @return An {@link Optional} containing the path to the immediate predecessor container directory, or empty if none found.
     */
    public static Optional<Path> findPredecessor(Path baseDir, Version currentVersion) {
        if (baseDir == null || currentVersion == null || !Files.isDirectory(baseDir)) {
            return Optional.empty();
        }

        List<Path> candidates = new ArrayList<>();
        try (DirectoryStream<Path> ds = Files.newDirectoryStream(baseDir, Files::isDirectory)) {
            for (Path subDir : ds) {
                String folderName = subDir.getFileName().toString();
                Optional<Version> parsedOpt = Version.parse(folderName);
                if (parsedOpt.isPresent()) {
                    Version v = parsedOpt.get();
                    if (v.isOlderThan(currentVersion) && isContainerDirectory(subDir)) {
                        candidates.add(subDir);
                    }
                }
            }
        } catch (IOException e) {
            log.warn("Could not scan base directory for predecessor versions {}: {}", baseDir, e.getMessage());
            return Optional.empty();
        }

        if (candidates.isEmpty()) {
            return Optional.empty();
        }

        // Sort descending by Version to locate the highest predecessor
        candidates.sort((p1, p2) -> {
            Version v1 = Version.parse(p1.getFileName().toString()).orElseThrow();
            Version v2 = Version.parse(p2.getFileName().toString()).orElseThrow();
            return v2.compareTo(v1);
        });

        return Optional.of(candidates.get(0));
    }

    /**
     * Migrates persistent settings from a source predecessor container directory to the target container directory.
     * <p>
     * Copies all {@code *.kryo} files from {@code providers/}, {@code templates/}, and {@code sessions/}
     * (including {@code sessions/saved/} if present).
     * </p>
     *
     * @param sourceDir The predecessor container directory.
     * @param targetDir The destination container directory.
     * @return The total number of entity files migrated.
     * @throws IOException If copying files fails.
     */
    public static int copySettings(Path sourceDir, Path targetDir) throws IOException {
        if (sourceDir == null || targetDir == null) {
            throw new IllegalArgumentException("sourceDir and targetDir cannot be null");
        }
        if (!Files.isDirectory(sourceDir)) {
            throw new IOException("Source directory does not exist: " + sourceDir);
        }

        int count = 0;
        count += copyKryoFiles(sourceDir.resolve("providers"), targetDir.resolve("providers"));
        count += copyKryoFiles(sourceDir.resolve("templates"), targetDir.resolve("templates"));
        count += copyKryoFiles(sourceDir.resolve("sessions"), targetDir.resolve("sessions"));

        Path savedSessionsSource = sourceDir.resolve("sessions").resolve("saved");
        if (Files.isDirectory(savedSessionsSource)) {
            count += copyKryoFiles(savedSessionsSource, targetDir.resolve("sessions").resolve("saved"));
        }

        log.info("Migrated {} settings entity files from {} to {}", count, sourceDir, targetDir);
        return count;
    }

    /**
     * Copies all regular {@code *.kryo} files from a source directory to a destination directory.
     *
     * @param source The source directory.
     * @param target The target destination directory.
     * @return The count of files copied.
     * @throws IOException If file I/O fails.
     */
    private static int copyKryoFiles(Path source, Path target) throws IOException {
        if (!Files.isDirectory(source)) {
            return 0;
        }
        if (!Files.exists(target)) {
            Files.createDirectories(target);
        }

        int copied = 0;
        try (Stream<Path> stream = Files.list(source)) {
            List<Path> files = stream.filter(p -> !Files.isDirectory(p) && p.toString().endsWith(".kryo")).toList();
            for (Path file : files) {
                Path dest = target.resolve(file.getFileName());
                Files.copy(file, dest, StandardCopyOption.REPLACE_EXISTING);
                copied++;
            }
        }
        return copied;
    }
}
