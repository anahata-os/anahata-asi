/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi;

import java.io.Serializable;
import java.util.Arrays;
import java.util.Objects;
import java.util.Optional;
import lombok.Getter;

/**
 * Immutable domain value object representing a software version for ASI containers and modules.
 * <p>
 * Supports parsing standard semantic versions, release-target qualifiers, and build snapshot stamps
 * (e.g. {@code "1.2.0"}, {@code "1.2.0-20260904"}, {@code "1.2.0-SNAPSHOT"}, {@code "1.1.14"}).
 * The clean version representation discards build qualifiers and timestamps following the first dash ('-'),
 * allowing reliable filesystem folder mapping and numeric segment comparisons.
 * </p>
 *
 * @author anahata
 */
@Getter
public final class Version implements Comparable<Version>, Serializable {

    /**
     * Serialization identifier for version instances.
     */
    private static final long serialVersionUID = 1L;

    /**
     * The raw, unmodified version string passed at construction.
     */
    private final String rawVersion;

    /**
     * The clean version string containing only the leading numeric dot segments before any dash.
     */
    private final String cleanVersion;

    /**
     * The array of parsed integer components for numeric segment comparison.
     */
    private final int[] segments;

    /**
     * Constructs a new Version instance from a raw version string.
     *
     * @param raw The raw version string to parse (e.g. {@code "1.2.0-20260904"}).
     * @throws IllegalArgumentException if the version string cannot be parsed into numeric segments.
     */
    public Version(String raw) {
        this.rawVersion = raw.trim();
        int dashIdx = this.rawVersion.indexOf('-');
        this.cleanVersion = (dashIdx != -1) ? this.rawVersion.substring(0, dashIdx).trim() : this.rawVersion;

        String[] parts = this.cleanVersion.split("\\.");
        if (parts.length == 0 || this.cleanVersion.isEmpty()) {
            throw new IllegalArgumentException("Invalid version string: " + raw);
        }

        int[] segs = new int[parts.length];
        for (int i = 0; i < parts.length; i++) {
            String part = parts[i].trim();
            if (part.isEmpty()) {
                throw new IllegalArgumentException("Empty segment in version: " + raw);
            }
            try {
                segs[i] = Integer.parseInt(part);
            } catch (NumberFormatException e) {
                throw new IllegalArgumentException("Non-numeric segment '" + part + "' in version: " + raw, e);
            }
        }
        this.segments = segs;
    }

    /**
     * Safely attempts to parse a string into a {@link Version} without throwing exceptions.
     * <p>
     * Particularly useful for evaluating filesystem directory names (e.g. ignoring folders like
     * {@code "screenshots"}, {@code "sessions"}, or hidden entries).
     * </p>
     *
     * @param text The candidate string to parse.
     * @return An {@link Optional} containing the parsed {@link Version}, or {@link Optional#empty()} if invalid.
     */
    public static Optional<Version> parse(String text) {
        if (text == null || text.isBlank()) {
            return Optional.empty();
        }
        try {
            return Optional.of(new Version(text));
        } catch (Exception e) {
            return Optional.empty();
        }
    }

    /**
     * Checks if this version is strictly older than another version.
     *
     * @param other The version to compare against.
     * @return {@code true} if this version is less than {@code other}.
     */
    public boolean isOlderThan(Version other) {
        return compareTo(other) < 0;
    }

    /**
     * Checks if this version is strictly newer than another version.
     *
     * @param other The version to compare against.
     * @return {@code true} if this version is greater than {@code other}.
     */
    public boolean isNewerThan(Version other) {
        return compareTo(other) > 0;
    }

    /**
     * Returns a copy of the numeric segments.
     *
     * @return Array of integer segments.
     */
    public int[] getSegments() {
        return segments.clone();
    }

    /**
     * {@inheritDoc}
     * <p>
     * Compares numeric version segments pairwise from major to minor. Missing trailing segments
     * in shorter versions are treated as zeroes (e.g. {@code "1.2"} is equivalent to {@code "1.2.0"}).
     * </p>
     */
    @Override
    public int compareTo(Version other) {
        int maxLen = Math.max(this.segments.length, other.segments.length);
        for (int i = 0; i < maxLen; i++) {
            int thisSeg = (i < this.segments.length) ? this.segments[i] : 0;
            int otherSeg = (i < other.segments.length) ? other.segments[i] : 0;
            if (thisSeg != otherSeg) {
                return Integer.compare(thisSeg, otherSeg);
            }
        }
        return 0;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Two Version objects are equal if they represent the exact same numeric version sequence.
     * </p>
     */
    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        Version other = (Version) o;
        return compareTo(other) == 0;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Computes the hash code based on the normalized numeric segments, ignoring trailing zeros
     * so that {@code equals()} and {@code hashCode()} maintain their invariant.
     * </p>
     */
    @Override
    public int hashCode() {
        int result = 1;
        int lastNonZero = segments.length - 1;
        while (lastNonZero >= 0 && segments[lastNonZero] == 0) {
            lastNonZero--;
        }
        for (int i = 0; i <= lastNonZero; i++) {
            result = 31 * result + segments[i];
        }
        return result;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Returns the clean version representation.
     * </p>
     */
    @Override
    public String toString() {
        return cleanVersion;
    }
}
