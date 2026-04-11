/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb;

import java.util.List;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * Test subject for CodeRefiner tools.
 */
public class CodeRefinerTestSubject {

    /**
     * Serial version UID.
     */
    private static final long serialVersionUID = 1L;

    /**
     * Internal listener interface.
     */
    private static interface SubjectListener {
    }

    /**
     * Version string.
     */
    @Deprecated
    private static final String VERSION = "V1";

    /**
     * Status of the testing subject.
     */
    public static enum SubjectStatus {
    }

    /**
     * The name of the test subject. Used to verify field-replacement logic.
     */
    private String name;
    /**
     * List of achievements.
     */
    private final List<String> achievements = new ArrayList<>();
/**
     * The age of the subject. Use with caution!
     */
        protected Integer age;

    /**
     * Metadata record.
     */
    public static class SubjectMeta {

        /**
         * Unique identifier for the metadata record.
         */
        String id;
        /**
         * Creation timestamp for this record.
         */
        long timestamp;
    }

    /**
     * Default constructor for test instantiation.
     */
    public CodeRefinerTestSubject() {
    }

    /**
     * Gets the subject name with a Barça prefix.
     * @return the prefixed name
     */
    public String getName() {
        return name;
    }

    /**
     * Checks if the name is valid.
     */
    public boolean hasValidName() {
        return name != null && !name.isEmpty();
    }

    public void setName(String name) throws IllegalStateException {
        this.name = name;
    }

    /**
     * Filters a list of items to remove nulls and returns a mutable list.
     * @param <T> the type of items
     * @param items the list to filter
     * @return a mutable list without nulls
     * @throws IllegalArgumentException if items is null
     */
    public <T> List<T> filterNonNull(List<T> items) throws IllegalArgumentException {
        if (items == null) throw new IllegalArgumentException("items cannot be null");
        return items.stream().filter(Objects::nonNull).collect(Collectors.toList());
    }

    /**
     * A test annotation.
     */
    public static @interface MyTestAnnotation {
    }

    /**
     * Utility to get the type name.
     */
    public static String getTypeName() {
        return "CodeRefinerTestSubject";
    }
}

/**
 * A secondary top-level class for testing.
 */
class SecondaryClass {
}
