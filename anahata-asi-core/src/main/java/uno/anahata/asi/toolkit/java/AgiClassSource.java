/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.toolkit.java;

import lombok.Builder;
import lombok.NonNull;

/**
 * Immutable source code descriptor for an in-memory Java class.
 * <p>
 * Binds a fully qualified class name (FQN) to its complete Java source code.
 * Used for batch compilation of modular architectures and circular
 * dependencies.
 * </p>
 *
 * @param fqn The fully qualified class name (e.g. {@code "com.foo.Bar"}).
 * @param sourceCode The complete Java source code of the class.
 *
 * @author anahata
 */
@Builder
public record AgiClassSource(
        @NonNull
        String fqn,
        @NonNull
        String sourceCode
        ) {

    /**
     * Canonical constructor validating that FQN and sourceCode are non-null and
     * trimmed.
     *
     * @param fqn The class FQN.
     * @param sourceCode The class source code.
     */
    public AgiClassSource {
        fqn = fqn.trim();
        if (fqn.isBlank()) {
            throw new IllegalArgumentException("Class FQN cannot be blank.");
        }
        if (sourceCode.isBlank()) {
            throw new IllegalArgumentException("Source code cannot be blank.");
        }
    }

    /**
     * Calculates the number of lines in the source code.
     *
     * @return Total line count.
     */
    public int getLineCount() {
        return sourceCode.split("\r\n|\r|\n").length;
    }
}
