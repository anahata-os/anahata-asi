/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.tools.project.components;

import java.io.Serializable;
import java.util.Collections;
import java.util.List;
import javax.lang.model.element.ElementKind;

/**
 * Immutable descriptor representing a cached inner or nested class within a Java source file.
 * <p>
 * Preserves the structural hierarchy, element kind, supertypes, and Javadoc summary of nested
 * types across turns without requiring re-parsing of the AST symbol table.
 * </p>
 *
 * @param fqn The fully qualified name of the nested type (e.g. {@code "com.foo.Bar$Inner"}).
 * @param kind The Java element kind (e.g. {@link ElementKind#CLASS}, {@link ElementKind#INTERFACE}, {@link ElementKind#RECORD}).
 * @param supertypes The formatted supertypes string (e.g. {@code "implements Serializable"}), or {@code null} if none.
 * @param javadocSummary The sanitized first sentence of the class-level Javadoc summary, or {@code null} if none.
 * @param children Recursive list of nested member types declared within this inner class.
 *
 * @author Anahata
 */
public record CachedInnerClass(
        String fqn,
        ElementKind kind,
        String supertypes,
        String javadocSummary,
        List<CachedInnerClass> children
) implements Serializable {

    /**
     * Canonical constructor guaranteeing non-null, immutable child collections.
     *
     * @param fqn The fully qualified name of the nested type.
     * @param kind The element kind.
     * @param supertypes The supertypes declaration string.
     * @param javadocSummary The Javadoc summary string.
     * @param children The list of nested child types.
     */
    public CachedInnerClass {
        children = (children == null || children.isEmpty())
                ? Collections.emptyList()
                : List.copyOf(children);
    }
}
