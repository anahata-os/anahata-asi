/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.intellij.tools.java.codemodel;

import io.swagger.v3.oas.annotations.media.Schema;
import java.util.List;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Lightweight, token-efficient descriptor representing a member (method, constructor,
 * field, property, or nested type) across any JVM language (Java, Kotlin, Groovy, Scala)
 * within the IntelliJ IDEA project model and index.
 * <p>
 * Unlike legacy models, this DTO carries zero redundant URLs or platform SPI handles.
 * It provides the canonical member FQN, simple name, element kind, source language,
 * full human-readable signature, return type, modifiers, and inheritance origin.
 * </p>
 *
 * @author anahata
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Schema(description = "Represents a member (method, constructor, field, or nested type) across any JVM language.")
public class MemberItem {

    /**
     * The canonical fully-qualified name of the member (e.g. 'java.util.List.add(java.lang.Object)').
     */
    @Schema(description = "The canonical fully-qualified name of the member (e.g. 'pkg.Class.method(paramType)').")
    private String fqn;

    /**
     * The simple name of the member (e.g. 'add' or 'count').
     */
    @Schema(description = "The simple name of the member.")
    private String name;

    /**
     * The kind of member (e.g. METHOD, CONSTRUCTOR, FIELD, PROPERTY, CLASS, INTERFACE, ENUM).
     */
    @Schema(description = "The kind of member (e.g. METHOD, CONSTRUCTOR, FIELD, PROPERTY, CLASS, INTERFACE, ENUM).")
    private String kind;

    /**
     * The source programming language (e.g. 'JAVA', 'KOTLIN', 'GROOVY', 'SCALA').
     */
    @Schema(description = "The source programming language (e.g. 'JAVA', 'KOTLIN', 'GROOVY', 'SCALA').")
    private String language;

    /**
     * The human-readable declaration signature (e.g. 'public boolean add(E e)' or 'val count: Int').
     */
    @Schema(description = "The declaration signature (e.g. 'public boolean add(E e)' or 'val count: Int').")
    private String signature;

    /**
     * The return type or field type (e.g. 'boolean' or 'java.lang.String').
     */
    @Schema(description = "The return type or field type.")
    private String returnType;

    /**
     * The list of modifier keywords (e.g. ['public', 'static', 'final', 'suspend']).
     */
    @Schema(description = "The list of modifier keywords.")
    private List<String> modifiers;

    /**
     * Whether this member was inherited from a superclass or interface rather than declared directly.
     */
    @Schema(description = "True if the member is inherited from a supertype; false if declared directly.")
    private boolean isInherited;

    /**
     * The FQN of the class that originally declared this member.
     */
    @Schema(description = "The FQN of the declaring class (especially useful if inherited).")
    private String declaringClassFqn;
}
