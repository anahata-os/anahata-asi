/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.intellij.tools.java.codemodel;

import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.v3.oas.annotations.media.Schema;
import java.util.ArrayList;
import java.util.List;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * A recursive, token-efficient node representing a type hierarchy tree across any JVM language
 * (Java, Kotlin, Groovy, Scala) within the IntelliJ IDEA project model and index.
 * <p>
 * Eliminates redundant empty lists by using a single recursive {@link #children} collection
 * governed by the active {@link HierarchyDirection} (subtypes or supertypes), and avoids nested
 * URL keychains by placing type identity, language, and origin location directly on each node.
 * </p>
 *
 * @author anahata
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Schema(description = "A recursive node in a type hierarchy tree across any JVM language.")
public class HierarchyNode {

    /**
     * The fully-qualified name of the type at this node.
     */
    @Schema(description = "The fully-qualified name of the type.")
    private String fqn;

    /**
     * The simple name of the type at this node.
     */
    @Schema(description = "The simple name of the type.")
    private String simpleName;

    /**
     * The structural classification of the type (e.g. CLASS, INTERFACE, ENUM, RECORD, ANNOTATION, OBJECT, TRAIT).
     */
    @Schema(description = "The kind of type (e.g. CLASS, INTERFACE, ENUM, RECORD, ANNOTATION, OBJECT, TRAIT).")
    private String kind;

    /**
     * The source programming language (e.g. 'JAVA', 'KOTLIN', 'GROOVY', 'SCALA').
     */
    @Schema(description = "The source language (e.g. 'JAVA', 'KOTLIN', 'GROOVY', 'SCALA').")
    private String language;

    /**
     * The origin location descriptor (e.g. 'Module: anahata-asi-core', 'SDK: 25', 'Library: kryo-5.6.2.jar').
     */
    @Schema(description = "The origin location descriptor.")
    private String location;

    /**
     * The traversal direction represented by the children of this node (SUBTYPES or SUPERTYPES).
     */
    @Schema(description = "The traversal direction represented by the children (SUBTYPES or SUPERTYPES).")
    private HierarchyDirection direction;

    /**
     * The recursive list of child hierarchy nodes (subtypes or supertypes depending on traversal direction).
     */
    @Builder.Default
    @JsonInclude(JsonInclude.Include.NON_EMPTY)
    @Schema(description = "Recursive list of child nodes (subtypes or supertypes).")
    private List<HierarchyNode> children = new ArrayList<>();
}
