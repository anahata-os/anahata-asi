/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.intellij.tools.java.codemodel;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Lightweight, token-efficient descriptor representing a type across any JVM language
 * (Java, Kotlin, Groovy, Scala) within the IntelliJ IDEA project model and index.
 * <p>
 * Contains the fully-qualified name, simple name, element kind (e.g. CLASS, INTERFACE,
 * ENUM, RECORD, ANNOTATION, OBJECT, TRAIT), programming language, and a human-readable
 * location tag (e.g. SDK name, Maven module, or dependency JAR) used for deterministic
 * disambiguation when multiple types share the same FQN across classpaths.
 * </p>
 *
 * @author anahata
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Schema(description = "Represents a type across any JVM language (Java, Kotlin, Groovy, Scala) discovered in IntelliJ's project or library index.")
public class TypeItem {

    /**
     * The fully-qualified name of the type (e.g. 'java.util.List' or 'org.jetbrains.idea.maven.dom.MavenDomUtil').
     */
    @Schema(description = "The fully-qualified name of the type.")
    private String fqn;

    /**
     * The simple name of the type (e.g. 'List' or 'MavenDomUtil').
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
     * Disambiguation location descriptor indicating the origin module, SDK, or library JAR.
     */
    @Schema(description = "The origin location (e.g. 'Module: anahata-asi-core', 'SDK: 25', 'Library: kryo-5.6.2.jar').")
    private String location;
}
