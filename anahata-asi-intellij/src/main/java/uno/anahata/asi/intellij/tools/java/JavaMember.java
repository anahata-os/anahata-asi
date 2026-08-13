/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.intellij.tools.java;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import lombok.ToString;
import lombok.experimental.SuperBuilder;

import javax.lang.model.element.ElementKind;
import java.net.URL;
import java.util.Set;

/**
 * A lightweight, serializable "keychain" DTO that uniquely identifies a Java class member 
 * (field, method, constructor, etc.) inside the IntelliJ IDEA platform.
 * 
 * @author anahata
 */
@Data
@SuperBuilder
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode(callSuper = true)
@ToString(callSuper = true)
public class JavaMember extends JavaType {

    /** The simple name of the member (e.g., "myField", "myMethod"). */
    private String name;

    /** The kind of the member (e.g., FIELD, METHOD, CONSTRUCTOR). */
    private ElementKind kind;

    /** The set of modifiers for this member (e.g., "public", "static", "default"). */
    private Set<String> modifiers;

    /**
     * Custom constructor to initialize all fields, including the inherited type keychain.
     *
     * @param fqn       the canonical fully-qualified name of the member.
     * @param name      the simple member name.
     * @param kind      the member kind (field, method, constructor, …).
     * @param url       the file URL backing the declaring type.
     * @param modifiers the member's modifier keywords.
     */
    public JavaMember(String fqn, String name, ElementKind kind, URL url, Set<String> modifiers) {
        super(fqn, url);
        this.name = name;
        this.kind = kind;
        this.modifiers = modifiers;
    }
}
