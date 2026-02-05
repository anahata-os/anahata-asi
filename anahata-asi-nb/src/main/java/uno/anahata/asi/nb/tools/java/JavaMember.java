/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi.nb.tools.java;

import java.net.URL;
import java.util.Set;
import javax.lang.model.element.Element;
import javax.lang.model.element.ElementKind;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import lombok.ToString;
import lombok.experimental.SuperBuilder;
import org.netbeans.api.java.source.ElementHandle;

/**
 * A lightweight, serializable "keychain" DTO that uniquely identifies a Java
 * class member (field, method, constructor, etc.). By extending {@link JavaType},
 * members that represent types (classes, interfaces, enums) can be used directly
 * as roots for further exploration.
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

    /**
     * The simple name of the member (e.g., "myField", "myMethod").
     */
    private String name;

    /**
     * The kind of the member (e.g., FIELD, METHOD, CONSTRUCTOR).
     */
    private ElementKind kind;

    /**
     * The set of modifiers for this member (e.g., "public", "static", "default").
     */
    private Set<String> modifiers;

    /**
     * Constructs a new JavaMember.
     * @param handle the element handle.
     * @param fqn the fully qualified name of the member.
     * @param name the member name.
     * @param kind the member kind.
     * @param url the class file URL.
     * @param modifiers the set of modifiers.
     */
    public JavaMember(ElementHandle<? extends Element> handle, String fqn, String name, ElementKind kind, URL url, Set<String> modifiers) {
        super(handle, fqn, url);
        this.name = name;
        this.kind = kind;
        this.modifiers = modifiers;
    }

    /**
     * Gets the source code for this member.
     * @return a JavaMemberSource object.
     * @throws Exception if the source cannot be retrieved.
     */
    @Override
    public JavaMemberSource getSource() throws Exception {
        return new JavaMemberSource(this);
    }

    /**
     * Gets the Javadoc for this member.
     * @return a JavaMemberDocs object.
     * @throws Exception if the Javadoc cannot be retrieved.
     */
    @Override
    public JavaMemberDocs getJavadoc() throws Exception {
        return new JavaMemberDocs(this);
    }
}
