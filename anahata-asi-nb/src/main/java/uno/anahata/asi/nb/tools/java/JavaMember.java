/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
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
     * {@inheritDoc} 
     * <p>This implementation specializes the source retrieval for class members, 
     * returning a {@link JavaMemberSource} which extracts only the member's 
     * specific source code fragment from the containing file.</p> 
     */
    @Override
    public JavaMemberSource getSource() throws Exception {
        return new JavaMemberSource(this);
    }

    /** 
     * {@inheritDoc} 
     * <p>This implementation specializes the Javadoc retrieval for class members, 
     * returning a {@link JavaMemberDocs} which leverages the element handle 
     * to fish the member's specific documentation from sources or index.</p> 
     */
    @Override
    public JavaMemberDocs getJavadoc() throws Exception {
        return new JavaMemberDocs(this);
    }
}
