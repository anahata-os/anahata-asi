/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi.nb.tools.java;

import java.net.URL;
import javax.lang.model.element.Element;
import javax.lang.model.element.ElementKind;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.netbeans.api.java.source.ElementHandle;

/**
 * A lightweight, serializable "keychain" DTO that uniquely identifies a Java
 * class member (field, method, constructor, etc.). It is designed to be the
 * result of a discovery tool and the input to an action tool.
 *
 * @author anahata
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class JavaMember {

    /**
     * The serializable handle to the actual code element. This may be null if
     * the member was discovered via a method that doesn't produce a handle.
     */
    private ElementHandle<? extends Element> handle;

    /**
     * The simple name of the member (e.g., "myField", "myMethod").
     */
    private String name;

    /**
     * The kind of the member (e.g., FIELD, METHOD, CONSTRUCTOR).
     */
    private ElementKind kind;

    /**
     * A human-readable representation of the member's signature or type.
     */
    private String details;
    
    /**
     * The URL of the file containing this member. This is used to promote
     * inner types to first-class JavaType objects.
     */
    private URL url;

    @Override
    public String toString() {
        return kind + ": " + name + " (" + details + ")";
    }
}
