/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.intellij.tools.java;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;

import java.net.URL;

/**
 * A lightweight, serializable "keychain" DTO that uniquely identifies a Java type.
 * It holds the fully qualified name and a URL pointing to the class file or source file.
 * 
 * @author anahata
 */
@Data
@SuperBuilder
@NoArgsConstructor
@AllArgsConstructor
public class JavaType {

    /** The fully qualified name of the type (e.g., java.lang.String). */
    private String fqn;
    
    /** The URL pointing to the class file or source file for this type. */
    private URL url;
}
