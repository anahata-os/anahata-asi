/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.tool;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Marks a class as an AI-callable toolkit and provides essential metadata.
 * All public methods within a class annotated with {@code @AiToolkit}
 * are automatically registered as individual tools, provided they are also
 * annotated with {@code @AiTool}.
 *
 * @author anahata-gemini-pro-2.5
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
public @interface AiToolkit {

    /**
     * A detailed description of what the tools in this toolkit do, including its purpose,
     * usage notes, and any general constraints. This is provided to the model to help
     * it understand the toolkit's capabilities.
     * 
     * @return The toolkit description.
     */
    String value();

    /**
     * The default maximum depth policy for ALL of this toolkit's tools. 
     * This serves as a fallback for any tools in this toolkit that do 
     * not specify an explicit max depth policy in their {@code @AiTool} annotation.
     * <p>
     * A value of -1 indicates that the value should be inherited from the system default 
     * defined in {@code ChatConfig}.
     * 
     * @return The default maximum depth.
     */
    int maxDepth() default -1; // Inherit from system default
}
