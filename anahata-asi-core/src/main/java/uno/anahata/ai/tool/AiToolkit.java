/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.ai.tool;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Marks a class as an AI-callable toolkit and provides essential metadata.
 * All public static methods within a class annotated with {@code @AiToolkit}
 * are automatically registered as individual tools, provided they are also
 * annotated with {@code @AiTool}.
 *
 * @author anahata-gemini-pro-2.5
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
public @interface AiToolkit {

    /**
     * A detailed description of what the tools on this toolkit do, including its purpose,
     * usage notes, etc.
     */
    String value();

    /**
     * The default retention policy for ALL of this toolkit's tools in number of user turns. 
     * This serves as a default for any tools in this toolkit that do 
     * not specify a retention policy.
     * A value of -1 indicates that the value should be inherited from the system default.
     */
    int retention() default -1; // Inherit from system default
}
