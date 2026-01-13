/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.ai.tool;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Provides a description for a parameter of a method marked with {@link AiTool}.
 * This is essential for the model to understand how to use the tool correctly.
 *
 * @author anahata-gemini-pro-2.5
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.PARAMETER)
public @interface AiToolParam {
    /**
     * A clear and concise description of the parameter's purpose.
     * @return The description of this parameter
     */
    String value();
    
    /**
     * Whether this parameter is required.
     * @return 
     */
    boolean required() default true;

    /**
     * Id of the renderer that will render the value of this parameter (e.g. 'code','path'...).
     * @return 
     */
    String rendererId() default "";
}
