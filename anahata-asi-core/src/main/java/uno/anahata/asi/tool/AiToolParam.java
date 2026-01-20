/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.tool;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Provides a description for a parameter of a method marked with {@link AiTool}.
 * This is essential for the model to understand how to use the tool correctly,
 * as it is used to generate the JSON schema for the tool's arguments.
 *
 * @author anahata-gemini-pro-2.5
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.PARAMETER)
public @interface AiToolParam {
    /**
     * A clear and concise description of the parameter's purpose and expected format.
     * This is injected into the tool's JSON schema as the property description.
     * 
     * @return The description of this parameter.
     */
    String value();
    
    /**
     * Determines whether this parameter is mandatory for the tool call.
     * 
     * @return {@code true} if the parameter is required, {@code false} otherwise.
     */
    boolean required() default true;

    /**
     * The ID of a specialized renderer that should be used to display the value 
     * of this parameter in the UI (e.g., 'java', 'path', 'markdown').
     * 
     * @return The renderer ID, or an empty string for default rendering.
     */
    String rendererId() default "";
}
