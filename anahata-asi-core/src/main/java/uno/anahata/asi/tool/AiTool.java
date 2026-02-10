/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.tool;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import uno.anahata.asi.model.tool.ToolPermission;

/**
 * Marks a method as an AI-callable tool and provides essential metadata.
 * This is the cornerstone of the V2 tool framework, defining the tool's
 * description, max depth policy, and default approval behavior.
 *
 * @author anahata-gemini-pro-2.5
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.METHOD)
public @interface AiTool {

    /**
     * A detailed description of what the tool does, including its purpose,
     * parameters, and expected output. This is critical for the model's
     * understanding and is used to generate the tool's schema.
     * 
     * @return The tool description.
     */
    String value();

    /**
     * The maximum depth policy for this tool's call/response pair in the
     * conversation history. This serves as a default hint that can be 
     * overridden by the model at runtime.
     * <p>
     * A value of -1 indicates that the value should be inherited from the 
     * {@code @AiToolkit} annotation or the system default.
     * 
     * @return The maximum depth to keep this tool's result in context.
     */
    int maxDepth() default -1; // Inherit from toolkit

    /**
     * Determines the default approval behavior for this tool.
     * <ul>
     *   <li>If {@code true} (the default), the tool's initial permission will be 
     *       {@link ToolPermission#PROMPT}, requiring user confirmation for each call.</li>
     *   <li>If {@code false}, the initial permission will be 
     *       {@link ToolPermission#APPROVE_ALWAYS}, allowing the tool to run without 
     *       prompting unless overridden by user preferences.</li>
     * </ul>
     * 
     * @return {@code true} if the tool requires explicit user approval by default.
     */
    boolean requiresApproval() default true;
}
