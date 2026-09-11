/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.agi.tool;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Documents internal framework methods on {@link ToolContext} and other toolkits,
 * capturing human-readable descriptions and thread-safety requirements for subthread execution.
 *
 * @author anahata
 */
@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.METHOD)
public @interface Internal {

    /**
     * Concise description of what the internal method does.
     *
     * @return The method description.
     */
    String value();

    /**
     * Whether calling this method from a subthread (e.g. JavaFX thread, custom thread pool,
     * or worker task) requires capturing the {@link ToolContext} first via {@code getToolContext()}.
     *
     * @return {@code true} if a captured ToolContext is required outside the tool execution thread.
     */
    boolean requiresCapturedToolContext() default false;
}
