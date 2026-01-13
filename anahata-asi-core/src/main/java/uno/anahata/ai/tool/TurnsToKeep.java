/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.ai.tool;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * A dedicated annotation to explicitly define the retention policy for a tool's
 * call/response pair in the conversation history, in number of user turns.
 * <p>
 * This can be applied at the class level (on an {@code @AiToolkit}) or at the
 * method level (on an {@code @AiTool}). The method-level annotation always
 * takes precedence.
 *
 * @author anahata-gemini-pro-2.5
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE, ElementType.METHOD})
public @interface TurnsToKeep {
    /**
     * The number of user turns the tool call and its response should be
     * retained in the conversation context.
     */
    int value();
}
