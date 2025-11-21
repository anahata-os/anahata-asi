/*
 * Copyright 2025 Anahata.
 *
 * Licensed under the Anahata Software License (ASL) V2.0;
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      https://github.com/pablo-anahata/anahata-ai-parent/blob/main/LICENSE
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * Força Barça!
 */
package uno.anahata.ai.tool;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import uno.anahata.ai.model.tool.ToolPermission;

/**
 * Marks a method as an AI-callable tool and provides essential metadata.
 * This is the cornerstone of the V2 tool framework.
 *
 * @author anahata-gemini-pro-2.5
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.METHOD)
public @interface AiTool {

    /**
     * A detailed description of what the tool does, including its purpose,
     * parameters, and expected output. This is critical for the model's
     * understanding.
     */
    String value();

    /**
     * The retention policy for this tool's call/response pair in the
     * conversation history, in number of user turns. This serves as a
     * default hint that can be overridden by the model at runtime.
     * A value of -1 indicates indefinite retention.
     */
    int retention() default 5; // System default

    /**
     * Determines the default approval behavior for this tool.
     * If {@code true} (the default), the tool's initial permission will be {@link ToolPermission#APPROVE},
     * requiring user confirmation for each call.
     * If {@code false}, the initial permission will be {@link ToolPermission#APPROVE_ALWAYS}, allowing
     * the tool to run without prompting unless overridden by user preferences.
     */
    boolean requiresApproval() default true;
}
