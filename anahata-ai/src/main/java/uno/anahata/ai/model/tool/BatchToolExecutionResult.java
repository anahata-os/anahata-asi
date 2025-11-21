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
package uno.anahata.ai.model.tool;

import uno.anahata.ai.model.tool.java.JavaMethodToolResponse;
import java.util.List;
import java.util.stream.Collectors;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NonNull;

/**
 * A container for the results of all tool invocations processed in a single turn.
 * This replaces {@code uno.anahata.ai.tools.FunctionProcessingResult}.
 *
 * @author anahata-gemini-pro-2.5
 */
@Getter
@AllArgsConstructor
public class BatchToolExecutionResult {
    /** A list of the individual results for each tool invocation. */
    @NonNull
    private final List<JavaMethodToolResponse> results;

    /** An optional comment provided by the user during the confirmation process. */
    private final String userComment;

    /** A flag indicating if the user cancelled the entire batch operation. */
    private final boolean wasCancelled;

    /**
     * Generates a concise summary string of all tool outcomes for user feedback.
     * @return A string like "[ToolName id=... STATUS] [ToolName id=... STATUS]"
     */
    /*
    public String getFeedbackSummary() {
        if (results == null || results.isEmpty()) {
            return wasCancelled ? "[Operation Cancelled]" : "[No tool outcomes]";
        }
        return results.stream()
            .map(result -> String.format("[%s id=%s %s]",
                result.getInvocation().getName(),
                result.getInvocation().getId(),
                result.getStatus().name()))
            .collect(Collectors.joining(" "));
    }
    */
}
