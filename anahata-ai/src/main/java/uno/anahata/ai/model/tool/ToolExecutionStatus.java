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

/**
 * Represents the lifecycle status of a single tool's execution.
 * This provides a more granular view than a simple success/failure flag.
 *
 * @author anahata-gemini-pro-2.5
 */
public enum ToolExecutionStatus {
    /** The tool call has been created but not yet processed or executed. */
    PENDING,

    /** The tool was executed successfully. */
    EXECUTED,

    /** The tool execution was attempted but failed due to an exception. */
    FAILED,
    
    /** The tool was not executed because it was not found in the list of enabled tools. */
    NOT_FOUND,

    /** 
     * The tool was not executed for a variety of reasons, such as being
     * skipped by the user, disabled by preferences, or cancelled. The
     * specific reason should be in the 'error' field of the response.
     */
    NOT_EXECUTED;
}
