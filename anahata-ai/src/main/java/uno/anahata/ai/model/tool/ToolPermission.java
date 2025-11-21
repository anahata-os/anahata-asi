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
 * Represents the user's interactive, one-time choice for a specific tool invocation
 * within the UI prompt.
 *
 * @author anahata-gemini-pro-2.5
 */
public enum ToolPermission {
    /** Approve the invocation for this turn only. */
    APPROVE,
    /** Deny the invocation for this turn only. */
    DENY,
    /** Approve for this turn and save the preference as ALWAYS. */
    APPROVE_ALWAYS,
    /** Deny for this turn and save the preference as NEVER. */
    DENY_NEVER;
}
