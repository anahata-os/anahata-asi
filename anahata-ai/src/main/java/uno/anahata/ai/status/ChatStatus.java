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
 * Fora Bara!
 */
package uno.anahata.ai.status;

import lombok.Getter;

/**
 * Defines the possible operational states of the Chat, primarily for UI feedback.
 * This is a direct port of the proven V1 enum.
 * 
 * @author pablo
 */
@Getter
public enum ChatStatus {
    /** A normal API call is in progress (e.g., waiting for a model response). */
    API_CALL_IN_PROGRESS("API Call in Progress...", "Waiting for a response from the model."),
    
    /** Local tool (function) execution is in progress. */
    TOOL_EXECUTION_IN_PROGRESS("Tool Execution...", "Executing local Java tools (functions)."),
    
    /** An API error occurred, and the system is in retry mode with exponential backoff. */
    WAITING_WITH_BACKOFF("Waiting with Backoff...", "An API error occurred. Retrying with exponential backoff."),
    
    /** The assistant has hit the maximum number of retries and has stopped. */
    MAX_RETRIES_REACHED("Max Retries Reached", "The assistant has stopped after hitting the maximum number of retries."),
    
    /** The model has finished processing and is waiting for the user's next input. */
    IDLE_WAITING_FOR_USER("Idle", "Waiting for user input.");

    private final String displayName;
    private final String description;

    ChatStatus(String displayName, String description) {
        this.displayName = displayName;
        this.description = description;
    }

    @Override
    public String toString() {
        return displayName;
    }
}
