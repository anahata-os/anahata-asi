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
package uno.anahata.ai.model.core;

import java.util.List;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NonNull;
import uno.anahata.ai.model.provider.AbstractModel;

/**
 * A standardized, model-agnostic request to be sent to an AI model provider.
 * @author anahata-gemini-pro-2.5
 */
@Getter
@AllArgsConstructor
public class Request {
    /** The model to use for the request. */
    @NonNull
    private final AbstractModel model;
    
    /** The complete conversation history to be sent to the model. */
    @NonNull
    private final List<AbstractMessage> history;

    /** The configuration for this specific request. */
    private final RequestConfig config;
}
