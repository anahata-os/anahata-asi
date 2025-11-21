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
package uno.anahata.ai.model.tool.java;

import java.util.Map;
import lombok.Getter;
import lombok.NonNull;
import uno.anahata.ai.model.tool.AbstractToolCall;

/**
 * A model-agnostic representation of a request to execute a specific Java method tool.
 *
 * @author anahata-gemini-pro-2.5
 */
@Getter
public class JavaMethodToolCall extends AbstractToolCall<JavaMethodTool, JavaMethodToolResponse> {
    
    public JavaMethodToolCall(@NonNull String id, @NonNull JavaMethodTool tool, @NonNull Map<String, Object> args) {
        super(id, tool, args);
    }

    @Override
    protected JavaMethodToolResponse createResponse() {
        return new JavaMethodToolResponse(this);
    }
}
