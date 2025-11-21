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

import com.google.gson.Gson;
import com.google.gson.JsonSyntaxException;
import java.lang.reflect.Method;
import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import lombok.Getter;
import lombok.NonNull;
import uno.anahata.ai.model.tool.AbstractTool;
import uno.anahata.ai.model.tool.ToolPermission;
import uno.anahata.ai.model.tool.AbstractToolkit;

/**
 * A model-agnostic, stateful representation of a single Java method tool.
 * This is a self-contained, executable unit that encapsulates both the
 * definition of the tool and the logic required to invoke it via reflection.
 *
 * @author anahata-gemini-pro-2.5
 */
@Getter
public class JavaMethodTool extends AbstractTool<JavaMethodToolParameter, JavaMethodToolCall> {
    private static final Gson GSON = new Gson();

    /** The full Java method signature. */
    @NonNull
    private final String javaMethodSignature;

    /** The underlying Java method that this tool represents. */
    @NonNull
    private final Method method;

    /** The singleton instance of the toolkit class, used for invoking non-static methods. */
    private final Object toolInstance;

    public JavaMethodTool(
            @NonNull String name,
            @NonNull String description,
            @NonNull ToolPermission permission,
            @NonNull List<JavaMethodToolParameter> parameters,
            @NonNull String javaMethodSignature,
            @NonNull Method method,
            int retentionTurns,
            Object toolInstance, // Can be null for static methods
            AbstractToolkit toolkit,
            String returnTypeSchema
    ) {
        super(name, description, toolkit, permission, parameters, returnTypeSchema);
        setRetentionTurns(retentionTurns);
        this.javaMethodSignature = javaMethodSignature;
        this.method = method;
        this.toolInstance = toolInstance;
    }

    @Override
    public JavaMethodToolCall createCall(String id, Map<String, Object> jsonArgs) {
        // 1. Pre-flight validation for required parameters
        List<String> missingParams = new ArrayList<>();
        for (JavaMethodToolParameter param : getParameters()) { // No cast needed!
            if (param.isRequired() && !jsonArgs.containsKey(param.getName())) {
                missingParams.add(param.getName());
            }
        }
        if (!missingParams.isEmpty()) {
            String reason = "Tool call rejected: Missing required parameters: " + String.join(", ", missingParams);
            JavaMethodToolCall call = new JavaMethodToolCall(id, this, jsonArgs);
            call.getResponse().reject(reason);
            return call;
        }

        // 2. Convert arguments from JSON types to Java types using our rich parameter models
        Map<String, Object> convertedArgs = new HashMap<>();
        try {
            for (JavaMethodToolParameter javaParam : getParameters()) { // No cast needed!
                String paramName = javaParam.getName();
                Object rawValue = jsonArgs.get(paramName);
                if (rawValue != null) {
                    String jsonValue = GSON.toJson(rawValue);
                    // Use the stored generic Type for accurate deserialization
                    Object convertedValue = GSON.fromJson(jsonValue, javaParam.getJavaType());
                    convertedArgs.put(paramName, convertedValue);
                }
            }
        } catch (JsonSyntaxException e) {
            String reason = "Tool call rejected: Failed to convert arguments. Error: " + e.getMessage();
            JavaMethodToolCall call = new JavaMethodToolCall(id, this, jsonArgs);
            call.getResponse().reject(reason);
            return call;
        }

        // 3. Create the final call object
        return new JavaMethodToolCall(id, this, convertedArgs);
    }

    @Override
    public Type getResponseType() {
        return JavaMethodToolResponse.class;
    }
}
