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

import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.lang.reflect.Parameter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import lombok.Getter;
import lombok.NonNull;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.ai.model.tool.ToolPermission;
import uno.anahata.ai.model.tool.AbstractToolkit;
import uno.anahata.ai.tool.AIToolParam;
import uno.anahata.ai.tool.AiTool;
import uno.anahata.ai.tool.AiToolkit;
import uno.anahata.ai.tool.schema.SchemaProvider;

/**
 * A domain object that parses a Java class via reflection to build a complete,
 * self-contained Toolkit, including all its tools and parameters.
 * <p>
 * This class is the cornerstone of the V2's decoupled tool architecture,
 * separating the parsing of tool metadata from the management and execution of tools.
 */
@Slf4j
@Getter
public class JavaObjectToolkit extends AbstractToolkit<JavaMethodTool> {

    /** The singleton instance of the tool class. */
    private final Object toolInstance;

    /** A list of all declared methods (tools) for this toolkit. */
    private final List<JavaMethodTool> tools;

    /**
     * Constructs a new JavaObjectToolkit by parsing the given class.
     * @param toolClass The class to parse.
     * @throws IllegalArgumentException if the class is not a valid toolkit.
     */
    public JavaObjectToolkit(Class<?> toolClass) throws Exception {
        super(parseToolkitName(toolClass), parseToolkitDescription(toolClass));
        
        try {
            this.toolInstance = toolClass.getDeclaredConstructor().newInstance();
        } catch (Exception e) {
            throw new IllegalArgumentException("Could not instantiate tool class: " + toolClass.getName() + ". It must be public and have a public no-arg constructor.", e);
        }

        this.tools = new ArrayList<>();
        AiToolkit toolkitAnnotation = toolClass.getAnnotation(AiToolkit.class);
        int defaultRetention = (toolkitAnnotation != null) ? toolkitAnnotation.retention() : 5;

        for (Method method : toolClass.getDeclaredMethods()) {
            if (method.isAnnotationPresent(AiTool.class)) {
                tools.add(buildJavaMethodTool(method, defaultRetention));
            }
        }
    }

    private static String parseToolkitName(Class<?> toolClass) {
        if (!toolClass.isAnnotationPresent(AiToolkit.class)) {
            throw new IllegalArgumentException("Class " + toolClass.getName() + " is not annotated with @AiToolkit.");
        }
        return toolClass.getSimpleName();
    }

    private static String parseToolkitDescription(Class<?> toolClass) {
        return toolClass.getAnnotation(AiToolkit.class).value();
    }

    private JavaMethodTool buildJavaMethodTool(Method method, int defaultRetention) throws Exception {
        AiTool toolAnnotation = method.getAnnotation(AiTool.class);
        String toolName = name + "." + method.getName();
        int retention = toolAnnotation.retention() != 5 ? toolAnnotation.retention() : defaultRetention;

        List<JavaMethodToolParameter> parameters = new ArrayList<>();
        for (Parameter p : method.getParameters()) {
            parameters.add(buildToolParameter(p));
        }

        ToolPermission defaultPermission = toolAnnotation.requiresApproval()
            ? ToolPermission.APPROVE
            : ToolPermission.APPROVE_ALWAYS;
            
        String returnTypeSchema = SchemaProvider.generateInlinedSchemaString(method.getGenericReturnType());

        return new JavaMethodTool(
            toolName,
            toolAnnotation.value(),
            defaultPermission,
            parameters, // Pass the specific list
            buildMethodSignature(method),
            method,
            retention,
            this.toolInstance, // Pass the instance to the tool for execution
            this,
            returnTypeSchema
        );
    }

    private JavaMethodToolParameter buildToolParameter(Parameter p) throws Exception {
        AIToolParam paramAnnotation = p.getAnnotation(AIToolParam.class);
        if (paramAnnotation == null) {
            throw new IllegalArgumentException("Parameter '" + p.getName() + "' in method '" + p.getDeclaringExecutable().getName() + "' is missing @AIToolParam annotation.");
        }
        
        String jsonSchema = SchemaProvider.generateInlinedSchemaString(p.getParameterizedType());
        if (jsonSchema == null) {
            throw new IllegalArgumentException("Could not generate schema for parameter " + p.getName() + " in method " + p.getDeclaringExecutable().getName());
        }
        
        return new JavaMethodToolParameter(
            p.getName(),
            paramAnnotation.value(),
            jsonSchema,
            paramAnnotation.required(),
            "", // rendererId is not in the annotation
            p.getParameterizedType()
        );
    }

    private String buildMethodSignature(Method m) {
        String signature = Modifier.toString(m.getModifiers())
            + " " + m.getGenericReturnType().getTypeName()
            + " " + m.getName() + "("
            + Arrays.stream(m.getParameters())
            .map(p -> p.getParameterizedType().getTypeName() + " " + p.getName())
            .collect(Collectors.joining(", "))
            + ")";

        if (m.getExceptionTypes().length > 0) {
            signature += " throws " + Arrays.stream(m.getExceptionTypes())
                .map(Class::getCanonicalName)
                .collect(Collectors.joining(", "));
        }
        return signature;
    }

    @Override
    public List<JavaMethodTool> getAllTools() {
        return tools;
    }
}
