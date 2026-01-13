/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.ai.model.tool.java;

import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import lombok.Getter;
import lombok.NonNull;
import uno.anahata.ai.model.core.AbstractModelMessage;
import uno.anahata.ai.model.tool.AbstractTool;
import uno.anahata.ai.model.tool.ToolPermission;
import uno.anahata.ai.tool.AiTool;
import uno.anahata.ai.tool.schema.SchemaProvider;

/**
 * A model-agnostic, stateful representation of a single Java method tool.
 * This is a self-contained, executable unit that encapsulates both the
 * definition of the tool and the logic required to invoke it via reflection.
 *
 * @author anahata-gemini-pro-2.5
 */
@Getter
public class JavaMethodTool extends AbstractTool<JavaMethodToolParameter, JavaMethodToolCall> {
    
    /** The full Java method signature. */
    @NonNull
    private final String javaMethodSignature;

    /** The underlying Java method that this tool represents. */
    @NonNull
    private final Method method;

    /** The singleton instance of the toolkit class, used for invoking non-static methods. */
    private final Object toolInstance;

    /**
     * The definitive, intelligent constructor for creating a JavaMethodTool from a reflection Method.
     * This constructor encapsulates all the logic for parsing annotations and generating schemas.
     *
     * @param toolkit The parent toolkit.
     * @param toolInstance The singleton instance of the class containing the method.
     * @param method The reflection Method to parse.
     * @param toolAnnotation The pre-fetched @AiTool annotation.
     * @throws Exception if schema generation fails.
     */
    public JavaMethodTool(JavaObjectToolkit toolkit, Object toolInstance, Method method, AiTool toolAnnotation) throws Exception {
        super(toolkit.getName() + "." + method.getName());

        // Set parent fields
        this.toolkit = toolkit;
        this.permission = toolAnnotation.requiresApproval()
            ? ToolPermission.PROMPT
            : ToolPermission.APPROVE_ALWAYS;
        super.responseJsonSchema = SchemaProvider.generateInlinedSchemaString(getResponseType(), "result", method.getGenericReturnType());
        
        // Set own fields
        this.method = method;
        this.toolInstance = toolInstance;
        this.javaMethodSignature = buildMethodSignature(method);
        
        // Build description
        StringBuilder descriptionBuilder = new StringBuilder(toolAnnotation.value());
        descriptionBuilder.append("\n\njava method signature: ").append(this.javaMethodSignature);
        this.description = descriptionBuilder.toString();

        // Set retention using the clean inheritance model
        int retention = toolAnnotation.retention();
        if (retention == -1 && toolkit != null) { // Sentinel for inherit
            retention = toolkit.getDefaultRetention();
        }
        setRetentionTurns(retention);

        // A tool creates its own parameters.
        for (java.lang.reflect.Parameter p : method.getParameters()) {
            getParameters().add(JavaMethodToolParameter.of(this, p));
        }
    }
    
    public static String buildMethodSignature(Method m) {
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
    public JavaMethodToolCall createCall(AbstractModelMessage modelMessage, String id, Map<String, Object> jsonArgs) {
        // 1. Pre-flight validation for required parameters
        List<String> missingParams = new ArrayList<>();
        for (JavaMethodToolParameter param : getParameters()) {
            if (param.isRequired() && !jsonArgs.containsKey(param.getName())) {
                missingParams.add(param.getName());
            }
        }
        if (!missingParams.isEmpty()) {
            String reason = "Tool call rejected: Missing required parameters: " + String.join(", ", missingParams);
            JavaMethodToolCall call = new JavaMethodToolCall(modelMessage, id, this, jsonArgs);
            call.getResponse().reject(reason);
            return call;
        }

        // 2. Convert arguments from JSON types to Java types using our rich parameter models
        Map<String, Object> convertedArgs = new HashMap<>();
        try {
            for (JavaMethodToolParameter javaParam : getParameters()) {
                String paramName = javaParam.getName();
                Object rawValue = jsonArgs.get(paramName);
                if (rawValue != null) {
                    // Use the stored generic Type for accurate deserialization
                    Object convertedValue = SchemaProvider.OBJECT_MAPPER.convertValue(rawValue, SchemaProvider.OBJECT_MAPPER.constructType(javaParam.getJavaType()));
                    convertedArgs.put(paramName, convertedValue);
                }
            }
        } catch (IllegalArgumentException e) {
            String reason = "Tool call rejected: Failed to convert arguments. Error: " + e.getMessage();
            JavaMethodToolCall call = new JavaMethodToolCall(modelMessage, id, this, jsonArgs);
            call.getResponse().reject(reason);
            return call;
        }

        // 3. Create the final call object
        return new JavaMethodToolCall(modelMessage, id, this, convertedArgs);
    }

    @Override
    public Type getResponseType() {
        return JavaMethodToolResponse.class;
    }
}
