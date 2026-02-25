/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.model.tool.java;

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
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.model.core.AbstractModelMessage;
import uno.anahata.asi.model.tool.AbstractTool;
import uno.anahata.asi.model.tool.ToolPermission;
import uno.anahata.asi.tool.AiTool;
import uno.anahata.asi.tool.schema.SchemaProvider;

/**
 * A model-agnostic, stateful representation of a single Java method tool.
 * This is a self-contained, executable unit that encapsulates both the
 * definition of the tool and the logic required to invoke it via reflection.
 *
 * @author anahata-gemini-pro-2.5
 */
@Getter
@Slf4j
public class JavaMethodTool extends AbstractTool<JavaMethodToolParameter, JavaMethodToolCall> {
    
    /** The full Java method signature. */
    @NonNull
    private final String javaMethodSignature;

    /** The underlying Java method that this tool represents. Transient as Method is not serializable. */
    private transient Method method;

    /**
     * The definitive, intelligent constructor for creating a JavaMethodTool from a reflection Method.
     * This constructor encapsulates all the logic for parsing annotations and generating schemas.
     *
     * @param toolkit The parent toolkit.
     * @param method The reflection Method to parse.
     * @param toolAnnotation The pre-fetched @AiTool annotation.
     * @throws Exception if schema generation fails.
     */
    public JavaMethodTool(JavaObjectToolkit toolkit, Method method, AiTool toolAnnotation) throws Exception {
        super(toolkit.getName() + "." + method.getName());

        // Set parent fields
        this.toolkit = toolkit;
        this.permission = toolAnnotation.requiresApproval()
            ? ToolPermission.PROMPT
            : ToolPermission.APPROVE_ALWAYS;
        super.responseJsonSchema = SchemaProvider.generateInlinedSchemaString(getResponseType(), "result", method.getGenericReturnType());
        
        // Set own fields
        this.method = method;
        this.javaMethodSignature = buildMethodSignature(method);
        
        // Set max depth using the clean inheritance model
        int maxDepth = toolAnnotation.maxDepth();
        if (maxDepth == 0) {
            throw new IllegalArgumentException("Tool '" + getName() + "' cannot have maxDepth=0. Use -1 to inherit or >= 1 to live.");
        }
        setMaxDepth(maxDepth);
        
        // Build description
        StringBuilder descriptionBuilder = new StringBuilder(toolAnnotation.value());
        descriptionBuilder.append("\n\nToolkit: ").append(this.toolkit.getName());        
        descriptionBuilder.append("\nMethod:\n").append(this.javaMethodSignature);
        this.description = descriptionBuilder.toString();

        // A tool creates its own parameters.
        java.lang.reflect.Parameter[] params = method.getParameters();
        for (int i = 0; i < params.length; i++) {
            getParameters().add(JavaMethodToolParameter.of(this, params[i], i));
        }
    }

    /**
     * Retrieves the instance of the toolkit class from the parent toolkit.
     * @return The toolkit instance.
     */
    public Object getToolkitInstance() {
        return ((JavaObjectToolkit)toolkit).getToolkitInstance();
    }

    /**
     * Returns the underlying Java Method, restoring it lazily if necessary.
     * @return The reflection Method object.
     */
    public synchronized Method getMethod() {
        if (method == null) {
            log.info("Lazily restoring Method for tool: {} using signature lookup", getName());
            Object instance = getToolkitInstance();
            if (instance == null) {
                 throw new RuntimeException("Cannot restore method: parent toolkit instance is not yet available for tool " + getName());
            }
            Class<?> currentClass = instance.getClass();
            while (currentClass != null && currentClass != Object.class) {
                for (Method m : currentClass.getDeclaredMethods()) {
                    if (javaMethodSignature.equals(buildMethodSignature(m))) {
                        this.method = m;
                        break;
                    }
                }
                if (method != null) break;
                currentClass = currentClass.getSuperclass();
            }
            
            if (method == null) {
                throw new RuntimeException("Failed to restore method via signature lookup: " + javaMethodSignature + " in " + instance.getClass().getName());
            }
        }
        return method;
    }
    
    @Override
    public String getDescription() {
        StringBuilder sb = new StringBuilder(super.description);
        sb.append("\nPermission: ").append(this.permission);
        
        int ret = getMaxDepth();
        sb.append("\nMax Depth: ").append(ret == -1 ? "-1 (inherit from toolkit)" : ret);
        
        int effective = getEffectiveMaxDepth();
        String source = "tool";
        if (ret == -1) {
            source = (toolkit.getDefaultMaxDepth() == -1) ? "chat config" : "toolkit";
        }
        sb.append("\nEffective Max Depth: ").append(effective).append(" (inherited from ").append(source).append(")");
        
        return sb.toString();
    }
    
    /**
     * Builds a human-readable string representation of a Java method's signature.
     * 
     * @param m The method to inspect.
     * @return The formatted method signature string.
     */
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
            JavaMethodToolCall call = new JavaMethodToolCall(modelMessage, id, this, jsonArgs, jsonArgs);
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
            log.error("Failed to convert arguments", e);
            String reason = "Tool call rejected: Failed to convert arguments. Error: " + e.getMessage();
            JavaMethodToolCall call = new JavaMethodToolCall(modelMessage, id, this, jsonArgs, jsonArgs);
            call.getResponse().reject(reason);
            return call;
        }

        // 3. Create the final call object
        return new JavaMethodToolCall(modelMessage, id, this, jsonArgs, convertedArgs);
    }

    @Override
    public Type getResponseType() {
        return JavaMethodToolResponse.class;
    }
}
