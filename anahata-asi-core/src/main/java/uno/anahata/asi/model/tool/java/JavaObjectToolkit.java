/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.model.tool.java;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.context.ContextProvider;
import uno.anahata.asi.model.core.Rebindable;
import uno.anahata.asi.model.tool.AbstractToolkit;
import uno.anahata.asi.tool.AiTool;
import uno.anahata.asi.tool.AiToolkit;
import uno.anahata.asi.tool.ToolContext;
import uno.anahata.asi.tool.ToolManager;

/**
 * A domain object that parses a Java class via reflection to build a complete,
 * self-contained Toolkit, including all its tools and parameters.
 * <p>
 * This class is the cornerstone of the V2's decoupled tool architecture,
 * separating the parsing of tool metadata from the management and execution of tools.
 * </p>
 * 
 * @author anahata-gemini-pro-2.5
 */
@Slf4j
@Getter
public class JavaObjectToolkit extends AbstractToolkit<JavaMethodTool> implements Rebindable {

    /** The singleton instance of the tool class. */
    private final Object toolInstance;

    /** 
     * A list of all declared methods (tools) for this toolkit. 
     * Non-final to support robust deserialization and hot-reloading.
     */
    private List<JavaMethodTool> tools;

    /**
     * Constructs a new JavaObjectToolkit by parsing the given class.
     * @param toolManager The parent ToolManager.
     * @param toolClass The class to parse.
     * @throws Exception if the class is not a valid toolkit or instantiation fails.
     */
    public JavaObjectToolkit(ToolManager toolManager, Class<?> toolClass) throws Exception {
        super(toolManager);
        
        AiToolkit toolkitAnnotation = toolClass.getAnnotation(AiToolkit.class);
        if (toolkitAnnotation == null) {
            throw new IllegalArgumentException("Class " + toolClass.getName() + " is not annotated with @AiToolkit.");
        }
        
        // Set parent fields
        this.name = toolClass.getSimpleName();
        this.description = toolkitAnnotation.value();
        this.defaultRetention = toolkitAnnotation.retention();
        
        try {
            this.toolInstance = toolClass.getDeclaredConstructor().newInstance();
            if (toolInstance instanceof ToolContext tc) {
                tc.setToolkit(this);
            }
        } catch (Exception e) {
            throw new IllegalArgumentException("Could not instantiate toolkit class: " + toolClass.getName() + ". It must be public and have a public no-arg constructor.", e);
        }

        this.tools = new ArrayList<>();
        for (Method method : getAllAnnotatedMethods(toolClass)) {
            AiTool toolAnnotation = method.getAnnotation(AiTool.class);
            if (toolAnnotation != null) {
                tools.add(new JavaMethodTool(this, toolInstance, method, toolAnnotation));
            }
        }
    }

    /**
     * {@inheritDoc}
     * Implementation details: Includes a null guard to handle circular dependencies 
     * during Kryo deserialization.
     */
    @Override
    public List<JavaMethodTool> getAllTools() {
        if (tools == null) {
            return Collections.emptyList();
        }
        return tools;
    }

    @Override
    public ContextProvider getContextProvider() {
         if (toolInstance instanceof ContextProvider cp) {
            return cp;
         } else {
             return null;
         }
    }

    @Override
    public void rebind() {
        log.info("Rebinding JavaObjectToolkit: {}", name);
        if (toolInstance instanceof ToolContext tc) {
            tc.setToolkit(this);
        }

        if (this.tools == null) {
            this.tools = new ArrayList<>();
        }

        // Hot-reload logic: Sync the tools list with the current class definition
        Map<String, Method> currentMethods = new HashMap<>();
        for (Method m : getAllAnnotatedMethods(toolInstance.getClass())) {
            AiTool toolAnnotation = m.getAnnotation(AiTool.class);
            if (toolAnnotation != null) {
                currentMethods.put(JavaMethodTool.buildMethodSignature(m), m);
            }
        }

        List<JavaMethodTool> toRemove = new ArrayList<>();
        for (JavaMethodTool tool : tools) {
            String signature = tool.getJavaMethodSignature();
            if (currentMethods.containsKey(signature)) {
                // Tool is still valid. It will lazily restore its Method object.
                currentMethods.remove(signature);
            } else {
                // Tool signature no longer exists in the class.
                log.warn("Tool signature no longer exists, marking for removal: {}", signature);
                toRemove.add(tool);
            }
        }

        tools.removeAll(toRemove);

        // Add new tools
        for (Map.Entry<String, Method> entry : currentMethods.entrySet()) {
            Method m = entry.getValue();
            AiTool toolAnnotation = m.getAnnotation(AiTool.class);
            try {
                log.info("Adding new tool discovered during rebind: {}", entry.getKey());
                tools.add(new JavaMethodTool(this, toolInstance, m, toolAnnotation));
            } catch (Exception e) {
                log.error("Failed to create new tool during rebind: " + entry.getKey(), e);
            }
        }
    }

    /**
     * Recursively finds all methods annotated with {@link AiTool} in the class hierarchy.
     * 
     * @param clazz The class to start the search from.
     * @return A list of annotated methods, with child methods taking precedence over parent methods.
     */
    private List<Method> getAllAnnotatedMethods(Class<?> clazz) {
        List<Method> annotatedMethods = new ArrayList<>();
        Set<String> signatures = new HashSet<>();
        Class<?> currentClass = clazz;
        while (currentClass != null && currentClass != Object.class) {
            for (Method method : currentClass.getDeclaredMethods()) {
                AiTool toolAnnotation = method.getAnnotation(AiTool.class);
                if (toolAnnotation != null) {
                    String signature = JavaMethodTool.buildMethodSignature(method);
                    if (signatures.add(signature)) {
                        annotatedMethods.add(method);
                    }
                }
            }
            currentClass = currentClass.getSuperclass();
        }
        return annotatedMethods;
    }
}
