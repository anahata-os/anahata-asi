/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.agi.tool.spi.java;

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
import uno.anahata.asi.agi.context.ContextProvider;
import uno.anahata.asi.persistence.Rebindable;
import uno.anahata.asi.agi.tool.spi.AbstractToolkit;
import uno.anahata.asi.agi.tool.AnahataToolkit;
import uno.anahata.asi.agi.tool.ToolContext;
import uno.anahata.asi.agi.tool.ToolManager;
import uno.anahata.asi.agi.tool.AgiToolkit;
import uno.anahata.asi.agi.tool.AgiTool;

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

    /** 
     * The fully qualified name of the toolkit class. 
     * This is the "Ground Truth" used to restore the instance after deserialization.
     */
    private final String toolkitClassName;

    /** 
     * The singleton instance of the tool class. 
     * Non-final to support circular reference resolution during Kryo deserialization.
     */
    private Object toolkitInstance;

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
        this.toolkitClassName = toolClass.getName();
        
        AgiToolkit toolkitAnnotation = toolClass.getAnnotation(AgiToolkit.class);
        if (toolkitAnnotation == null) {
            throw new IllegalArgumentException("Class " + toolClass.getName() + " is not annotated with @AgiToolkit.");
        }
        
        // Set parent fields
        this.name = toolClass.getSimpleName();
        this.description = "fqn:" + toolClass.getName() + "\n" + toolkitAnnotation.value();
        
        int maxDepth = toolkitAnnotation.maxDepth();
        if (maxDepth == 0) {
            throw new IllegalArgumentException("Toolkit '" + name + "' cannot have maxDepth=0. Use -1 to inherit from config or >= 1 to live.");
        }
        this.defaultMaxDepth = maxDepth;
        
        try {
            this.toolkitInstance = toolClass.getDeclaredConstructor().newInstance();
            if (toolkitInstance instanceof ToolContext tc) {
                log.debug("Onboarding {}", tc);
                tc.setToolkit(this);
            }
        } catch (Exception e) {
            log.error("Exception initializing toolkit instance. Rethrowing", e);
            throw new IllegalArgumentException("Could not initialize toolkit class: " + toolClass.getName() + ". It must be public and have a public no-arg constructor.", e);
        }

        this.tools = new ArrayList<>();
        for (Method method : getAllAnnotatedMethods(toolClass)) {
            AgiTool toolAnnotation = method.getAnnotation(AgiTool.class);
            if (toolAnnotation != null) {
                tools.add(new JavaMethodTool(this, method, toolAnnotation));
            }
        }
    }

    /**
     * {@inheritDoc} 
     * <p>
     * Implementation details: 
     * Delegates to the underlying implementation if it is an {@link AnahataToolkit}.
     * </p>
     */
    @Override
    public void initialize() {
        if (toolkitInstance instanceof AnahataToolkit at) {
            log.info("Initializing toolkit implementation: {}", at.getName());
            at.initialize();
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
         if (toolkitInstance instanceof ContextProvider cp) {
            return cp;
         } else {
             return null;
         }
    }

    @Override
    public void rebind() {
        log.debug("Rebinding JavaObjectToolkit: {}", name);
        
        // 1. Restore Instance if missing (transient recovery)
        if (toolkitInstance == null && toolkitClassName != null) {
            try {
                log.debug("Restoring toolkit instance for class: {}", toolkitClassName);
                Class<?> clazz = Class.forName(toolkitClassName);
                this.toolkitInstance = clazz.getDeclaredConstructor().newInstance();
            } catch (Exception e) {
                log.error("Failed to restore toolkit instance during rebind: " + toolkitClassName, e);
            }
        }

        if (toolkitInstance instanceof ToolContext tc) {
            tc.setToolkit(this);
        }

        if (this.tools == null) {
            this.tools = new ArrayList<>();
        }

        // 2. Hot-reload logic: Sync the tools list with the current class definition.
        // We only proceed if the instance is available to avoid NPEs during circularity resolution.
        if (toolkitInstance != null) {
            Map<String, Method> currentMethods = new HashMap<>();
            for (Method m : getAllAnnotatedMethods(toolkitInstance.getClass())) {
                AgiTool toolAnnotation = m.getAnnotation(AgiTool.class);
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
                AgiTool toolAnnotation = m.getAnnotation(AgiTool.class);
                try {
                    log.info("Adding new tool discovered during rebind: {}", entry.getKey());
                    tools.add(new JavaMethodTool(this, m, toolAnnotation));
                } catch (Exception e) {
                    log.error("Failed to create new tool during rebind: " + entry.getKey(), e);
                }
            }
        }
    }

    /**
     * Recursively finds all methods annotated with {@link AgiTool} in the class hierarchy.
     * <p>
     * <b>Discovery Pattern:</b> This method traverses up the superclass hierarchy, ensuring 
     * that child overrides take precedence over parent declarations.
     * </p>
     * 
     * @param clazz The class to start the search from.
     * @return A list of annotated methods.
     */
    public static List<Method> getAllAnnotatedMethods(Class<?> clazz) {
        List<Method> annotatedMethods = new ArrayList<>();
        Set<String> signatures = new HashSet<>();
        Class<?> currentClass = clazz;
        while (currentClass != null && currentClass != Object.class) {
            for (Method method : currentClass.getDeclaredMethods()) {
                AgiTool toolAnnotation = method.getAnnotation(AgiTool.class);
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

    
    /** 
     * {@inheritDoc} 
     * <p>
     * Implementation details: 
     * 1. Warm up tools: triggers restoration of reflection methods and parameters.
     * 2. Delegates to the underlying implementation if it is an {@link AnahataToolkit}.
     * </p> 
     */
    @Override
    public void postActivate() {
        log.debug("Post-activating Java toolkit: {}", name);
        // 1. Warm up tools to prevent race conditions during UI rendering
        for (JavaMethodTool tool : getAllTools()) {
            try {
                tool.postActivate();
            } catch (Exception e) {
                log.error("Failed to postActivate tool: {}", tool.getName(), e);
            }
        }
        
        // 2. Delegate to implementation
        if (toolkitInstance instanceof AnahataToolkit at) {
            at.postActivate();
        }
    }
}
