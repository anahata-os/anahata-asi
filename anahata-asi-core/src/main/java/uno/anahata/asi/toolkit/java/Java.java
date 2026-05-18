package uno.anahata.asi.toolkit.java;

import uno.anahata.asi.toolkit.java.classpath.VeryPrettyClassPathPrinter;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.io.StringWriter;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.URI;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Properties;
import java.util.Set;
import java.util.TreeMap;
import java.util.concurrent.Callable;
import javax.swing.text.html.ImageView;
import javax.tools.Diagnostic;
import javax.tools.DiagnosticCollector;
import javax.tools.FileObject;
import javax.tools.ForwardingJavaFileManager;
import javax.tools.JavaCompiler;
import javax.tools.JavaFileManager;
import javax.tools.JavaFileObject;
import javax.tools.SimpleJavaFileObject;
import javax.tools.ToolProvider;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.AbstractAsiContainer;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.agi.AgiConfig;
import uno.anahata.asi.internal.TextUtils;
import uno.anahata.asi.agi.message.RagMessage;
import uno.anahata.asi.agi.resource.handle.PathHandle;
import uno.anahata.asi.agi.resource.handle.ResourceHandle;
import uno.anahata.asi.agi.resource.handle.StringHandle;
import uno.anahata.asi.agi.resource.handle.UrlHandle;
import uno.anahata.asi.agi.resource.view.AbstractResourceView;
import uno.anahata.asi.agi.resource.view.ResourceView;
import uno.anahata.asi.agi.resource.view.TextView;
import uno.anahata.asi.agi.tool.spi.java.JavaMethodTool;
import uno.anahata.asi.agi.tool.spi.java.JavaMethodToolResponse;
import uno.anahata.asi.agi.tool.AgiToolException;
import uno.anahata.asi.agi.tool.OnTheFlyAgiTool;
import uno.anahata.asi.agi.tool.ToolContext;
import uno.anahata.asi.agi.tool.AnahataToolkit;
import uno.anahata.asi.agi.tool.ToolManager;
import uno.anahata.asi.agi.tool.ToolResponseAttachment;
import uno.anahata.asi.agi.tool.spi.AbstractToolkit;
import uno.anahata.asi.agi.tool.spi.java.JavaMethodToolCall;
import uno.anahata.asi.agi.tool.spi.java.JavaObjectToolkit;
import uno.anahata.asi.agi.tool.AgiToolkit;
import uno.anahata.asi.agi.tool.AgiToolParam;
import uno.anahata.asi.agi.tool.AgiTool;

/**
 * A powerful toolkit for compiling and executing Java code dynamically within
 * the application's JVM. It provides a "hot-reload" capability by using a
 * child-first classloader and supports context-aware execution through the
 * {@link OnTheFlyAgiTool} base class.
 *
 * @author anahata
 */
@Slf4j
@AgiToolkit("Toolkit for compiling and executing java code, has a 'temp' HashMap for storing java objects across turns / tool calls and uses a child first classloader if additional classpath entries are provided")
public class Java extends AnahataToolkit {

    /**
     * A set of infrastructure classes that MUST always be loaded by the parent
     * classloader (the ASI engine) to preserve static state and ThreadLocal
     * context. This prevents "Identity Crisis" issues where a child-loaded
     * script cannot access the engine's context.
     */
    @Getter
    protected final Set<String> parentFirstClassess = new HashSet<>();
            
    /**
     * The base compiler and classloader classpath. Extra entries can be
     * provided at execution time. This serves as the foundation for both 
     * dynamic compilation and the child-first classloader logic.
     */
    public String defaultCompilerClasspath;
    
    /**
     * The transient printer used to generate token-optimized classpath manifests.
     * It is recreated lazily to ensure it is always available after deserialization.
     */
    protected transient VeryPrettyClassPathPrinter classpathPrinter;
    
    /**
     * Default constructor. Initializes the default classpath from the system's
     * "java.class.path" property.
     */
    public Java() {
        defaultCompilerClasspath = System.getProperty("java.class.path");
        registerParentFirstClass(OnTheFlyAgiTool.class);
        registerParentFirstClass(getClass());
        registerParentFirstClass(ToolContext.class);
        registerParentFirstClass(Agi.class);
        registerParentFirstClass(AgiConfig.class);
        registerParentFirstClass(ToolManager.class);
        registerParentFirstClass(AbstractToolkit.class);
        registerParentFirstClass(JavaObjectToolkit.class);
        registerParentFirstClass(JavaMethodTool.class);
        registerParentFirstClass(JavaMethodToolCall.class);
        registerParentFirstClass(JavaMethodToolResponse.class);
        registerParentFirstClass(ToolResponseAttachment.class);
        registerParentFirstClass(AgiToolException.class);
        registerParentFirstClass(uno.anahata.asi.agi.resource.ResourceManager.class);
        registerParentFirstClass(uno.anahata.asi.agi.resource.Resource.class);
        registerParentFirstClass(uno.anahata.asi.agi.resource.RefreshPolicy.class);
        registerParentFirstClass(uno.anahata.asi.agi.context.ContextPosition.class);
        registerParentFirstClass(ResourceView.class);
        registerParentFirstClass(AbstractResourceView.class);
        registerParentFirstClass(TextView.class);
        registerParentFirstClass(ImageView.class);
        registerParentFirstClass(ResourceHandle.class);
        registerParentFirstClass(PathHandle.class);
        registerParentFirstClass(UrlHandle.class);
        registerParentFirstClass(StringHandle.class);
        registerParentFirstClass(AbstractAsiContainer.class);
        log.info("Java toolkit instantiated:");
    }
    
    /** 
     * {@inheritDoc} 
     * <p>Empty implementation as per architectural refinement. 
     * Initialization logic is moved to {@link #postActivate()} and {@link #setDefaultClasspath(String)}.</p> 
     */
    @Override
    public void initialize() {
        log.info("initialize(): parentFirstClasses: " + parentFirstClassess);
    }

    /** 
     * {@inheritDoc} 
     * <p>Synchronizes the classpath printer with the current default classpath 
     * after the toolkit has been activated or deserialized.</p> 
     */
    @Override
    public void postActivate() {
        getClasspathPrinter().setRaw(defaultCompilerClasspath);
    }
    
    /**
     * {@inheritDoc}
     */
    @Override
    public List<String> getSystemInstructions() throws Exception {
        StringBuilder sb = new StringBuilder();
        sb.append(" Java Toolkit Instructions: \n");
        sb.append("When using `compileAndExecute`, your class should be **public**, named **Anahata**, extend `" + getConcreteClassModelShouldExtend().getName() + "`, have no package declaration and implement the call() method of " + Callable.class.getName() + "<Object>. ");
        sb.append("This provides the following helper methods for a rich, context-aware execution:\n\n");

        sb.append(" Available Methods that you can use within the code you write:\n");

        sb.append("**Inherited from " + getConcreteClassModelShouldExtend().getName() + "**:\n");
        appendMethods(sb, getConcreteClassModelShouldExtend());

        sb.append("\n Multi-threading and Thread Safety:\n");
        //sb.append("The `log()`, `error()`, and `addAttachment()` methods rely on a thread-local context and will fail if called from a subthread or the EDT (Event Dispatch Thread).\n");
        sb.append("- To access the context from another thread, capture it in a final variable: `final ToolContext ctx = getToolContext();` and use `ctx.log(...)`, `ctx.error(...)`, `ctx.addAttachment(...)`, etc.\n");

        sb.append("\nAbout the attribute maps:"
                + "\n- The Turn attribute map is for sharing data across tool calls within the same turn. (what in Servlet terms you could call 'request scoped'). Gets serialized."
                + "\n The Session Map is for this agi session. Anything stored in this map during one turn will be available in subsequent turns (or subsquente tool calls withing the same turn). Gets serialized "
                + "\n The ASI Container map is shared across sessions (agis) in the current AsiContainer (a given JVM could be running multiple ASI Containers). Currently it does not get serialized."
                + "\n The Application Map is a static field shared across all sessions (agis) of all ASI Containers running in this jvm\n");

        sb.append("\nAbout attachments: be careful attaching attachments as the supported mime types vary on a model basis .\n");

        sb.append("\n Example:\n");
        sb.append("```java\n");
        sb.append("import ").append(getConcreteClassModelShouldExtend().getName()).append(";\n");
        sb.append("\n");
        sb.append("public class Anahata extends ").append(getConcreteClassModelShouldExtend().getSimpleName()).append("{\n");
        sb.append("    @Override\n");
        sb.append("    public Object call() throws Exception {\n");
        sb.append("        log(\"Starting script execution...\");\n");
        sb.append("        \n");
        sb.append("        // Perform logic\n");
        sb.append("        String result = \"Hello from AnahataTool!\";\n");
        sb.append("        log(\"Result: \" + result);\n");
        sb.append("        \n");
        // You can also add errors or attachments
        // error("Something went wrong");
        // addAttachment(data, "image/png");
        sb.append("        \n");
        sb.append("        return result;\n");
        sb.append("    }\n");
        sb.append("}\n");
        sb.append("```\n");
        sb.append("\n");
        sb.append("\n");
        sb.append("**JVM System Properties**:\n");
        sb.append(getSystemProperties());

        return Collections.singletonList(sb.toString());
    }

    /**
     * Registers a class and all its superclasses and interfaces to be loaded
     * by the parent classloader.
     *
     * @param c The class to register.
     */
    public final void registerParentFirstClass(Class<?> c) {
        if (c == null || c.equals(Object.class) || parentFirstClassess.contains(c.getName())) {
            return;
        }
        parentFirstClassess.add(c.getName());
        registerParentFirstClass(c.getSuperclass());
        for (Class<?> iface : c.getInterfaces()) {
            registerParentFirstClass(iface);
        }
    }

    /**
     * The class the model should extend when generating Agi tools. 
     * <p>In this base implementation, it returns {@link OnTheFlyAgiTool}, which 
     * provides the necessary context anchors (log, error, etc.) for the script.</p>
     * @return the base AgiTool class.
     */
    protected Class<? extends ToolContext> getConcreteClassModelShouldExtend() {
        return OnTheFlyAgiTool.class;
    }


    /**
     * Gets the current default classpath used for compilation and class
     * loading.
     *
     * @return The full default classpath string.
     */
    @AgiTool("The full default classpath for compiling java code and for class loading")
    public String getDefaultClasspath() {
        return defaultCompilerClasspath;
    }

    /**
     * Sets the default classpath for the compiler and classloader.
     *
     * @param defaultCompilerClasspath The new default classpath string.
     */
    @AgiTool("Sets the default classpath for the compiler and classloader")
    public void setDefaultClasspath(@AgiToolParam("The default classpath for all code compiled by the Java toolkit") String defaultCompilerClasspath) {
        if (!Objects.equals(this.defaultCompilerClasspath, defaultCompilerClasspath)) {
            this.defaultCompilerClasspath = defaultCompilerClasspath;
            getClasspathPrinter().setRaw(defaultCompilerClasspath);
        }
    }

    /**
     * Returns a token-efficient, pretty-printed version of the default
     * classpath.
     * <p>Implementation note: This leverages lexical grouping and version 
     * promotion to keep the classpath manifest small.</p>
     * @return The pretty-printed classpath string.
     */
    public String getPrettyPrintedDefaultClasspath() {
        return getClasspathPrinter().getPretty();
    }

    /**
     * Gets the lazily-initialized classpath printer.
     * @return The {@link VeryPrettyClassPathPrinter} instance.
     */
    protected final VeryPrettyClassPathPrinter getClasspathPrinter() {
        if (classpathPrinter == null) {
            classpathPrinter = createClassPathPrinter();
            classpathPrinter.setRaw(defaultCompilerClasspath);
        }
        return classpathPrinter;
    }

    /**
     * Factory method to create the specialized classpath printer.
     * @return A new {@link VeryPrettyClassPathPrinter} instance.
     */
    protected VeryPrettyClassPathPrinter createClassPathPrinter() {
        return new VeryPrettyClassPathPrinter();
    }

    /**
     * {@inheritDoc}
     * <p>Adds session/container map keys and the abbreviated classpath manifest 
     * to the RAG message to provide the model with awareness of its persistent 
     * state and available libraries.</p>
     */
    @Override
    public void populateMessage(RagMessage ragMessage) throws Exception {
        String ragText = 
                 "\nSession (Agi) map keys (shared across turns, persistent): " + getSessionMap().keySet()
                + "\nASI Container map keys (shared across sessions, not persistent today): " + getAsiContainerMap().keySet()
                + "\nApplication map keys (shared across ASI Containers, not persistent today): " + getApplicationMap().keySet()
                + "\nDefault Compiler and ClassLoader Classpath (abbreviated):\n" + getPrettyPrintedDefaultClasspath();
        ragMessage.addTextPart(ragText);
    }

    /**
     * Appends the signatures of all declared methods of a class to a
     * StringBuilder, filtering out standard Object methods and internal lambda/abstract 
     * cruft.
     * @param sb The StringBuilder to append to.
     * @param clazz The class to inspect.
     */
    protected void appendMethods(StringBuilder sb, Class<?> clazz) {

        for (Method m : clazz.getMethods()) {
            if (!m.getDeclaringClass().equals(Object.class)) {
                String methodString = JavaMethodTool.buildMethodSignature(m);
                if (!methodString.contains("anahata") && !methodString.contains("lambda$") && !methodString.contains("abstract")) {
                    sb.append("- `").append(methodString).append("`\n");
                }
            }
        }
        
        sb.append("\nInternal anahata Agi container apis. Mostly for debugging / troubleshooting. Don't guess members on anahata types. If you think you need to use them or you think they could help you complete "
                + "a task, discover their members first.\n");
        
        for (Method m : clazz.getMethods()) {
            if (!m.getDeclaringClass().equals(Object.class)) {
                String methodString = JavaMethodTool.buildMethodSignature(m);
                if (methodString.contains("anahata") && !methodString.contains("etToolkit(") ) {
                    sb.append("- `").append(methodString).append("`\n");
                }
            }
        }
    }

    /**
     * Compiles Java source code into a Class object using the system's Java
     * compiler.
     *
     * @param sourceCode The Java source code to compile.
     * @param className The fully qualified name of the class.
     * @param extraClassPath Additional classpath entries to include.
     * @param compilerOptions Additional options for the Java compiler.
     * @return The compiled Class object.
     * @throws ClassNotFoundException if the class cannot be found after
     * compilation.
     * @throws NoSuchMethodException if a required method is missing.
     * @throws IllegalAccessException if access to a member is denied.
     * @throws InvocationTargetException if a method invocation fails.
     */
    //@AgiTool("Compiles the source code of a java class with the default compiler classpath")
    public Class compile(
            @AgiToolParam(value = "The source code", rendererId = "java") String sourceCode,
            @AgiToolParam("The class name") String className,
            @AgiToolParam(value = "Additional classpath entries", required = false) String extraClassPath,
            @AgiToolParam(value = "Additional compiler options", required = false) String[] compilerOptions)
            throws ClassNotFoundException, NoSuchMethodException, IllegalAccessException, InvocationTargetException {

        final ToolContext ctx = getToolContext();

        log("Compiling class: " + className);
        JavaCompiler compiler = ToolProvider.getSystemJavaCompiler();

        if (compiler == null) {
            throw new RuntimeException("JDK required (running on JRE).");
        }

        String sourceFile = className + ".java";
        JavaFileObject source = new SimpleJavaFileObject(URI.create("string:///" + sourceFile), JavaFileObject.Kind.SOURCE) {
            @Override
            public CharSequence getCharContent(boolean ignoreEncodingErrors) {
                return sourceCode;
            }
        };

        DiagnosticCollector<JavaFileObject> diagnostics = new DiagnosticCollector<>();

        ForwardingJavaFileManager<JavaFileManager> fileManager = new ForwardingJavaFileManager<JavaFileManager>(compiler.getStandardFileManager(diagnostics, null, null)) {
            private final Map<String, ByteArrayOutputStream> compiledClasses = new HashMap<>();

            @Override
            public JavaFileObject getJavaFileForOutput(JavaFileManager.Location location, String className, JavaFileObject.Kind kind, FileObject sibling) throws IOException {
                if (kind == JavaFileObject.Kind.CLASS) {
                    ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
                    compiledClasses.put(className, outputStream);
                    return new SimpleJavaFileObject(URI.create("mem:///" + className.replace('.', '/') + ".class"), JavaFileObject.Kind.CLASS) {
                        @Override
                        public OutputStream openOutputStream() throws IOException {
                            return outputStream;
                        }
                    };
                }
                return super.getJavaFileForOutput(location, className, kind, sibling);
            }

            public Map<String, byte[]> getCompiledClasses() {
                Map<String, byte[]> result = new HashMap<>();
                for (Map.Entry<String, ByteArrayOutputStream> entry : compiledClasses.entrySet()) {
                    result.put(entry.getKey(), entry.getValue().toByteArray());
                }
                return result;
            }
        };

        if (extraClassPath != null) {
            log("Including extra classpath entries: " + extraClassPath.split(File.pathSeparator).length);
            log.info("extraClassPath: {} entries:\n{}", extraClassPath.split(File.pathSeparator).length, extraClassPath);
        }

        String classpath = defaultCompilerClasspath;
        if (extraClassPath != null && !extraClassPath.isEmpty()) {
            // CRITICAL FIX: Prepend extraClassPath to ensure hot-reloaded classes take precedence
            classpath = extraClassPath + File.pathSeparator + classpath;
        }

        log("Total compilation classpath entries: " + classpath.split(File.pathSeparator).length);
        if (compilerOptions != null) {
            log.info("compilerOptions:", Arrays.asList(compilerOptions));
        }

        List<String> options = new ArrayList<>(Arrays.asList("-classpath", classpath));

        if (compilerOptions != null) {
            options.addAll(Arrays.asList(compilerOptions));
        }

        // START of new code
        boolean hasVersionFlag = false;
        if (compilerOptions != null) {
            for (String option : compilerOptions) {
                if (option.equals("--release") || option.equals("-source") || option.equals("-target")) {
                    hasVersionFlag = true;
                    break;
                }
            }
        }

        if (!hasVersionFlag) {
            String runtimeVersion = System.getProperty("java.specification.version");
            log.info("No explicit Java version compiler flag found. Defaulting to --release {}.", runtimeVersion);
            log("No explicit Java version compiler flag found. Defaulting to --release " + runtimeVersion);
            options.add("--release");
            options.add(runtimeVersion);
        }
        // END of new code

        if (!options.contains("-proc:none")) {
            options.add("-proc:none");
        }
        log.debug("Compiling with options: \n{}", options);

        StringWriter writer = new StringWriter();
        JavaCompiler.CompilationTask task = compiler.getTask(writer, fileManager, diagnostics, options, null, Collections.singletonList(source));
        boolean success = task.call();
        log.info("Compilation Success: {}", success);

        if (!success) {
            StringBuilder error = new StringBuilder("Compiler: " + compiler + "\n");
            error.append("Task:").append(task).append("\n");
            error.append("Diagnostics: \n");
            for (Diagnostic<? extends JavaFileObject> d : new ArrayList<>(diagnostics.getDiagnostics())) {
                log.warn("Compiler Diagnostic: {}", d);
                error.append(d.toString()).append("\n");
            }
            System.out.println(error);
            throw new java.lang.RuntimeException("Compilation error:\n" + error.toString());
        }

        Map<String, byte[]> compiledClasses = ((Map<String, byte[]>) fileManager.getClass().getMethod("getCompiledClasses").invoke(fileManager));

        List<URL> urlList = new ArrayList<>();
        if (extraClassPath != null && !extraClassPath.isEmpty()) {
            String[] pathElements = extraClassPath.split(File.pathSeparator);
            for (String element : pathElements) {
                try {
                    urlList.add(new File(element).toURI().toURL());
                } catch (Exception e) {
                    log.warn("Invalid classpath entry: {}", element, e);
                }
            }
        }

        URLClassLoader reloadingClassLoader = new URLClassLoader(urlList.toArray(new URL[0]), Thread.currentThread().getContextClassLoader()) {
            @Override
            protected Class<?> loadClass(String name, boolean resolve) throws ClassNotFoundException {
                synchronized (getClassLoadingLock(name)) {
                    // 1. Check if class is already loaded by this loader
                    Class<?> c = findLoadedClass(name);
                    if (c == null) {
                        // 2. PARENT-FIRST for critical infrastructure:
                        // These classes MUST maintain a single identity across all loaders
                        // to preserve ThreadLocals and static context anchors.
                        if (parentFirstClassess.contains(name)) {
                            ctx.log("Delegating infrastructure class to parent: " + name);
                            return super.loadClass(name, resolve);
                        }

                        // 3. Check for our in-memory compiled class first (the "hot-reload" part for Anahata.java)
                        byte[] bytes = compiledClasses.get(name);
                        if (bytes != null) {
                            log.info("Hot-reloading in-memory class: {}", name);
                            c = defineClass(name, bytes, 0, bytes.length);
                        } else {
                            try {
                                // 4. CHILD-FIRST: Try to find the class in our own URLs (e.g., target/classes)
                                c = findClass(name);
                                log.info("Loaded class from extraClassPath (Child-First): {}" + name);
                            } catch (ClassNotFoundException e) {
                                // 5. FALLBACK: Ask the toolkit if it can find the bytes elsewhere (e.g. MR-JARs)
                                byte[] fallbackBytes = findClassFallbackBytes(name);
                                if (fallbackBytes != null) {
                                    ctx.log("Loaded class from Fallback Bridge: " + name);
                                    c = defineClass(name, fallbackBytes, 0, fallbackBytes.length);
                                } else {
                                    // 6. PARENT-LAST: If not found, delegate to the parent classloader.
                                    c = super.loadClass(name, resolve);
                                }
                            }
}
                    }
                    if (resolve) {
                        resolveClass(c);
                    }
                    return c;
                }
            }
        };

        return reloadingClassLoader.loadClass(className);
    }

    /**
     * A hook for subclasses to provide class bytes if the standard loading flow fails.
     * <p>This is used by the NetBeans implementation (NbJava) to bridge 
     * Multi-Release JAR classes into the memory-based loader.</p>
     * @param name The FQN of the class.
     * @return The class bytes, or null if not found.
     */
    protected byte[] findClassFallbackBytes(String name) {
        return null;
    }

    /**
     * Compiles and executes a Java class named 'Anahata' on the application's
* JVM. The class must extend {@link OnTheFlyAgiTool} and implement
     * {@link Callable}.
     *
     * @param sourceCode The Java source code to compile and execute.
     * @param extraClassPath Additional classpath entries.
     * @param compilerOptions Additional compiler options.
     * @return The result of the execution.
     * @throws Exception if compilation or execution fails.
     */
    @AgiTool(
            value = "Compiles and executes the 'Anahata' class on the application's JVM.\n"
            + "The class should:\n"
            + "- be public, \n"
            + "- have no package declaration, \n"
            + "- extend AgiTool (or any subtype) and \n"
            + "- implement the call method of java.util.concurrent.Callable<Object>.\n"
    )
    public Object compileAndExecute(
            @AgiToolParam(value = "Source code of the 'Anahata' class.", rendererId = "java") String sourceCode,
            @AgiToolParam(value = "Optional Compiler's additional classpath entries separated with File.pathSeparator. These will be first in the final compiler's classpath and the child-first set of the ClassLoader's classpath", required = false) String extraClassPath,
            @AgiToolParam(value = "Optional Compiler's options.", required = false) String[] compilerOptions) throws Exception {

        log.info("executeJavaCode: \nsource={}", sourceCode);
        log.info("executeJavaCode: \nextraCompilerClassPath={}", extraClassPath);

        Class c = compile(sourceCode, "Anahata", extraClassPath, compilerOptions);

        // CRITICAL FIX: Use setAccessible(true) to allow instantiation even if the class/constructor is not public.
        var constructor = c.getDeclaredConstructor();
        constructor.setAccessible(true);
        Object o = constructor.newInstance();

        // Onboard the tool instance into the current context
        if (o instanceof ToolContext tc) {
            log("Onboarding tool instance: " + c.getName());
            tc.setToolkit(this.toolkit);
        } else {
            log("Warning: Compiled class does not extend ToolContext. Identity propagation disabled.");
        }

        if (o instanceof Callable callable) {
            log.info("Calling call() method on Callable (or AnahataTool)");
            return callable.call();
        } else {
            throw new AgiToolException("Source file should extend AnahataTool or implement java.util.Callable");
        }
    }

    /**
     * Represents a node in the hierarchical tree of system properties.
     */
    private static class SystemPropertyNode {

        /**
         * The segment name of this node (e.g., "java").
         */
        String segment;
        /**
         * The full dot-separated path to this node (e.g., "java.vendor").
         */
        String fullPath;
        /**
         * The value of the property, if this is a leaf node.
         */
        Object value;
        /**
         * The children of this node, keyed by their segment name.
         */
        Map<String, SystemPropertyNode> children = new TreeMap<>();

        /**
         * Constructs a new node.
         *
         * @param segment The segment name.
         * @param fullPath The full path.
         */
        SystemPropertyNode(String segment, String fullPath) {
            this.segment = segment;
            this.fullPath = fullPath;
        }

        /**
         * Checks if this node is a leaf (has no children).
         *
         * @return true if it's a leaf.
         */
        boolean isLeaf() {
            return children.isEmpty();
        }
    }

    /**
     * Generates a token-efficient, hierarchical representation of all JVM
     * system properties (excluding the classpath itself).
     * @return A formatted string of system properties.
     * @throws Exception if an error occurs.
     */
    public String getSystemProperties() throws Exception {
        Properties props = System.getProperties();
        SystemPropertyNode root = new SystemPropertyNode("", "");

        for (Object keyObj : props.keySet()) {
            String key = (String) keyObj;
            if (key.startsWith("java.class.path")) {
                continue;
            }

            String[] parts = key.split("\\.");
            SystemPropertyNode current = root;
            StringBuilder pathAcc = new StringBuilder();
            for (String part : parts) {
                if (pathAcc.length() > 0) {
                    pathAcc.append(".");
                }
                pathAcc.append(part);
                current = current.children.computeIfAbsent(part, k -> new SystemPropertyNode(k, pathAcc.toString()));
            }
            current.value = props.get(key);
        }

        StringBuilder sb = new StringBuilder();
        // Process top-level groups
        for (SystemPropertyNode child : root.children.values()) {
            renderSysProp(sb, child, 0);
        }
        return sb.toString();
    }

    /**
     * Recursively renders a system property node and its children into a
     * formatted string.
     * @param sb The StringBuilder to append to.
     * @param node The node to render.
     * @param indent The current indentation level.
     */
    private void renderSysProp(StringBuilder sb, SystemPropertyNode node, int indent) {
        String tabs = "  ".repeat(indent);

        // Collapse logic: if a node has exactly one child and no value, merge with child
        SystemPropertyNode current = node;
        String displayLabel = current.segment;
        while (current.children.size() == 1 && current.value == null) {
            SystemPropertyNode next = current.children.values().iterator().next();
            displayLabel += "." + next.segment;
            current = next;
        }

        if (current.isLeaf()) {
            // It's a single property or a fully collapsed path
            sb.append(tabs).append("- `").append(displayLabel).append("`: ")
                     .append(TextUtils.formatValue(current.value)).append("\n");
        } else {
            // It's a group
            // User requested full prefix in the header
            sb.append(tabs).append("**").append(current.fullPath).append("**:\n");

            if (current.value != null) {
                // If the prefix node itself has a value (e.g. java.vendor)
                sb.append(tabs).append("  - `value`: ").append(TextUtils.formatValue(current.value)).append("\n");
            }

            for (SystemPropertyNode child : current.children.values()) {
                renderSysProp(sb, child, indent + 1);
            }
        }
    }
}
