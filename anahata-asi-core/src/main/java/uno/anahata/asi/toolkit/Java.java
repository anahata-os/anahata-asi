package uno.anahata.asi.toolkit;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.io.StringWriter;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Parameter;
import java.net.URI;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.TreeMap;
import java.util.concurrent.Callable;
import javax.tools.Diagnostic;
import javax.tools.DiagnosticCollector;
import javax.tools.FileObject;
import javax.tools.ForwardingJavaFileManager;
import javax.tools.JavaCompiler;
import javax.tools.JavaFileManager;
import javax.tools.JavaFileObject;
import javax.tools.SimpleJavaFileObject;
import javax.tools.ToolProvider;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.chat.Chat;
import uno.anahata.asi.internal.ClasspathPrinter;
import uno.anahata.asi.internal.TextUtils;
import uno.anahata.asi.model.core.RagMessage;
import uno.anahata.asi.model.core.TextPart;
import uno.anahata.asi.model.tool.java.JavaMethodTool;
import uno.anahata.asi.model.tool.java.JavaMethodToolResponse;
import uno.anahata.asi.tool.AiTool;
import uno.anahata.asi.tool.AiToolException;
import uno.anahata.asi.tool.AiToolParam;
import uno.anahata.asi.tool.AiToolkit;
import uno.anahata.asi.tool.AnahataTool;
import uno.anahata.asi.tool.ToolContext;
import uno.anahata.asi.tool.AnahataToolkit;

/**
 * A powerful toolkit for compiling and executing Java code dynamically within
 * the application's JVM. It provides a "hot-reload" capability by using a
 * child-first classloader and supports context-aware execution through the
 * {@link AnahataTool} base class.
 *
 * @author anahata
 */
@Slf4j
@AiToolkit("Toolkit for compiling and executing java code, has a 'temp' HashMap for storing java objects across turns / tool calls and uses a child first classloader if additional classpath entries are provided")
public class Java extends AnahataToolkit {

    /**
     * A set of infrastructure classes that MUST always be loaded by the parent
     * classloader (the ASI engine) to preserve static state and ThreadLocal
     * context. This prevents "Identity Crisis" issues where a child-loaded
     * script cannot access the engine's context.
     */
    private static final Set<String> PARENT_FIRST_CLASSES = Set.of(
            AnahataTool.class.getName(),
            ToolContext.class.getName(),
            JavaMethodToolResponse.class.getName(),
            uno.anahata.asi.model.tool.ToolResponseAttachment.class.getName(),
            uno.anahata.asi.tool.AiToolException.class.getName()
    );

    /**
     * The base compiler and classloader classpath. Extra entries can be
     * provided at execution time.
     */
    public String defaultCompilerClasspath;

    /**
     * {@inheritDoc}
     */
    @Override
    public List<String> getSystemInstructions() throws Exception {
        StringBuilder sb = new StringBuilder();
        sb.append("### Java Toolkit Instructions: \n");
        sb.append("When using `compileAndExecute`, your class should be **public**, named **Anahata**, extend `" + AnahataTool.class.getName() + "`, have no package declaration and implement the call method of Callable<Object>. ");
        sb.append("This provides the following helper methods for a rich, context-aware execution:\n\n");

        sb.append("#### Available Methods that you can use within the code you write:\n");
        sb.append("- **Inherited from AnahataTool**:\n");
        appendMethods(sb, AnahataTool.class);
        sb.append("- **Inherited from ToolContext**:\n");
        appendMethods(sb, ToolContext.class);

        sb.append("\n#### Multi-threading and Thread Safety:\n");
        sb.append("The `log()`, `error()`, and `addAttachment()` methods rely on a thread-local context and will fail if called from a subthread or the EDT (Event Dispatch Thread).\n");
        sb.append("- To access the context from another thread, capture it in a final variable: `final ToolContext ctx = getToolContext();` and use `ctx.log(...)`, `ctx.error(...)`, etc.\n");

        sb.append("\nAbout the maps: the Session Map is for this session only (chat scoped), the ASI Container map is shared across sessions (chats) and the application map is a static field so shared across any all sessions of all containers running in this jvm\n");
        sb.append("\nAbout the attachments: at the time of this release (only tested with gemini-3-flash) only pdf, text and image attachments are supported\n");

        sb.append("\n#### Example:\n");
        sb.append("```java\n");
        sb.append("import " + AnahataTool.class.getName() + ";\n");
        sb.append("\n");
        sb.append("public class Anahata extends AnahataTool {\n");
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
     * Default constructor. Initializes the default classpath from the system's
     * "java.class.path" property.
     */
    public Java() {
        defaultCompilerClasspath = System.getProperty("java.class.path");
        log.info("Java toolkit instantiated:");
    }

    /**
     * Gets the current default classpath used for compilation and class
     * loading.
     *
     * @return The full default classpath string.
     */
    @AiTool("The full default classpath for compiling java code and for class loading")
    public String getDefaultClasspath() {
        return defaultCompilerClasspath;
    }

    /**
     * Sets the default classpath for the compiler and classloader.
     *
     * @param defaultCompilerClasspath The new default classpath string.
     */
    @AiTool("Sets the default classpath for the compiler and classloader")
    public void setDefaultClasspath(@AiToolParam("The default classpath for all code compiled by the Java toolkit") String defaultCompilerClasspath) {
        this.defaultCompilerClasspath = defaultCompilerClasspath;
    }

    /**
     * Returns a token-efficient, pretty-printed version of the default
     * classpath.
     *
     * @return The pretty-printed classpath string.
     */
    public String getPrettyPrintedDefaultClasspath() {
        return ClasspathPrinter.prettyPrint(defaultCompilerClasspath);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void populateMessage(RagMessage ragMessage) throws Exception {
        String ragText = "\nSession map keys (shared across turns): " + getSessionMap().keySet()
                + "\nASI Container map keys (shared across sessions): " + getContainerMap().keySet()
                + "\nApplication map keys (shared across containers): " + getApplicationMap().keySet()
                + "\nDefault Compiler and ClassLoader Classpath (abbreviated):\n" + getPrettyPrintedDefaultClasspath();
        ragMessage.addTextPart(ragText);
    }

    /**
     * Appends the signatures of all declared methods of a class to a
     * StringBuilder.
     *
     * @param sb The StringBuilder to append to.
     * @param clazz The class to inspect.
     */
    private void appendMethods(StringBuilder sb, Class<?> clazz) {

        for (Method m : clazz.getDeclaredMethods()) {
            String methodString = JavaMethodTool.buildMethodSignature(m);
            if (!methodString.contains("anahata")) {
                sb.append("- `").append(methodString).append("`\n");
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
    //@AiTool("Compiles the source code of a java class with the default compiler classpath")
    public Class compile(
            @AiToolParam(value = "The source code", rendererId = "java") String sourceCode,
            @AiToolParam("The class name") String className,
            @AiToolParam(value = "Additional classpath entries", required = false) String extraClassPath,
            @AiToolParam(value = "Additional compiler options", required = false) String[] compilerOptions)
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
            error.append("Task:" + task + "\n");
            error.append("Diagnostics:\n");
            for (Diagnostic<? extends JavaFileObject> diagnostic : diagnostics.getDiagnostics()) {
                error.append(diagnostic.toString()).append("\n");
                log.info("Compiler Diagnostic: {}", diagnostic.toString());
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
                        if (PARENT_FIRST_CLASSES.contains(name)) {
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
                                // 5. PARENT-LAST: If not found, delegate to the parent classloader.
                                c = super.loadClass(name, resolve);
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
     * Compiles and executes a Java class named 'Anahata' on the application's
     * JVM. The class must extend {@link AnahataTool} and implement
     * {@link Callable}.
     *
     * @param sourceCode The Java source code to compile and execute.
     * @param extraClassPath Additional classpath entries.
     * @param compilerOptions Additional compiler options.
     * @return The result of the execution.
     * @throws Exception if compilation or execution fails.
     */
    @AiTool(
            value = "Compiles and executes a Java class named 'Anahata' on the application's JVM.\n"
            + "The class should:\n"
            + "- be public, \n"
            + "- have no package declaration, \n"
            + "- extend uno.anahata.ai.tool.AnahataTool and \n"
            + "- implement the call method of java.util.concurrent.Callable<Object>.\n",
            requiresApproval = true
    )
    public Object compileAndExecute(
            @AiToolParam(value = "Source code of a public class called 'Anahata' that extends AnahataTool, implement Callable and has no package declaration.", rendererId = "java") String sourceCode,
            @AiToolParam(value = "Compiler's additional classpath entries separated with File.pathSeparator. These will be first in the final compiler's and ClassLoader's classpath", required = false) String extraClassPath,
            @AiToolParam(value = "Compiler's options.", required = false) String[] compilerOptions) throws Exception {

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
            throw new AiToolException("Source file should extend AnahataTool or implement java.util.Callable");
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
     * system properties (excluding the classpath).
     *
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
     *
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
