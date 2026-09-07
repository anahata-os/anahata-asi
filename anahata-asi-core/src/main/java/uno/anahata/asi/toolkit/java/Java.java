package uno.anahata.asi.toolkit.java;

import uno.anahata.asi.toolkit.java.classpath.VeryPrettyClassPathPrinter;
import java.io.File;
import java.io.StringWriter;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.URI;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.Callable;
import java.util.stream.Stream;
import javax.swing.text.html.ImageView;
import javax.tools.Diagnostic;
import javax.tools.DiagnosticCollector;
import javax.tools.JavaCompiler;
import javax.tools.JavaFileObject;
import javax.tools.SimpleJavaFileObject;
import javax.tools.ToolProvider;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.AbstractAsiContainer;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.agi.AgiConfig;
import uno.anahata.asi.agi.context.ContextPosition;
import uno.anahata.asi.internal.SystemPropertiesUtils;
import uno.anahata.asi.agi.message.RagMessage;
import uno.anahata.asi.agi.resource.RefreshPolicy;
import uno.anahata.asi.agi.resource.Resource;
import uno.anahata.asi.agi.resource.ResourceManager;
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
@AgiToolkit("Toolkit for compiling and executing java code. Uses a child-first classloader for extra classpath entries")
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
     * The transient printer used to generate token-optimized classpath
     * manifests. It is recreated lazily to ensure it is always available after
     * deserialization.
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
        registerParentFirstClass(ResourceManager.class);
        registerParentFirstClass(Resource.class);
        registerParentFirstClass(RefreshPolicy.class);
        registerParentFirstClass(ContextPosition.class);
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
     * <p>
     * Empty implementation as per architectural refinement. Initialization
     * logic is moved to {@link #postActivate()} and
     * {@link #setDefaultClasspath(String)}.</p>
     */
    @Override
    public void initialize() {
        log.debug("initialize(): parentFirstClasses: " + parentFirstClassess);
    }

    /**
     * {@inheritDoc}
     * <p>
     * Synchronizes the classpath printer with the current default classpath
     * after the toolkit has been activated or deserialized.</p>
     */
    @Override
    public void postActivate() {
        this.classpathPrinter = null;
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

        sb.append("\n⚠️ **IN-PROCESS JVM EXECUTION SAFETY WARNING**:\n");
        sb.append("Your compiled Java code executes directly **inside the host application's JVM process**.\n");
        sb.append("- **DO NOT call `System.exit(...)`** or `Runtime.getRuntime().halt(...)` as it will instantly terminate the host application.\n");
        sb.append("- **DO NOT mutate global JVM static state** or system properties unless specifically instructed.\n\n");

        sb.append("⚙️ **Compilation & ClassLoading Architecture**:\n");
        sb.append("When you invoke tools that compile and execute Java code (such as `compileAndExecute` or its subclass variants):\n\n");
        sb.append("1. **In-Memory Compilation & Classpath Priority**:\n");
        sb.append("   - The `JavaCompiler` compiles your `Anahata.java` source code in memory.\n");
        sb.append("   - **Classpath Priority**: Any `extraClassPath` entries provided to the tool are **prepended** ahead of the default classpath (`extraClassPath + File.pathSeparator + defaultCompilerClasspath`). This ensures custom or updated classes take precedence over existing libraries.\n\n");
        sb.append("2. **Child-First ClassLoading & Hot Reloading**:\n");
        sb.append("   - A custom `URLClassLoader` loads your compiled bytecode and any `extraClassPath` entries **Child-First**. This enables instant hot-reloading of modified classes in memory.\n\n");
        sb.append("3. **Parent-First Infrastructure Guard**:\n");
        sb.append("   - Core framework classes are initialized on a Parent-First list when the toolkit is loaded.\n");
        sb.append("   - These classes are explicitly delegated to the host JVM loader to prevent duplicate classloading when `extraClassPath` contains framework JARs, guaranteeing that ThreadLocal state and `instanceof` identity remain unified.\n");
        sb.append("   - The active list of Parent-First class FQNs is dynamically printed in the RAG message on every turn.\n");
        sb.append("   - You can dynamically mutate this set using `addParentFirstClasses` or `removeParentFirstClasses`.\n\n");

        sb.append("\n Multi-threading, Background Tasks, and Context Propagation:\n");
        sb.append("The logging (`log`), error reporting (`error`), attachment (`addAttachment`), turn map (`getTurnMap`), and response inspection (`getResponse`, `getCall`, `getModelMessage`) methods rely on ThreadLocal state bound to the tool execution thread.\n");
        sb.append("If you spawn background threads (e.g. `new Thread()`, `CompletableFuture`, `ExecutorService`), that ThreadLocal context will NOT be present on the new thread unless explicitly propagated.\n\n");

        sb.append("### Recommended Multi-Threading Patterns:\n\n");
        sb.append("**Pattern 1: Capture ToolContext (`getToolContext()`)**\n");
        sb.append("Capture `final ToolContext ctx = getToolContext();` on the main execution thread *before* creating background tasks. All methods called on `ctx` directly target the captured tool response without depending on ThreadLocal state:\n");
        sb.append("```java\n");
        sb.append("final ToolContext ctx = getToolContext();\n");
        sb.append("getExecutorService().submit(() -> {\n");
        sb.append("    ctx.log(\"Background processing started...\");\n");
        sb.append("    // Do background work...\n");
        sb.append("    ctx.log(\"Background processing complete!\");\n");
        sb.append("});\n");
        sb.append("```\n\n");

        sb.append("**Pattern 2: Built-in `runAsync(taskName, runnable)`**\n");
        sb.append("Automatically captures context, sets the thread name, binds context to the worker thread, and catches/logs background errors cleanly:\n");
        sb.append("```java\n");
        sb.append("runAsync(\"benchmark-task\", () -> {\n");
        sb.append("    log(\"Logging directly from context-bound worker thread!\");\n");
        sb.append("});\n");
        sb.append("```\n\n");

        sb.append("**Pattern 3: Thread-Safe Logger (`getThreadSafeLogger()`)**\n");
        sb.append("Get a `Consumer<String>` logger on the execution thread for pass-through logging:\n");
        sb.append("```java\n");
        sb.append("Consumer<String> logger = getThreadSafeLogger();\n");
        sb.append("CompletableFuture.runAsync(() -> logger.accept(\"Thread-safe log message!\"));\n");
        sb.append("```\n\n");

        sb.append("About the attribute maps:\n"
                + "- The Turn attribute map is for sharing data across tool calls within the same turn. (what in Servlet terms you could call 'request scoped'). Gets serialized.\n"
                + "- The Session Map is for this AGI session. Anything stored in this map during one turn will be available in subsequent turns (or subsequent tool calls within the same turn). This field is Persistent, gets serialized on every turn when the AGI container gets saved and survives application restarts, make sure that any object stored here is serializable with java.\n"
                + "- The ASI Container map is shared across sessions (agis) in the current AsiContainer (a given JVM could be running multiple ASI Containers). Currently it does not get serialized.\n"
                + "- The Application Map is a static field shared across all sessions (agis) of all ASI Containers running in this jvm\n");

        sb.append("\nAbout attachments: be careful attaching attachments as the supported mime types vary on a model basis.\n");

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
        sb.append("        return result;\n");
        sb.append("    }\n");
        sb.append("}\n");
        sb.append("```\n");
        sb.append("\n");
        sb.append("\n");
        sb.append("**JVM System Properties**:\n");
        sb.append(SystemPropertiesUtils.getSystemProperties());

        return Collections.singletonList(sb.toString());
    }

    /**
     * Registers a class and all its superclasses and interfaces to be loaded by
     * the parent classloader.
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
     * <p>
     * In this base implementation, it returns {@link OnTheFlyAgiTool}, which
     * provides the necessary context anchors (log, error, etc.) for the
     * script.</p>
     *
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
            this.classpathPrinter = null;
        }
    }

    /**
     * Returns a token-efficient, pretty-printed version of the default
     * classpath.
     * <p>
     * Implementation note: This leverages lexical grouping and version
     * promotion to keep the classpath manifest small.</p>
     *
     * @return The pretty-printed classpath string.
     */
    public String getPrettyPrintedDefaultClasspath() {
        return getClasspathPrinter().getPretty();
    }

    /**
     * Gets the lazily-initialized classpath printer.
     *
     * @return The {@link VeryPrettyClassPathPrinter} instance.
     */
    protected final VeryPrettyClassPathPrinter getClasspathPrinter() {
        if (classpathPrinter == null) {
            classpathPrinter = createClassPathPrinter();
            classpathPrinter.setRaw(getDefaultClasspath());
        }
        return classpathPrinter;
    }

    /**
     * Factory method to create the specialized classpath printer.
     *
     * @return A new {@link VeryPrettyClassPathPrinter} instance.
     */
    protected VeryPrettyClassPathPrinter createClassPathPrinter() {
        return new VeryPrettyClassPathPrinter();
    }

    /**
     * {@inheritDoc}
     * <p>
     * Adds session/container map keys and the abbreviated classpath manifest to
     * the RAG message to provide the model with awareness of its persistent
     * state and available libraries.</p>
     */
    @Override
    public void populateMessage(RagMessage ragMessage) throws Exception {
        String ragText
                = "\nSession (Agi) map keys (shared across turns, persistent): " + getSessionMap().keySet()
                + "\nASI Container map keys (shared across AGIs within the container), not persistent today): " + getAsiContainerMap().keySet()
                + "\nApplication map keys (a JVM wide static field, not persistent today): " + getApplicationMap().keySet()
                + "\nParent-First Infrastructure Classes (loaded by host loader to preserve ThreadLocal & context identity): " + getParentFirstClassess()
                + "\nDefault Compiler and ClassLoader Classpath (abbreviated):\n" + getPrettyPrintedDefaultClasspath();
        ragMessage.addTextPart(ragText);

        JavaCompiler compiler = getDefaultJavaCompiler();
        StringBuilder jdksInfo = new StringBuilder("\n### Available Java Compilers & JDKs\n");
        jdksInfo.append("- **In-Memory JavaCompiler**: ")
                .append(compiler != null ? "Available (" + compiler.getClass().getSimpleName() + ")" : "Not Available (Running on JRE/JBR)")
                .append("\n");
        List<KnownJdk> knownJdks = getKnownJdks();
        if (!knownJdks.isEmpty()) {
            jdksInfo.append("- **Known JDKs**:\n");
            for (KnownJdk jdk : knownJdks) {
                jdksInfo.append("  * `").append(jdk.name()).append("`");
                if (jdk.version() != null) {
                    jdksInfo.append(" (v").append(jdk.version()).append(")");
                }
                if (jdk.homePath() != null) {
                    jdksInfo.append(": ").append(jdk.homePath());
                }
                if (jdk.javacPath() != null) {
                    jdksInfo.append(" [javac: ").append(jdk.javacPath()).append("]");
                }
                if (jdk.preferred()) {
                    jdksInfo.append(" *(Default)*");
                }
                jdksInfo.append("\n");
            }
        }
        ragMessage.addTextPart(jdksInfo.toString());
    }

    /**
     * Adds a list of class FQNs to the Parent-First ClassLoader guard set,
     * resolving each class on the host classloader and recursively registering
     * its superclasses and interfaces.
     *
     * @param fqns List of fully qualified class names (FQNs) to resolve and add
     * to parent-first classes.
     * @throws java.lang.ClassNotFoundException If any requested class cannot be
     * loaded by the host ClassLoader.
     */
    @AgiTool("Adds a list of class FQNs to the Parent-First ClassLoader guard set, resolving each class and recursively registering its superclasses and interfaces")
    public void addParentFirstClasses(
            @AgiToolParam("List of fully qualified class names (FQNs) to add to parent-first classes") List<String> fqns) throws ClassNotFoundException {
        for (String fqn : fqns) {
            Class<?> clazz = Class.forName(fqn.trim(), false, getClass().getClassLoader());
            registerParentFirstClass(clazz);
        }
    }

    /**
     * Removes a list of class FQNs from the Parent-First guard set to allow
     * Child-First hot reloading.
     *
     * @param fqns List of fully qualified class names (FQNs) to remove from
     * parent-first classes.
     */
    @AgiTool("Removes a list of class FQNs from the Parent-First guard set to allow Child-First hot reloading")
    public void removeParentFirstClasses(
            @AgiToolParam("List of fully qualified class names (FQNs) to remove from parent-first classes") List<String> fqns) {
        for (String fqn : fqns) {
            parentFirstClassess.remove(fqn.trim());
        }
    }

    /**
     * Appends the signatures of all declared methods of a class to a
     * StringBuilder, filtering out standard Object methods and internal
     * lambda/abstract cruft.
     *
     * @param sb The StringBuilder to append to.
     * @param clazz The class to inspect.
     */
    protected static void appendMethods(StringBuilder sb, Class<?> clazz) {

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
                if (methodString.contains("anahata") && !methodString.contains("etToolkit(")) {
                    sb.append("- `").append(methodString).append("`\n");
                }
            }
        }
    }

    /**
     * Specialized child-first, hot-reloading {@link URLClassLoader} used for executing dynamic
     * scripts compiled in memory or via external javac.
     */
    public class AnahataURLClassLoader extends URLClassLoader {

        private final Map<String, byte[]> compiledClasses;

        /**
         * Constructs a new AnahataURLClassLoader.
         *
         * @param urls            the child-first classpath URLs.
         * @param compiledClasses in-memory bytecode map (class name -> bytes).
         * @param parent          the parent classloader (defaults to {@code Java.this.getClass().getClassLoader()} if null).
         */
        public AnahataURLClassLoader(List<URL> urls, Map<String, byte[]> compiledClasses, ClassLoader parent) {
            super(urls.toArray(new URL[0]), parent != null ? parent : Java.this.getClass().getClassLoader());
            this.compiledClasses = compiledClasses != null ? compiledClasses : Collections.emptyMap();
        }

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
                        ToolContext ctx = getToolContext();
                        if (ctx != null) {
                            ctx.log("Delegating infrastructure class to parent: " + name);
                        }
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
                            log.info("Loaded class from default classpath (Child-First): {}", name);
                        } catch (ClassNotFoundException e) {
                            // 5. FALLBACK: Ask the toolkit if it can find the bytes elsewhere (e.g. MR-JARs)
                            byte[] fallbackBytes = findClassFallbackBytes(name);
                            if (fallbackBytes != null) {
                                ToolContext ctx = getToolContext();
                                if (ctx != null) {
                                    ctx.log("Loaded class from Fallback Bridge: " + name);
                                }
                                c = defineClass(name, fallbackBytes, 0, fallbackBytes.length);
                            } else {
                                // 6. PARENT-LAST: If not found, delegate to the parent classloader.
                                try {
                                    c = super.loadClass(name, resolve);
                                } catch (ClassNotFoundException parentEx) {
                                    // 7. SIBLING / EXTRA CLASSLOADERS: (e.g., NetBeans JavaFX module)
                                    for (ClassLoader extraLoader : getExtraClassLoaders()) {
                                        try {
                                            c = extraLoader.loadClass(name);
                                            break;
                                        } catch (ClassNotFoundException ignored) {
                                        }
                                    }
                                    if (c == null) {
                                        throw parentEx;
                                    }
                                }
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
    }

    /**
     * Factory method to create an instance of {@link AnahataURLClassLoader}.
     *
     * @param extraUrls         extra URLs to search child-first.
     * @param compiledClasses   in-memory compiled bytecode map.
     * @param parentClassLoader parent classloader (defaults to {@code getClass().getClassLoader()} if null).
     * @return a new {@link AnahataURLClassLoader}.
     */
    protected AnahataURLClassLoader createReloadingClassLoader(
            List<URL> extraUrls,
            Map<String, byte[]> compiledClasses,
            ClassLoader parentClassLoader) {
        return new AnahataURLClassLoader(extraUrls, compiledClasses, parentClassLoader != null ? parentClassLoader : getClass().getClassLoader());
    }

    /**
     * Discovers all known JDK installations on the host environment.
     * <p>
     * Scans:
     * 1. The currently running JVM (via {@code System.getProperty("java.home")}).
     * 2. The {@code JAVA_HOME} environment variable.
     * 3. Standard platform JDK directories (/usr/lib/jvm, /Library/Java/JavaVirtualMachines, C:\Program Files\Java, etc.).
     * 4. The {@code javac} executable available on the system {@code PATH}.
     * </p>
     * <p>
     * Subclasses (such as {@code NbJava} and {@code IntellijJava}) override this method to add
     * IDE-registered platforms and project SDKs.
     * </p>
     *
     * @return a list of discovered {@link KnownJdk} instances.
     */
    public List<KnownJdk> getKnownJdks() {
        List<KnownJdk> result = new ArrayList<>();
        Set<Path> seenJavacPaths = new HashSet<>();

        // 1. Current running JVM
        try {
            String javaHomeProp = System.getProperty("java.home");
            if (javaHomeProp != null) {
                Path home = Path.of(javaHomeProp);
                Path javac = findJavacInJdkHome(home);
                if (javac != null && seenJavacPaths.add(javac.toAbsolutePath().normalize())) {
                    result.add(new KnownJdk("Current JVM (" + System.getProperty("java.version") + ")", home, javac, System.getProperty("java.version"), true));
                }
            }
        } catch (Exception e) {
            log.debug("Error checking java.home for javac", e);
        }

        // 2. JAVA_HOME environment variable
        try {
            String envJavaHome = System.getenv("JAVA_HOME");
            if (envJavaHome != null && !envJavaHome.isBlank()) {
                Path home = Path.of(envJavaHome.trim());
                Path javac = findJavacInJdkHome(home);
                if (javac != null && seenJavacPaths.add(javac.toAbsolutePath().normalize())) {
                    result.add(new KnownJdk("JAVA_HOME (" + home.getFileName() + ")", home, javac, null, false));
                }
            }
        } catch (Exception e) {
            log.debug("Error checking JAVA_HOME for javac", e);
        }

        // 3. Standard OS directories
        List<Path> standardRoots = List.of(
                Path.of("/usr/lib/jvm"),
                Path.of("/Library/Java/JavaVirtualMachines"),
                Path.of("C:\\Program Files\\Java"),
                Path.of("C:\\Program Files\\Eclipse Adoptium"),
                Path.of("C:\\Program Files\\Amazon Corretto")
        );
        for (Path root : standardRoots) {
            if (Files.exists(root) && Files.isDirectory(root)) {
                try (Stream<Path> stream = Files.list(root)) {
                    for (Path candidate : stream.toList()) {
                        Path javac = findJavacInJdkHome(candidate);
                        if (javac != null && seenJavacPaths.add(javac.toAbsolutePath().normalize())) {
                            result.add(new KnownJdk(candidate.getFileName().toString(), candidate, javac, null, false));
                        }
                    }
                } catch (Exception ignored) {
                }
            }
        }

        // 4. Javac on system PATH
        try {
            String pathEnv = System.getenv("PATH");
            if (pathEnv != null) {
                for (String p : pathEnv.split(File.pathSeparator)) {
                    if (!p.isBlank()) {
                        Path dir = Path.of(p.trim());
                        Path javac = dir.resolve(org.apache.commons.lang3.SystemUtils.IS_OS_WINDOWS ? "javac.exe" : "javac");
                        if (Files.isExecutable(javac) && seenJavacPaths.add(javac.toAbsolutePath().normalize())) {
                            result.add(new KnownJdk("PATH (" + javac.toAbsolutePath() + ")", dir.getParent(), javac, null, false));
                        }
                    }
                }
            }
        } catch (Exception ignored) {
        }

        return result;
    }

    /**
     * Helper to find a javac executable within a candidate JDK home directory.
     *
     * @param home candidate JDK home path.
     * @return path to javac binary if found and executable, or null.
     */
    public static Path findJavacInJdkHome(Path home) {
        if (home == null || !Files.exists(home)) {
            return null;
        }
        Path direct = home.resolve("bin").resolve(org.apache.commons.lang3.SystemUtils.IS_OS_WINDOWS ? "javac.exe" : "javac");
        if (Files.isExecutable(direct)) {
            return direct;
        }
        Path macHome = home.resolve("Contents").resolve("Home").resolve("bin").resolve("javac");
        if (Files.isExecutable(macHome)) {
            return macHome;
        }
        return null;
    }

    /**
     * Resolves an explicit JDK identifier, name, or path to a javac executable path.
     *
     * @param jdkNameOrPath optional name, ID, directory, or direct javac executable path.
     * @return the resolved Path to javac, or null if null/empty string provided.
     * @throws AgiToolException if an explicit identifier or path was specified but could not be found.
     */
    public Path resolveJavacPath(String jdkNameOrPath) throws AgiToolException {
        if (jdkNameOrPath == null || jdkNameOrPath.isBlank()) {
            return null;
        }
        String query = jdkNameOrPath.trim();

        // 1. Check if direct executable path
        Path asPath = Path.of(query);
        if (Files.isExecutable(asPath) && asPath.getFileName().toString().startsWith("javac")) {
            return asPath;
        }

        // 2. Check if directory containing bin/javac
        if (Files.isDirectory(asPath)) {
            Path javac = findJavacInJdkHome(asPath);
            if (javac != null) {
                return javac;
            }
        }

        // 3. Match against known JDK names/IDs
        for (KnownJdk known : getKnownJdks()) {
            if (known.name().equalsIgnoreCase(query) || known.name().toLowerCase().contains(query.toLowerCase())) {
                if (known.hasCompiler()) {
                    return known.javacPath();
                }
            }
        }

        throw new AgiToolException("Specified JDK / javac '" + query + "' could not be resolved to an executable javac binary.");
    }

    /**
     * Compiles Java source code into a Class object.
     * <p>
     * Resolution order:
     * 1. If an explicit {@code javacPath} is provided, compiles externally using that binary.
     * 2. If {@code javacPath} is null and in-memory {@link JavaCompiler} is available, compiles in memory.
     * 3. If {@code javacPath} is null and in-memory compiler is NOT available (JRE/JBR), automatically falls back
     *    to the first available JDK javac from {@link #getKnownJdks()}.
     * </p>
     *
     * @param sourceCode      the Java source code to compile.
     * @param className       the simple or fully qualified name of the class.
     * @param extraClassPath  additional classpath entries to include.
     * @param compilerOptions additional options for the compiler.
     * @param javacPath       optional explicit path to a javac executable.
     * @return the compiled Class object.
     * @throws Exception if compilation or classloading fails.
     */
    public Class<?> compile(
            String sourceCode,
            String className,
            String extraClassPath,
            String[] compilerOptions,
            Path javacPath) throws Exception {

        if (javacPath != null) {
            return compileWithExternalJavac(sourceCode, className, extraClassPath, compilerOptions, javacPath);
        }

        JavaCompiler inMemoryCompiler = getDefaultJavaCompiler();
        if (inMemoryCompiler != null) {
            return compileInMemory(sourceCode, className, extraClassPath, compilerOptions, inMemoryCompiler);
        }

        // Auto-fallback to external javac if running on JBR/JRE without in-memory compiler
        for (KnownJdk known : getKnownJdks()) {
            if (known.hasCompiler()) {
                log.info("No in-memory JavaCompiler available; auto-selected known JDK javac: {}", known.javacPath());
                return compileWithExternalJavac(sourceCode, className, extraClassPath, compilerOptions, known.javacPath());
            }
        }

        throw new AgiToolException("No Java compiler available. Running on a JRE without in-memory compiler, and no external JDK javac was found.");
    }

    /**
     * Compiles Java source code in memory using {@link JavaCompiler}.
     *
     * @param sourceCode      the Java source code to compile.
     * @param className       the fully qualified name of the class.
     * @param extraClassPath  additional classpath entries to include.
     * @param compilerOptions additional options for the Java compiler.
     * @param compiler        the compiler instance.
     * @return the compiled Class object.
     * @throws ClassNotFoundException    if class not found.
     * @throws NoSuchMethodException    if method not found.
     * @throws IllegalAccessException    if access denied.
     * @throws InvocationTargetException if invocation fails.
     */
    public Class<?> compileInMemory(
            String sourceCode,
            String className,
            String extraClassPath,
            String[] compilerOptions,
            JavaCompiler compiler)
            throws ClassNotFoundException, NoSuchMethodException, IllegalAccessException, InvocationTargetException {

        final ToolContext ctx = getToolContext();

        log("Compiling class in memory: " + className);

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

        InMemoryJavaFileManager fileManager = new InMemoryJavaFileManager(compiler.getStandardFileManager(diagnostics, null, null));

        if (extraClassPath != null) {
            log("Including extra classpath entries: " + extraClassPath.split(File.pathSeparator).length);
            log.info("extraClassPath: {} entries:\n{}", extraClassPath.split(File.pathSeparator).length, extraClassPath);
        }

        String classpath = getDefaultClasspath();
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
            throw new RuntimeException("Compilation error:\n" + error.toString());
        }

        Map<String, byte[]> compiledClasses = fileManager.getCompiledClasses();

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

        AnahataURLClassLoader reloadingClassLoader = createReloadingClassLoader(urlList, compiledClasses, getClass().getClassLoader());
        return reloadingClassLoader.loadClass(className);
    }

    /**
     * Compiles Java source code using an external {@code javac} process and loads the resulting class.
     * <p>
     * Robust implementation:
     * 1. Writes all compiler options to an {@code @argfile} to completely bypass OS/Windows command-line length limits.
     * 2. Enforces matching {@code --release} bytecode compatibility to avoid UnsupportedClassVersionError.
     * 3. Performs atomic cleanup of the scratch directory in a finally block (zero disk leaks).
     * </p>
     *
     * @param sourceCode      the Java source code.
     * @param className       the simple class name.
     * @param extraClassPath  optional additional classpath entries.
     * @param compilerOptions optional compiler options.
     * @param javacPath       the absolute path to the javac executable.
     * @return the loaded {@link Class}.
     * @throws Exception on compilation or classloading failure.
     */
    protected Class<?> compileWithExternalJavac(
            String sourceCode,
            String className,
            String extraClassPath,
            String[] compilerOptions,
            Path javacPath) throws Exception {

        final ToolContext ctx = getToolContext();
        Path tempDir = Files.createTempDirectory("anahata-javac-" + className + "-");
        try {
            Path sourceFile = tempDir.resolve(className + ".java");
            Files.writeString(sourceFile, sourceCode, StandardCharsets.UTF_8);

            String classpath = getDefaultClasspath();
            if (extraClassPath != null && !extraClassPath.isEmpty()) {
                classpath = extraClassPath + File.pathSeparator + classpath;
            }

            List<String> options = new ArrayList<>();
            options.add("-d");
            options.add(tempDir.toAbsolutePath().toString());
            options.add("-classpath");
            options.add(classpath);

            if (compilerOptions != null) {
                options.addAll(Arrays.asList(compilerOptions));
            }

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
                log.info("No explicit Java version compiler flag found for external javac. Defaulting to --release {}.", runtimeVersion);
                if (ctx != null) {
                    ctx.log("No explicit Java version compiler flag found. Defaulting to --release " + runtimeVersion);
                }
                options.add("--release");
                options.add(runtimeVersion);
            }

            if (!options.contains("-proc:none")) {
                options.add("-proc:none");
            }
            options.add(sourceFile.toAbsolutePath().toString());

            // Write all arguments to an @argfile to avoid Windows/OS command line length limits
            Path argFile = tempDir.resolve("javac_args.txt");
            Files.write(argFile, options, StandardCharsets.UTF_8);

            List<String> command = List.of(javacPath.toAbsolutePath().toString(), "@" + argFile.toAbsolutePath());
            log.info("Executing external javac via argfile: {} with {} options", javacPath, options.size());

            ProcessBuilder pb = new ProcessBuilder(command);
            pb.redirectErrorStream(true);
            Process process = pb.start();
            String output = new String(process.getInputStream().readAllBytes(), StandardCharsets.UTF_8);
            int exitCode = process.waitFor();

            if (exitCode != 0) {
                log.error("Compilation error via javac ({}):\n{}", javacPath, output);
                throw new AgiToolException("Compilation error via javac (" + javacPath.getFileName() + "):\n" + output);
            }

            // Read compiled .class files into memory
            Map<String, byte[]> compiledClasses = new HashMap<>();
            try (Stream<Path> stream = Files.walk(tempDir)) {
                for (Path file : stream.filter(p -> p.toString().endsWith(".class")).toList()) {
                    String relative = tempDir.relativize(file).toString();
                    String classFqn = relative.replace(File.separatorChar, '.').replace('/', '.');
                    if (classFqn.endsWith(".class")) {
                        classFqn = classFqn.substring(0, classFqn.length() - 6);
                    }
                    compiledClasses.put(classFqn, Files.readAllBytes(file));
                }
            }

            List<URL> urlList = new ArrayList<>();
            if (extraClassPath != null && !extraClassPath.isEmpty()) {
                for (String entry : extraClassPath.split(File.pathSeparator)) {
                    try {
                        urlList.add(new File(entry).toURI().toURL());
                    } catch (Exception e) {
                        log.warn("Invalid classpath entry: {}", entry, e);
                    }
                }
            }

            AnahataURLClassLoader reloadingClassLoader = createReloadingClassLoader(urlList, compiledClasses, getClass().getClassLoader());
            return reloadingClassLoader.loadClass(className);
        } finally {
            // Guarantee atomic cleanup of the scratch directory (Zero Leaks!)
            try (Stream<Path> walk = Files.walk(tempDir)) {
                walk.sorted(java.util.Comparator.reverseOrder())
                        .map(Path::toFile)
                        .forEach(File::delete);
            } catch (Exception e) {
                log.warn("Failed to delete temp compilation dir: {}", tempDir, e);
            }
        }
    }

    /**
     * A hook for subclasses to provide class bytes if the standard loading flow
     * fails.
     * <p>
     * This is used by the NetBeans implementation (NbJava) to bridge
     * Multi-Release JAR classes into the memory-based loader.</p>
     *
     * @param name The FQN of the class.
     * @return The class bytes, or null if not found.
     */
    protected byte[] findClassFallbackBytes(String name) {
        return null;
    }

    /**
     * Hook for subclasses to provide extra/sibling classloaders to search if
     * both the child classpath and the parent classloader fail to find a class.
     *
     * @return A list of additional ClassLoaders to query.
     */
    protected List<ClassLoader> getExtraClassLoaders() {
        return Collections.emptyList();
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
            + "- extend uno.anahata.asi.agi.tool.OnTheFlyAgiTool (or the concreate subtype specified in the toolkit instructions, if any) and \n"
            + "- implement the call method of java.util.concurrent.Callable<Object>.\n"
            + "\nNote: Like any other tool, If call() throws an exception, the Exception's stack trace will be automatically converted to a string and included in the 'errors' attribute of the tool's response.\n"
    )
    public Object compileAndExecute(
            @AgiToolParam(value = "Source code of the 'Anahata' class.", rendererId = "java") String sourceCode,
            @AgiToolParam(value = "Optional Compiler's additional classpath entries separated with File.pathSeparator. These will be first in the final compiler's classpath and the child-first set of the ClassLoader's classpath", required = false) String extraClassPath,
            @AgiToolParam(value = "Optional Compiler's options.", required = false) String[] compilerOptions,
            @AgiToolParam(value = "Optional JDK name (from Available JDKs) or explicit path to a javac executable. If omitted, uses the default compiler.", required = false) String jdk) throws Exception {

        log.info("executeJavaCode: \nsource={}", sourceCode);
        log.info("executeJavaCode: \nextraCompilerClassPath={}", extraClassPath);

        Path javacPath = resolveJavacPath(jdk);
        Class<?> c = compile(sourceCode, "Anahata", extraClassPath, compilerOptions, javacPath);

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
     * Convenience overload for {@link #compileAndExecute(String, String, String[], String)} using default compiler.
     *
     * @param sourceCode      the source code.
     * @param extraClassPath  additional classpath.
     * @param compilerOptions compiler options.
     * @return the execution result.
     * @throws Exception on error.
     */
    public Object compileAndExecute(String sourceCode, String extraClassPath, String[] compilerOptions) throws Exception {
        return compileAndExecute(sourceCode, extraClassPath, compilerOptions, (String) null);
    }
    
    /**
     * Overridable method for implementations to decide what compiler to use by default.
     * 
     * @return <code>ToolProvider.getSystemJavaCompiler();</code>
     */
    protected JavaCompiler getDefaultJavaCompiler() {
        return ToolProvider.getSystemJavaCompiler();
    }
}
