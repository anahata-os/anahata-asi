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
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.Callable;
import java.util.concurrent.ConcurrentHashMap;
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
     * In-memory compiled classes registered across turns for this AGI session.
     */
    @Getter
    protected Map<String, AgiCompiledClass> agiCompiledClasses = new ConcurrentHashMap<>();

    /**
     * Session-scoped classloader holding in-memory compiled classes and library URLs.
     */
    protected transient AgiClassLoader agiClassLoader;

    /**
     * Default constructor. Initializes the default classpath from the system's
     * "java.class.path" property.
     */
    public Java() {
        defaultCompilerClasspath = System.getProperty("java.class.path");
        registerParentFirstClass(OnTheFlyAgiTool.class);
        registerParentFirstClass(AgiCompiledClass.class);
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
        this.agiClassLoader = null;
    }

    /**
     * Returns the active session-scoped {@link AgiClassLoader}, lazily instantiating it
     * from all accumulated {@link AgiCompiledClass} library URLs if not yet created.
     *
     * @return the active {@link AgiClassLoader}.
     */
    public synchronized AgiClassLoader getOrCreateAgiClassLoader() {
        if (agiClassLoader == null) {
            Set<String> uniqueEntries = new LinkedHashSet<>();
            for (AgiCompiledClass acc : agiCompiledClasses.values()) {
                String extra = acc.getExtraClassPath();
                if (extra != null && !extra.isBlank()) {
                    for (String p : extra.split(File.pathSeparator)) {
                        if (!p.isBlank()) {
                            uniqueEntries.add(p.trim());
                        }
                    }
                }
            }
            List<URL> urls = new ArrayList<>();
            for (String entry : uniqueEntries) {
                try {
                    urls.add(new File(entry).toURI().toURL());
                } catch (Exception e) {
                    String msg = "Invalid extraClassPath URL: " + entry + " (" + e.getMessage() + ")";
                    log.warn(msg, e);
                    error(msg);
                }
            }
            agiClassLoader = new AgiClassLoader(urls, this, getClass().getClassLoader());
        }
        return agiClassLoader;
    }

    /**
     * Closes and resets the active {@link AgiClassLoader}. Required when re-compiling or
     * removing classes to allow the JVM to reload redefined classes on the next execution.
     */
    public synchronized void resetAgiClassLoader() {
        if (agiClassLoader != null) {
            try {
                agiClassLoader.close();
            } catch (Exception e) {
                log.error("Failed to close AgiClassLoader: {}", e.getMessage(), e);
                error("Failed to close AgiClassLoader: " + e.getMessage());
            }
            agiClassLoader = null;
        }
    }

    /**
     * {@inheritDoc}
     * <p>
     * Provides system instructions for runtime Java code compilation, detailing
     * available methods, multi-threading patterns, memory safety, and JVM
     * properties.
     * </p>
     *
     * @return the list of system instruction blocks.
     * @throws Exception if an error occurs while assembling instructions.
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

        sb.append("⚙️ **3-Tier Compilation & ClassLoading Architecture**:\n");
        sb.append("This environment provides a sophisticated 3-tier classloader hierarchy designed for high-salience modular development across turns:\n\n");
        sb.append("1. **Tier 1 - Host ClassLoader (Platform & Environment Runtime)**:\n");
        sb.append("   - The root loader containing the JDK, host application APIs, and all bundled libraries.\n");
        sb.append("   - **Extra ClassLoaders**: In extensible environments, this tier also delegates to any additional platform classloaders registered by the host environment.\n");
        sb.append("   - **Parent-First Infrastructure Guard**: Core framework classes (`Agi`, `ToolContext`, `DesktopAgiTool`, `Resource`, etc.) are whitelisted to always load here. This preserves ThreadLocal bindings, tool context propagation, and singleton identities.\n\n");
        sb.append("2. **Tier 2 - AgiClassLoader (Session-Scoped In-Memory Metaspace)**:\n");
        sb.append("   - A persistent, session-scoped classloader that holds all modular Java classes compiled via `Java.compile(...)` (e.g. `Person`, `FlightContact`, domain entities, helper utilities) and any external library JARs passed via `extraClassPath`.\n");
        sb.append("   - **Type Identity Across Turns**: Classes defined here are compiled into session RAM and maintain identical `Class<?>` identity across turns. You can compile a class in Turn 1, instantiate it in Turn 2, store it, and use it across subsequent turns without `ClassCastException`.\n");
        sb.append("   - **Redefinition & Lifecycle**: If you recompile or remove an existing class, `AgiClassLoader` is safely rotated so the new definition takes effect cleanly on the next execution.\n\n");
        sb.append("3. **Tier 3 - AnahataClassLoader (Ephemeral Script Runner)**:\n");
        sb.append("   - A throwaway, child-first classloader created per `compileAndExecute` invocation specifically to run your `Anahata.java` script.\n");
        sb.append("   - **Automatic URL Pruning**: Any library URLs already registered in `AgiClassLoader` are automatically filtered out from `AnahataClassLoader` so both the script and compiled session classes link to the exact same library types.\n\n");
        sb.append("💡 **Multi-Turn Modular Development Workflow (`compile` vs `compileAndExecute`)**:\n");
        sb.append("- **`Java.compile(classFqn, sourceCode, extraClassPath, ...)`**: Compiles a modular top-level class, record, or interface into the session metaspace without executing it. Use this across turns to construct clean, multi-file architectures rather than cramming all logic into a single giant script file.\n");
        sb.append("- **`Java.compileAndExecute(sourceCode, extraClassPath, ...)`**: Compiles and runs a single-shot execution script extending `" + getConcreteClassModelShouldExtend() + "`. Can import and instantiate any classes previously compiled via `Java.compile`.\n");
        sb.append("- **Inspection & Cleanup Tools**: Use `Java.getAgiClassSources` to inspect stored source code from earlier turns, `Java.removeAgiClasses` to purge specific classes, or `Java.clearAllAgiClasses` to reset.\n\n");

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
                + "- **Turn Map (`getTurnMap()`)**: Request-scoped map for sharing state across tool calls within the same turn. Gets serialized.\n"
                + "- **Session Map (`getSessionMap()`)**: Session-scoped map that persists across turns. Note: When persisting sessions across IDE restarts via Kryo, objects stored here should belong to the host/parent classloader (standard JDK types, framework models, strings, collections). Dynamic in-memory classes defined exclusively in `AgiClassLoader` live for the duration of the running JVM session.\n"
                + "- **ASI Container Map (`getAsiContainerMap()`)**: Shared across all active AGI sessions in the current container (in-memory).\n"
                + "- **Application Map (`getApplicationMap()`)**: JVM-wide static map shared across all containers.\n");

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
     * Adds session/container map keys, the abbreviated classpath manifest,
     * in-memory compiled classes held by {@link AgiClassLoader}, and
     * available Java compilers and JDKs to the RAG message.
     * </p>
     *
     * @param ragMessage the incoming RAG message to populate.
     * @throws Exception if an error occurs during message population.
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

        ClassLoader hostLoader = getClass().getClassLoader();
        List<ClassLoader> extraLoaders = getExtraClassLoaders();
        StringBuilder clInfo = new StringBuilder("\n### ☕ JVM ClassLoader Hierarchy\n");
        clInfo.append("- **Tier 1 - Host ClassLoader**: `")
                .append(hostLoader != null ? hostLoader.toString() : "Bootstrap")
                .append("`\n");
        if (!extraLoaders.isEmpty()) {
            clInfo.append("- **Extra ClassLoaders (").append(extraLoaders.size()).append(")**:\n");
            for (ClassLoader el : extraLoaders) {
                clInfo.append("  * `").append(el.toString()).append("`\n");
            }
        }
        ragMessage.addTextPart(clInfo.toString());

        if (!agiCompiledClasses.isEmpty()) {
            SimpleDateFormat sdf = new SimpleDateFormat("HH:mm:ss");
            StringBuilder sb = new StringBuilder("\n### ☕ AgiClassLoader (Session In-Memory Metaspace)\n");
            sb.append("- **Active In-Memory Classes (").append(agiCompiledClasses.size()).append(")**:\n");
            for (AgiCompiledClass acc : agiCompiledClasses.values()) {
                sb.append("  * `").append(acc.getFqn()).append("` (")
                        .append(String.format("%.1f KB", acc.getTotalBytecodeSize() / 1024.0)).append(", ")
                        .append(acc.getSourceLines()).append(" lines, compiled: ")
                        .append(sdf.format(new Date(acc.getCompiledAtMillis()))).append(")");
                if (acc.getExtraClassPath() != null && !acc.getExtraClassPath().isBlank()) {
                    sb.append(" [extraClassPath: ").append(acc.getExtraClassPath()).append("]");
                }
                sb.append("\n");
            }
            AgiClassLoader acl = this.agiClassLoader;
            if (acl != null) {
                URL[] urls = acl.getURLs();
                if (urls != null && urls.length > 0) {
                    sb.append("- **Registered Extra Classpath URLs (").append(urls.length).append(")**:\n");
                    for (URL u : urls) {
                        sb.append("  * `").append(u).append("`\n");
                    }
                }
            }
            sb.append("*(Use `Java.getAgiClassSources` to inspect source code or `Java.removeAgiClasses` to remove)*\n");
            ragMessage.addTextPart(sb.toString());
        }

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
     * Collects all compiled bytecode maps from active {@link AgiCompiledClass} instances.
     *
     * @return map of binary class name to compiled bytecode byte array.
     */
    public Map<String, byte[]> getAllAgiCompiledBytecodes() {
        Map<String, byte[]> result = new HashMap<>();
        for (AgiCompiledClass acc : agiCompiledClasses.values()) {
            result.putAll(acc.getBytecodes());
        }
        return result;
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
     * Emits a high-salience classloading lifecycle log message.
     * <p>
     * If called within an active tool execution thread, it logs directly to the active
     * {@link JavaMethodToolResponse} so the AI model and developer see classloader events
     * live in the tool response. If called outside a tool execution thread, it logs to SLF4J
     * at INFO level.
     * </p>
     *
     * @param message the classloading log message.
     */
    public static void logClassloading(String message) {
        JavaMethodToolResponse current = JavaMethodToolResponse.getCurrent();
        if (current != null) {
            current.addLog(message);
        } else {
            log.info("[ClassLoading] {}", message);
        }
    }

    /**
     * Specialized child-first, hot-reloading {@link URLClassLoader} used for
     * executing dynamic scripts compiled in memory or via external javac.
     */
    public class AnahataClassLoader extends URLClassLoader {

        /**
         * In-memory bytecode definitions for newly compiled classes, mapped by
         * binary class name.
         */
        private final Map<String, byte[]> compiledClasses;

        /**
         * Constructs a new AnahataClassLoader with the active {@link AgiClassLoader}
         * as parent.
         *
         * @param urls the child-first classpath URLs (pruned of URLs already in AgiClassLoader).
         * @param compiledClasses in-memory bytecode map (class name -> bytes).
         * @param parent the parent classloader (typically {@link #getOrCreateAgiClassLoader()}).
         */
        public AnahataClassLoader(List<URL> urls, Map<String, byte[]> compiledClasses, ClassLoader parent) {
            super(urls.toArray(new URL[0]), parent != null ? parent : getOrCreateAgiClassLoader());
            this.compiledClasses = compiledClasses != null ? compiledClasses : Collections.emptyMap();
        }

        /**
         * {@inheritDoc}
         * <p>
         * Implements child-first class loading with parent-first delegation for
         * whitelisted framework infrastructure classes and direct in-memory
         * bytecode definition.
         * </p>
         *
         * @param name the binary name of the class.
         * @param resolve if true then resolve the class.
         * @return the resulting Class object.
         * @throws ClassNotFoundException if the class could not be found.
         */
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
                        logClassloading("[AnahataClassLoader] Delegating infrastructure class to parent: " + name);
                        return super.loadClass(name, resolve);
                    }

                    // 3. Check for our in-memory compiled class first (the "hot-reload" part for Anahata.java)
                    byte[] bytes = compiledClasses.get(name);
                    if (bytes != null) {
                        logClassloading("[AnahataClassLoader] Loading dynamic script: " + name);
                        c = defineClass(name, bytes, 0, bytes.length);
                    } else {
                        try {
                            // 4. CHILD-FIRST: Try to find the class in our own URLs (e.g., target/classes)
                            c = findClass(name);
                            log.info("Loaded class from child URLs (Child-First): {}", name);
                        } catch (ClassNotFoundException e) {
                            // 5. FALLBACK: Ask the toolkit if it can find the bytes elsewhere (e.g. MR-JARs)
                            byte[] fallbackBytes = findClassFallbackBytes(name);
                            if (fallbackBytes != null) {
                                logClassloading("[AnahataClassLoader] Loaded class from Fallback Bridge: " + name);
                                c = defineClass(name, fallbackBytes, 0, fallbackBytes.length);
                            } else {
                                // 6. PARENT-LAST: If not found, delegate to the parent classloader (AgiClassLoader).
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
     * Factory method to create an instance of {@link AnahataClassLoader} with
     * {@link AgiClassLoader} as parent.
     *
     * @param extraUrls extra URLs to search child-first.
     * @param compiledClasses in-memory compiled bytecode map.
     * @return a new {@link AnahataClassLoader}.
     */
    protected AnahataClassLoader createReloadingClassLoader(
            List<URL> extraUrls,
            Map<String, byte[]> compiledClasses) {
        return new AnahataClassLoader(extraUrls, compiledClasses, getOrCreateAgiClassLoader());
    }

    /**
     * Discovers all known JDK installations on the host environment.
     * <p>
     * Scans: 1. The currently running JVM (via
     * {@code System.getProperty("java.home")}). 2. The {@code JAVA_HOME}
     * environment variable. 3. Standard platform JDK directories (/usr/lib/jvm,
     * /Library/Java/JavaVirtualMachines, C:\Program Files\Java, etc.). 4. The
     * {@code javac} executable available on the system {@code PATH}.
     * </p>
     * <p>
     * Subclasses (such as {@code NbJava} and {@code IntellijJava}) override
     * this method to add IDE-registered platforms and project SDKs.
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
     * Resolves an explicit JDK identifier, name, or path to a javac executable
     * path.
     *
     * @param jdkNameOrPath optional name, ID, directory, or direct javac
     * executable path.
     * @return the resolved Path to javac, or null if null/empty string
     * provided.
     * @throws AgiToolException if an explicit identifier or path was specified
     * but could not be found.
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
     * Resolution order: 1. If an explicit {@code javacPath} is provided,
     * compiles externally using that binary. 2. If {@code javacPath} is null
     * and in-memory {@link JavaCompiler} is available, compiles in memory. 3.
     * If {@code javacPath} is null and in-memory compiler is NOT available
     * (JRE/JBR), automatically falls back to the first available JDK javac from
     * {@link #getKnownJdks()}.
     * </p>
     *
     * @param sourceCode the Java source code to compile.
     * @param className the simple or fully qualified name of the class.
     * @param extraClassPath additional classpath entries to include.
     * @param compilerOptions additional options for the compiler.
     * @param javacPath optional explicit path to a javac executable.
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
                log("No in-memory JavaCompiler available; auto-selected known JDK javac: " + known.javacPath());
                return compileWithExternalJavac(sourceCode, className, extraClassPath, compilerOptions, known.javacPath());
            }
        }

        throw new AgiToolException("No Java compiler available. Running on a JRE without in-memory compiler, and no external JDK javac was found.");
    }

    /**
     * Compiles Java source code in memory using {@link JavaCompiler}.
     *
     * @param sourceCode the Java source code to compile.
     * @param className the fully qualified name of the class.
     * @param extraClassPath additional classpath entries to include.
     * @param compilerOptions additional options for the Java compiler.
     * @param compiler the compiler instance.
     * @return the compiled Class object.
     * @throws ClassNotFoundException if class not found.
     * @throws NoSuchMethodException if method not found.
     * @throws IllegalAccessException if access denied.
     * @throws InvocationTargetException if invocation fails.
     */
    public Class<?> compileInMemory(
            String sourceCode,
            String className,
            String extraClassPath,
            String[] compilerOptions,
            JavaCompiler compiler)
            throws ClassNotFoundException, NoSuchMethodException, IllegalAccessException, InvocationTargetException {

        log("Compiling class in memory: " + className);

        if (compiler == null) {
            throw new RuntimeException("JDK required (running on JRE).");
        }

        Map<String, byte[]> compiledClasses;
        try {
            compiledClasses = compileInMemoryBytecodes(sourceCode, className, extraClassPath, compilerOptions, compiler);
        } catch (Exception e) {
            if (e instanceof RuntimeException re) {
                throw re;
            }
            throw new RuntimeException(e.getMessage(), e);
        }

        List<URL> urlList = new ArrayList<>();
        if (extraClassPath != null && !extraClassPath.isEmpty()) {
            String[] pathElements = extraClassPath.split(File.pathSeparator);
            for (String element : pathElements) {
                try {
                    urlList.add(new File(element).toURI().toURL());
                } catch (Exception e) {
                    String msg = "Invalid classpath entry: " + element + " (" + e.getMessage() + ")";
                    log.warn(msg, e);
                    error(msg);
                }
            }
        }

        AgiClassLoader acl = getOrCreateAgiClassLoader();
        Set<URL> agiUrls = acl.getRegisteredUrls();
        List<URL> childUrls = new ArrayList<>();
        for (URL u : urlList) {
            if (!agiUrls.contains(u)) {
                childUrls.add(u);
            }
        }

        AnahataClassLoader reloadingClassLoader = createReloadingClassLoader(childUrls, compiledClasses);
        return reloadingClassLoader.loadClass(className);
    }

    /**
     * Compiles Java source code using an external {@code javac} process and
     * loads the resulting class.
     * <p>
     * Robust implementation: 1. Writes all compiler options to an
     * {@code @argfile} to completely bypass OS/Windows command-line length
     * limits. 2. Enforces matching {@code --release} bytecode compatibility to
     * avoid UnsupportedClassVersionError. 3. Performs atomic cleanup of the
     * scratch directory in a finally block (zero disk leaks).
     * </p>
     *
     * @param sourceCode the Java source code.
     * @param className the simple class name.
     * @param extraClassPath optional additional classpath entries.
     * @param compilerOptions optional compiler options.
     * @param javacPath the absolute path to the javac executable.
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
            // Write existing in-memory AGI classes to the temp output directory so external javac sees them on classpath
            for (Map.Entry<String, byte[]> entry : getAllAgiCompiledBytecodes().entrySet()) {
                String binaryName = entry.getKey();
                Path classFilePath = tempDir.resolve(binaryName.replace('.', File.separatorChar) + ".class");
                if (classFilePath.getParent() != null) {
                    Files.createDirectories(classFilePath.getParent());
                }
                Files.write(classFilePath, entry.getValue());
            }

            Path sourceFile = tempDir.resolve(className.replace('.', File.separatorChar) + ".java");
            if (sourceFile.getParent() != null) {
                Files.createDirectories(sourceFile.getParent());
            }
            Files.writeString(sourceFile, sourceCode, StandardCharsets.UTF_8);

            String classpath = tempDir.toAbsolutePath().toString();
            if (extraClassPath != null && !extraClassPath.isEmpty()) {
                classpath = extraClassPath + File.pathSeparator + classpath;
            }
            classpath = classpath + File.pathSeparator + getDefaultClasspath();

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
                log("No explicit Java version compiler flag found for external javac. Defaulting to --release " + runtimeVersion);
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
            log("Executing external javac via argfile: " + javacPath + " with " + options.size() + " options");

            ProcessBuilder pb = new ProcessBuilder(command);
            pb.redirectErrorStream(true);
            Process process = pb.start();
            String output = new String(process.getInputStream().readAllBytes(), StandardCharsets.UTF_8);
            int exitCode = process.waitFor();

            if (exitCode != 0) {
                error("Compilation error via javac (" + javacPath.getFileName() + "):\n" + output);
                throw new AgiToolException("Compilation error via javac (" + javacPath.getFileName() + "):\n" + output);
            }

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
                        String msg = "Invalid classpath entry: " + entry + " (" + e.getMessage() + ")";
                        log.warn(msg, e);
                        error(msg);
                    }
                }
            }

            AgiClassLoader acl = getOrCreateAgiClassLoader();
            Set<URL> agiUrls = acl.getRegisteredUrls();
            List<URL> childUrls = new ArrayList<>();
            for (URL u : urlList) {
                if (!agiUrls.contains(u)) {
                    childUrls.add(u);
                }
            }

            AnahataClassLoader reloadingClassLoader = createReloadingClassLoader(childUrls, compiledClasses);
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
     * @param jdk Optional JDK name, ID, or path to a javac executable.
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
     * Convenience overload for
     * {@link #compileAndExecute(String, String, String[], String)} using
     * default compiler.
     *
     * @param sourceCode the source code.
     * @param extraClassPath additional classpath.
     * @param compilerOptions compiler options.
     * @return the execution result.
     * @throws Exception on error.
     */
    public Object compileAndExecute(String sourceCode, String extraClassPath, String[] compilerOptions) throws Exception {
        return compileAndExecute(sourceCode, extraClassPath, compilerOptions, (String) null);
    }
    
    /**
     * Overridable method for implementations to decide what compiler to use by
     * default.
     *
     * @return <code>ToolProvider.getSystemJavaCompiler();</code>
     */
    protected JavaCompiler getDefaultJavaCompiler() {
        return ToolProvider.getSystemJavaCompiler();
    }

    /**
     * Compiles source code in memory and extracts all generated class bytecode byte arrays.
     *
     * @param sourceCode the Java source code.
     * @param classFqn the class fully qualified name.
     * @param extraClassPath optional extra classpath.
     * @param compilerOptions optional compiler options.
     * @param compiler the JavaCompiler instance.
     * @return map of class binary names to compiled byte arrays.
     * @throws Exception on compilation error.
     */
    public Map<String, byte[]> compileInMemoryBytecodes(
            String sourceCode,
            String classFqn,
            String extraClassPath,
            String[] compilerOptions,
            JavaCompiler compiler) throws Exception {

        String simpleName = classFqn.contains(".") ? classFqn.substring(classFqn.lastIndexOf('.') + 1) : classFqn;
        String sourceFile = simpleName + ".java";
        JavaFileObject source = new SimpleJavaFileObject(URI.create("string:///" + sourceFile), JavaFileObject.Kind.SOURCE) {
            @Override
            public CharSequence getCharContent(boolean ignoreEncodingErrors) {
                return sourceCode;
            }
        };

        DiagnosticCollector<JavaFileObject> diagnostics = new DiagnosticCollector<>();
        InMemoryJavaFileManager fileManager = new InMemoryJavaFileManager(compiler.getStandardFileManager(diagnostics, null, null), getAllAgiCompiledBytecodes());

        String classpath = getDefaultClasspath();
        if (extraClassPath != null && !extraClassPath.isEmpty()) {
            classpath = extraClassPath + File.pathSeparator + classpath;
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
            options.add("--release");
            options.add(runtimeVersion);
        }

        if (!options.contains("-proc:none")) {
            options.add("-proc:none");
        }

        StringWriter writer = new StringWriter();
        JavaCompiler.CompilationTask task = compiler.getTask(writer, fileManager, diagnostics, options, null, Collections.singletonList(source));
        boolean success = task.call();

        if (!success) {
            StringBuilder error = new StringBuilder("Compilation Diagnostics:\n");
            for (Diagnostic<? extends JavaFileObject> d : diagnostics.getDiagnostics()) {
                error.append(d.toString()).append("\n");
            }
            throw new AgiToolException("Compilation error in class '" + classFqn + "':\n" + error.toString());
        }

        return fileManager.getCompiledClasses();
    }

    /**
     * Compiles a modular Java class and returns all resulting bytecode byte arrays.
     *
     * @param sourceCode the Java source code.
     * @param classFqn the class fully qualified name.
     * @param extraClassPath optional extra classpath.
     * @param compilerOptions optional compiler options.
     * @param javacPath optional path to javac executable.
     * @return map of class binary names to compiled byte arrays.
     * @throws Exception on error.
     */
    public Map<String, byte[]> compileBytecodes(
            String sourceCode,
            String classFqn,
            String extraClassPath,
            String[] compilerOptions,
            Path javacPath) throws Exception {

        if (javacPath != null) {
            Path tempDir = Files.createTempDirectory("anahata-compile-" + classFqn.replace('.', '_') + "-");
            try {
                for (Map.Entry<String, byte[]> entry : getAllAgiCompiledBytecodes().entrySet()) {
                    String binaryName = entry.getKey();
                    Path classFilePath = tempDir.resolve(binaryName.replace('.', File.separatorChar) + ".class");
                    if (classFilePath.getParent() != null) {
                        Files.createDirectories(classFilePath.getParent());
                    }
                    Files.write(classFilePath, entry.getValue());
                }

                Path sourceFile = tempDir.resolve(classFqn.replace('.', File.separatorChar) + ".java");
                if (sourceFile.getParent() != null) {
                    Files.createDirectories(sourceFile.getParent());
                }
                Files.writeString(sourceFile, sourceCode, StandardCharsets.UTF_8);

                String classpath = tempDir.toAbsolutePath().toString();
                if (extraClassPath != null && !extraClassPath.isEmpty()) {
                    classpath = extraClassPath + File.pathSeparator + classpath;
                }
                classpath = classpath + File.pathSeparator + getDefaultClasspath();

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
                    options.add("--release");
                    options.add(runtimeVersion);
                }

                if (!options.contains("-proc:none")) {
                    options.add("-proc:none");
                }
                options.add(sourceFile.toAbsolutePath().toString());

                Path argFile = tempDir.resolve("javac_args.txt");
                Files.write(argFile, options, StandardCharsets.UTF_8);

                List<String> command = List.of(javacPath.toAbsolutePath().toString(), "@" + argFile.toAbsolutePath());
                ProcessBuilder pb = new ProcessBuilder(command);
                pb.redirectErrorStream(true);
                Process process = pb.start();
                String output = new String(process.getInputStream().readAllBytes(), StandardCharsets.UTF_8);
                int exitCode = process.waitFor();

                if (exitCode != 0) {
                    throw new AgiToolException("Compilation error via javac (" + javacPath.getFileName() + "):\n" + output);
                }

                Map<String, byte[]> newBytes = new HashMap<>();
                try (Stream<Path> stream = Files.walk(tempDir)) {
                    for (Path file : stream.filter(p -> p.toString().endsWith(".class")).toList()) {
                        String relative = tempDir.relativize(file).toString();
                        String fqn = relative.replace(File.separatorChar, '.').replace('/', '.');
                        if (fqn.endsWith(".class")) {
                            fqn = fqn.substring(0, fqn.length() - 6);
                        }
                        // Only capture newly generated classes for this FQN (or its nested classes)
                        if (fqn.equals(classFqn) || fqn.startsWith(classFqn + "$")) {
                            newBytes.put(fqn, Files.readAllBytes(file));
                        }
                    }
                }
                return newBytes;
            } finally {
                try (Stream<Path> walk = Files.walk(tempDir)) {
                    walk.sorted(java.util.Comparator.reverseOrder())
                            .map(Path::toFile)
                            .forEach(File::delete);
                } catch (Exception ignored) {
                }
            }
        }

        JavaCompiler inMemoryCompiler = getDefaultJavaCompiler();
        if (inMemoryCompiler != null) {
            return compileInMemoryBytecodes(sourceCode, classFqn, extraClassPath, compilerOptions, inMemoryCompiler);
        }

        for (KnownJdk known : getKnownJdks()) {
            if (known.hasCompiler()) {
                return compileBytecodes(sourceCode, classFqn, extraClassPath, compilerOptions, known.javacPath());
            }
        }

        throw new AgiToolException("No Java compiler available for compile(). Running on a JRE without in-memory compiler, and no external JDK javac was found.");
    }

    /**
     * Compiles a modular Java class into the in-memory classpath of this AGI session without executing it.
     *
     * @param classFqn The fully qualified class name (e.g. 'uno.anahata.benchmarks.FlightContact').
     * @param sourceCode The Java source code of the class.
     * @param extraClassPath Optional extra classpath entries.
     * @param compilerOptions Optional compiler options.
     * @param jdk Optional JDK name or explicit path to a javac binary.
     * @return Confirmation message with compilation summary.
     * @throws Exception on compilation error.
     */
    @AgiTool(
            value = "Compiles a modular Java class into the in-memory classpath of this AGI session without executing it.\n"
            + "The compiled class is registered in the session's RAM and can be imported and instantiated by subsequent compile() and compileAndExecute() calls in this session.\n"
            + "Allows building complex, multi-file modular architectures across turns without squeezing all code into a single Anahata.java file."
    )
    public String compile(
            @AgiToolParam("The fully qualified class name (e.g. 'uno.anahata.benchmarks.FlightContact' or 'Airbase').") String classFqn,
            @AgiToolParam(value = "The Java source code of the class.", rendererId = "java") String sourceCode,
            @AgiToolParam(value = "Optional extra classpath entries separated by File.pathSeparator.", required = false) String extraClassPath,
            @AgiToolParam(value = "Optional compiler options.", required = false) String[] compilerOptions,
            @AgiToolParam(value = "Optional JDK name (from Available JDKs) or explicit path to javac binary.", required = false) String jdk) throws Exception {

        String fqn = classFqn != null ? classFqn.trim() : null;
        if (fqn == null || fqn.isBlank()) {
            throw new AgiToolException("classFqn parameter is required (e.g. 'uno.anahata.benchmarks.FlightContact').");
        }

        Path javacPath = resolveJavacPath(jdk);
        Map<String, byte[]> bytecodes = compileBytecodes(sourceCode, fqn, extraClassPath, compilerOptions, javacPath);

        if (bytecodes.isEmpty()) {
            throw new AgiToolException("Compilation produced zero .class bytecode for class '" + fqn + "'.");
        }

        int lineCount = sourceCode.split("\r\n|\r|\n").length;
        AgiCompiledClass agiClass = new AgiCompiledClass(fqn, sourceCode, bytecodes, extraClassPath, System.currentTimeMillis(), lineCount);

        // If this class was already defined in the active AgiClassLoader, reset loader to allow redefinition
        if (agiClassLoader != null && agiClassLoader.isClassLoaded(fqn)) {
            String msg = "Class '" + fqn + "' was already defined in active AgiClassLoader; resetting loader for class redefinition.";
            log.info(msg);
            log(msg);
            resetAgiClassLoader();
        }

        agiCompiledClasses.put(fqn, agiClass);
        if (extraClassPath != null && !extraClassPath.isBlank()) {
            getOrCreateAgiClassLoader().addExtraClassPath(extraClassPath);
        }

        int totalBytes = agiClass.getTotalBytecodeSize();
        log.info("Successfully compiled and registered AgiCompiledClass '{}' ({} bytes, {} classes/inner-classes)", fqn, totalBytes, bytecodes.size());

        StringBuilder sb = new StringBuilder();
        sb.append("SUCCESS: Compiled class '").append(fqn).append("' (")
                .append(String.format("%.1f KB", totalBytes / 1024.0)).append(", ")
                .append(lineCount).append(" lines");
        if (bytecodes.size() > 1) {
            sb.append(", nested classes: ").append(bytecodes.keySet());
        }
        sb.append(").\nActive session compiled classes (").append(agiCompiledClasses.size()).append("): ")
                .append(agiCompiledClasses.keySet());
        return sb.toString();
    }

    /**
     * Retrieves the stored Java source code for one or more previously compiled AGI classes.
     *
     * @param classFqns List of class fully qualified names to retrieve source code for.
     * @return Map of class FQN to its Java source code.
     */
    @AgiTool(
            value = "Retrieves the stored Java source code for one or more previously compiled AGI classes.\n"
            + "Use this to inspect or refactor classes compiled in previous turns that may have scrolled past the tool context window."
    )
    public Map<String, String> getAgiClassSources(
            @AgiToolParam("List of class fully qualified names to retrieve source code for.") List<String> classFqns) {

        Map<String, String> result = new HashMap<>();
        if (classFqns == null || classFqns.isEmpty()) {
            for (Map.Entry<String, AgiCompiledClass> entry : agiCompiledClasses.entrySet()) {
                result.put(entry.getKey(), entry.getValue().getSourceCode());
            }
            return result;
        }

        for (String fqn : classFqns) {
            if (fqn == null || fqn.isBlank()) {
                continue;
            }
            String trimmed = fqn.trim();
            AgiCompiledClass acc = agiCompiledClasses.get(trimmed);
            if (acc != null) {
                result.put(trimmed, acc.getSourceCode());
            } else {
                result.put(trimmed, "/* ERROR: Class '" + trimmed + "' not found in active AGI compiled classes. Available: " + agiCompiledClasses.keySet() + " */");
            }
        }
        return result;
    }

    /**
     * Removes one or more compiled classes from the AGI in-memory registry.
     *
     * @param classFqns List of class fully qualified names to remove.
     * @return Confirmation message with list of removed classes.
     */
    @AgiTool(
            value = "Removes one or more compiled classes from the AGI in-memory registry.\n"
            + "Purges the class bytecode and source code from RAM."
    )
    public String removeAgiClasses(
            @AgiToolParam("List of class fully qualified names to remove from the session registry.") List<String> classFqns) {

        if (classFqns == null || classFqns.isEmpty()) {
            return "No class FQNs provided to remove.";
        }

        List<String> removed = new ArrayList<>();
        for (String fqn : classFqns) {
            if (fqn == null || fqn.isBlank()) {
                continue;
            }
            String trimmed = fqn.trim();
            if (agiCompiledClasses.remove(trimmed) != null) {
                removed.add(trimmed);
            }
        }

        log("Removed " + removed.size() + " classes. Resetting AgiClassLoader to unload purged classes from JVM metaspace.");
        resetAgiClassLoader();

        return "Removed " + removed.size() + " classes: " + removed + ". Remaining active classes: " + agiCompiledClasses.keySet();
    }

    /**
     * Clears all compiled classes and sources from the AGI in-memory registry.
     *
     * @return Confirmation message.
     */
    @AgiTool(
            value = "Clears all compiled classes and sources from the AGI in-memory registry, resetting the in-memory workspace to clean state."
    )
    public String clearAllAgiClasses() {
        int count = agiCompiledClasses.size();
        agiCompiledClasses.clear();
        log("Cleared all " + count + " compiled classes. Resetting AgiClassLoader.");
        resetAgiClassLoader();
        return "Cleared all " + count + " in-memory compiled classes from active AGI registry.";
    }
}
