package uno.anahata.ai.toolkit;

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
import uno.anahata.ai.chat.Chat;
import uno.anahata.ai.internal.ClasspathPrinter;
import uno.anahata.ai.model.core.RagMessage;
import uno.anahata.ai.model.core.TextPart;
import uno.anahata.ai.model.tool.java.JavaMethodTool;
import uno.anahata.ai.model.tool.java.JavaMethodToolResponse;
import uno.anahata.ai.tool.AiTool;
import uno.anahata.ai.tool.AiToolException;
import uno.anahata.ai.tool.AiToolParam;
import uno.anahata.ai.tool.AiToolkit;
import uno.anahata.ai.tool.AnahataTool;
import uno.anahata.ai.tool.HandyToolStuff;
import uno.anahata.ai.tool.AnahataToolkit;

/**
 *
 * @author anahata
 */
@Slf4j
@AiToolkit("Toolkit for compiling and executing java code, has a 'temp' HashMap for storing java objects across turns / tool calls and uses a child first classloader if additional classpath entries are provided")
public class Java extends AnahataToolkit {
    

    /**
     * A session scoped map
     */
    public Map sessionMap = Collections.synchronizedMap(new HashMap());
    
    /**
     * An application scoped map
     */
    public static Map applicationMap = Collections.synchronizedMap(new HashMap());
    
    /**
     * The base compiler and classloader classpath (extra can be provided at execution time)
     */
    public String defaultCompilerClasspath;

    /**
     * Taraaa
     */
    public Java() {
        defaultCompilerClasspath = System.getProperty("java.class.path");
        log.info("Java toolkit instantiated:");
    }

    @AiTool("The full default classpath for compiling java code and for class loading")
    public String getDefaultClasspath() {
        return defaultCompilerClasspath;
    }

    @AiTool("Sets the default classpath for the compiler and classloader")
    public void setDefaultClasspath(@AiToolParam("The default classpath for all code compiled by the Java toolkit") String defaultCompilerClasspath) {
        this.defaultCompilerClasspath = defaultCompilerClasspath;
    }

    /**
     * Super beautiful version of the classpath that uses way less tokens than the full one.
     * ç
     * @return 
     */
    public String getPrettyPrintedDefaultClasspath() {
        return ClasspathPrinter.prettyPrint(defaultCompilerClasspath);
    }

    /**
     * Throws the session and application map keys and the pretty printed classpath on the rag message.
     * 
     * @param ragMessage - the ragMessage to be injected with content.
     * @throws Exception 
     */
    @Override
    public void populateMessage(RagMessage ragMessage) throws Exception {
        String ragText = "\nSession map keys: " + sessionMap.keySet()
                + "\nApplication map keys: " + applicationMap.keySet()
                + "\nDefault Compiler and ClassLoader Classpath (abbreviated):\n" + getPrettyPrintedDefaultClasspath();
        new TextPart(ragMessage, ragText);
    }
    
    @Override
    public List<String> getSystemInstructionParts(Chat chat) throws Exception {
        StringBuilder sb = new StringBuilder();
        sb.append("### Java Toolkit Instructions: \n");
        sb.append("When using `compileAndExecute`, your class should extend `uno.anahata.ai.tool.AnahataTool`, have no package declaration and implement the call method of Callable<Object>. ");
        sb.append("This provides the following helper methods for a rich, context-aware execution:\n\n");
        
        sb.append("#### Available Methods that you can use within the code you write (inherited from AnahataTool):\n");
        appendMethods(sb, AnahataTool.class);
        appendMethods(sb, HandyToolStuff.class);
        
        sb.append("\nAbout the maps: the session map is for you only (chat scoped) and the application map to be shared all other instances of you (jvm scoped)\n");
        sb.append("\nAbout the attachments: at the time of this release (only tested with gemini-3-flash) only pdf, text and image attachments are supported\n");
        
        sb.append("\n#### Example:\n");
        sb.append("```java\n");
        sb.append("import uno.anahata.ai.tool.AnahataTool;\n");
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
        sb.append("        // You can also add errors or attachments\n");
        sb.append("        // error(\"Something went wrong\");\n");
        sb.append("        // addAttachment(data, \"image/png\");\n");
        sb.append("        \n");
        sb.append("        return result;\n");
        sb.append("    }\n");
        sb.append("}\n");
        sb.append("```\n");
        
        
        return Collections.singletonList(sb.toString());
    }

    private void appendMethods(StringBuilder sb, Class<?> clazz) {
        
        for (Method m : clazz.getDeclaredMethods()) {
            String methodString = JavaMethodTool.buildMethodSignature(m);
            if (!methodString.contains("anahata")) {
                sb.append("- `").append(methodString).append("`\n");
            }
        }
    }

    @AiTool("Compiles the source code of a java class with the default compiler classpath")
    public Class compile(
            @AiToolParam(value = "The source code", rendererId = "java") String sourceCode,
            @AiToolParam("The class name") String className,
            @AiToolParam(value = "Additional classpath entries", required = false) String extraClassPath,
            @AiToolParam(value = "Additional compiler options", required = false) String[] compilerOptions)
            throws ClassNotFoundException, NoSuchMethodException, IllegalAccessException, InvocationTargetException {
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
            log.info("extraClassPath: {} entries:\n{}", extraClassPath.split(File.pathSeparator).length, extraClassPath);
        }

        log.info("defaultCompilerClasspath: {} entries:", defaultCompilerClasspath.split(File.pathSeparator).length);

        String classpath = defaultCompilerClasspath;
        if (extraClassPath != null && !extraClassPath.isEmpty()) {
            // CRITICAL FIX: Prepend extraClassPath to ensure hot-reloaded classes take precedence
            classpath = extraClassPath + File.pathSeparator + classpath;
        }

        log.info("total classpathEntries: {} entries:", classpath.split(File.pathSeparator).length);
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
                        // 2. Check for our in-memory compiled class first (the "hot-reload" part for Anahata.java)
                        byte[] bytes = compiledClasses.get(name);
                        if (bytes != null) {
                            log.info("Hot-reloading in-memory class: {}", name);
                            c = defineClass(name, bytes, 0, bytes.length);
                        } else {
                            try {
                                // 3. CHILD-FIRST: Try to find the class in our own URLs (e.g., target/classes)
                                c = findClass(name);
                                log.info("Loaded class from extraClassPath (Child-First): {}", name);
                            } catch (ClassNotFoundException e) {
                                // 4. PARENT-LAST: If not found, delegate to the parent classloader.
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

    @AiTool(
            value = "Compiles and executes a Java class named 'Anahata' on the application's JVM.\n"
            + "The class should:\n"
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
        Object o = c.getDeclaredConstructor().newInstance();

        if (o instanceof Callable callable) {
            log.info("Calling call() method on Callable (or AnahataTool)");
            return callable.call();
        } else {
            throw new AiToolException("Source file should extend AnahataTool or implement java.util.Callable");
        }
    }
}
