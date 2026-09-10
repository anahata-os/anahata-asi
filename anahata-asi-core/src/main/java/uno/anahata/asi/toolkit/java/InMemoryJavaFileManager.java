/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.toolkit.java;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URI;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import javax.tools.FileObject;
import javax.tools.ForwardingJavaFileManager;
import javax.tools.JavaFileManager;
import javax.tools.JavaFileObject;
import javax.tools.SimpleJavaFileObject;
import javax.tools.StandardLocation;

/**
 * An in-memory {@link ForwardingJavaFileManager} that intercepts compiled bytecode
 * output from {@link javax.tools.JavaCompiler} and collects it directly into byte arrays.
 * Also serves previously compiled in-memory classes as input symbols during subsequent
 * compilation tasks.
 * <p>
 * Eliminates the need for reflection hacks when extracting compiled bytecode from
 * dynamic compilation tasks.
 * </p>
 *
 * @author anahata
 */
public class InMemoryJavaFileManager extends ForwardingJavaFileManager<JavaFileManager> {

    /**
     * Map of class binary names to their intercepted in-memory byte output streams.
     */
    private final Map<String, ByteArrayOutputStream> compiledStreams = new HashMap<>();

    /**
     * Map of pre-existing in-memory compiled bytecode from previous AGI compilation turns.
     */
    private final Map<String, byte[]> existingBytecodes = new HashMap<>();

    /**
     * Constructs a new in-memory file manager wrapping the given standard file manager.
     *
     * @param fileManager the delegate standard file manager.
     */
    public InMemoryJavaFileManager(JavaFileManager fileManager) {
        super(fileManager);
    }

    /**
     * Constructs a new in-memory file manager wrapping the given standard file manager
     * with pre-existing in-memory class bytecodes.
     *
     * @param fileManager the delegate standard file manager.
     * @param existingBytecodes pre-existing compiled class bytecodes (class FQN -> byte[]).
     */
    public InMemoryJavaFileManager(JavaFileManager fileManager, Map<String, byte[]> existingBytecodes) {
        super(fileManager);
        if (existingBytecodes != null && !existingBytecodes.isEmpty()) {
            this.existingBytecodes.putAll(existingBytecodes);
        }
    }

    /**
     * {@inheritDoc}
     * <p>
     * Intercepts output of class files to capture their bytes in memory rather than
     * writing them to disk.
     * </p>
     *
     * @param location the package location.
     * @param className the name of the new class.
     * @param kind the kind of file, expected to be {@link JavaFileObject.Kind#CLASS}.
     * @param sibling a file object used as a hint for placement; may be {@code null}.
     * @return a file object for output.
     * @throws IOException if an I/O error occurred.
     */
    @Override
    public JavaFileObject getJavaFileForOutput(
            JavaFileManager.Location location,
            String className,
            JavaFileObject.Kind kind,
            FileObject sibling) throws IOException {

        if (kind == JavaFileObject.Kind.CLASS) {
            ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
            compiledStreams.put(className, outputStream);
            return new SimpleJavaFileObject(URI.create("mem:///" + className.replace('.', '/') + ".class"), JavaFileObject.Kind.CLASS) {
                @Override
                public OutputStream openOutputStream() throws IOException {
                    return outputStream;
                }
            };
        }
        return super.getJavaFileForOutput(location, className, kind, sibling);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Iterable<JavaFileObject> list(
            JavaFileManager.Location location,
            String packageName,
            Set<JavaFileObject.Kind> kinds,
            boolean recurse) throws IOException {

        Iterable<JavaFileObject> standard = super.list(location, packageName, kinds, recurse);
        if ((location == StandardLocation.CLASS_PATH || location == StandardLocation.CLASS_OUTPUT)
                && kinds != null && kinds.contains(JavaFileObject.Kind.CLASS)) {

            List<JavaFileObject> inMemoryList = new ArrayList<>();
            Map<String, byte[]> allInMem = new HashMap<>(existingBytecodes);
            for (Map.Entry<String, ByteArrayOutputStream> entry : compiledStreams.entrySet()) {
                allInMem.put(entry.getKey(), entry.getValue().toByteArray());
            }

            for (Map.Entry<String, byte[]> entry : allInMem.entrySet()) {
                String classFqn = entry.getKey();
                String pkg = "";
                int lastDot = classFqn.lastIndexOf('.');
                if (lastDot > 0) {
                    pkg = classFqn.substring(0, lastDot);
                }

                boolean matches = recurse
                        ? (packageName.isEmpty() || pkg.equals(packageName) || pkg.startsWith(packageName + "."))
                        : pkg.equals(packageName);

                if (matches) {
                    inMemoryList.add(new InMemoryJavaClassFileObject(classFqn, entry.getValue()));
                }
            }

            if (!inMemoryList.isEmpty()) {
                List<JavaFileObject> combined = new ArrayList<>();
                if (standard != null) {
                    for (JavaFileObject jfo : standard) {
                        combined.add(jfo);
                    }
                }
                combined.addAll(inMemoryList);
                return combined;
            }
        }
        return standard;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public JavaFileObject getJavaFileForInput(
            JavaFileManager.Location location,
            String className,
            JavaFileObject.Kind kind) throws IOException {

        if (kind == JavaFileObject.Kind.CLASS) {
            byte[] bytes = existingBytecodes.get(className);
            if (bytes == null && compiledStreams.containsKey(className)) {
                bytes = compiledStreams.get(className).toByteArray();
            }
            if (bytes != null) {
                return new InMemoryJavaClassFileObject(className, bytes);
            }
        }
        return super.getJavaFileForInput(location, className, kind);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String inferBinaryName(JavaFileManager.Location location, JavaFileObject file) {
        if (file instanceof InMemoryJavaClassFileObject inMem) {
            return inMem.getClassName();
        }
        return super.inferBinaryName(location, file);
    }

    /**
     * Retrieves all compiled class bytecode accumulated by this file manager.
     *
     * @return a map of class fully qualified name (FQN) to compiled bytecode bytes.
     */
    public Map<String, byte[]> getCompiledClasses() {
        Map<String, byte[]> result = new HashMap<>();
        for (Map.Entry<String, ByteArrayOutputStream> entry : compiledStreams.entrySet()) {
            result.put(entry.getKey(), entry.getValue().toByteArray());
        }
        return result;
    }

    /**
     * Simple in-memory class representation for compiler input.
     */
    public static class InMemoryJavaClassFileObject extends SimpleJavaFileObject {

        private final String className;
        private final byte[] bytecode;

        public InMemoryJavaClassFileObject(String className, byte[] bytecode) {
            super(URI.create("mem:///" + className.replace('.', '/') + Kind.CLASS.extension), Kind.CLASS);
            this.className = className;
            this.bytecode = bytecode;
        }

        public String getClassName() {
            return className;
        }

        @Override
        public InputStream openInputStream() {
            return new ByteArrayInputStream(bytecode);
        }
    }
}
