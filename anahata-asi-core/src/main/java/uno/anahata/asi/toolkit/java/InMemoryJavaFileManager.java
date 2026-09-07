/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.toolkit.java;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.net.URI;
import java.util.HashMap;
import java.util.Map;
import javax.tools.FileObject;
import javax.tools.ForwardingJavaFileManager;
import javax.tools.JavaFileManager;
import javax.tools.JavaFileObject;
import javax.tools.SimpleJavaFileObject;

/**
 * An in-memory {@link ForwardingJavaFileManager} that intercepts compiled bytecode
 * output from {@link javax.tools.JavaCompiler} and collects it directly into byte arrays.
 * <p>
 * Eliminates the need for reflection hacks when extracting compiled bytecode from
 * dynamic compilation tasks.
 * </p>
 *
 * @author anahata
 */
public class InMemoryJavaFileManager extends ForwardingJavaFileManager<JavaFileManager> {

    private final Map<String, ByteArrayOutputStream> compiledStreams = new HashMap<>();

    /**
     * Constructs a new in-memory file manager wrapping the given standard file manager.
     *
     * @param fileManager the delegate standard file manager.
     */
    public InMemoryJavaFileManager(JavaFileManager fileManager) {
        super(fileManager);
    }

    /**
     * {@inheritDoc}
     * <p>
     * Intercepts output of class files to capture their bytes in memory rather than
     * writing them to disk.
     * </p>
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
}
