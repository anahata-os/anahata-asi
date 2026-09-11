/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.toolkit.java;

import java.io.Serializable;
import java.text.SimpleDateFormat;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import lombok.Getter;
import lombok.NonNull;
import lombok.Setter;
import uno.anahata.asi.agi.context.BasicContextProvider;
import uno.anahata.asi.agi.message.RagMessage;

/**
 * Represents an in-memory compiled Java class within an AGI session.
 * Holds its {@link AgiClassSource}, compiled bytecode (including any nested/inner classes),
 * and compilation metadata.
 * <p>
 * Extends {@link BasicContextProvider} to automatically provide its source code in the
 * RAG message by default.
 * </p>
 *
 * @author anahata
 */
@Getter
@Setter
public class AgiCompiledClass extends BasicContextProvider implements Serializable {

    /**
     * The source code descriptor (FQN and source code). Cannot be null.
     */
    private final @NonNull AgiClassSource source;

    /**
     * The compiled bytecode for the top-level class and all nested/inner/anonymous classes,
     * mapped by their binary class names (e.g. 'com.foo.Bar' -> byte[], 'com.foo.Bar$Inner' -> byte[]).
     */
    private Map<String, byte[]> bytecodes = new HashMap<>();

    /**
     * Optional extra classpath entries used during compilation of this class.
     */
    private String extraClassPath;

    /**
     * Epoch timestamp in milliseconds when this class was compiled.
     */
    private long compiledAtMillis;

    /**
     * Constructs a new {@link AgiCompiledClass} from an {@link AgiClassSource} with its parent Java toolkit.
     *
     * @param javaToolkit the parent Java toolkit.
     * @param source the source descriptor.
     * @param bytecodes the compiled bytecode map.
     * @param extraClassPath optional extra classpath entries used during compilation.
     * @param compiledAtMillis epoch timestamp in milliseconds.
     */
    public AgiCompiledClass(@NonNull Java javaToolkit, @NonNull AgiClassSource source, Map<String, byte[]> bytecodes, String extraClassPath, long compiledAtMillis) {
        super(source.fqn(), source.fqn(), "In-memory compiled Java class: " + source.fqn() + " (" + source.getLineCount() + " lines)");
        this.source = source;
        this.bytecodes = bytecodes != null ? new HashMap<>(bytecodes) : new HashMap<>();
        this.extraClassPath = extraClassPath;
        this.compiledAtMillis = compiledAtMillis;
        setParent(javaToolkit);
    }

    /**
     * Convenience delegate to obtain the class fully qualified name from {@link #source}.
     *
     * @return the class FQN.
     */
    public String getFqn() {
        return source.fqn();
    }

    /**
     * Convenience delegate to obtain the Java source code from {@link #source}.
     *
     * @return the Java source code.
     */
    public String getSourceCode() {
        return source.sourceCode();
    }

    /**
     * Convenience delegate to obtain the source lines count from {@link #source}.
     *
     * @return total line count.
     */
    public int getSourceLines() {
        return source.getLineCount();
    }

    /**
     * Calculates the total size of all compiled bytecode in bytes.
     *
     * @return total bytecode size in bytes.
     */
    public int getTotalBytecodeSize() {
        if (bytecodes == null || bytecodes.isEmpty()) {
            return 0;
        }
        int total = 0;
        for (byte[] b : bytecodes.values()) {
            if (b != null) {
                total += b.length;
            }
        }
        return total;
    }

    /**
     * Returns an unmodifiable view of the bytecode map.
     *
     * @return unmodifiable map of class binary name to bytecode.
     */
    public Map<String, byte[]> getBytecodes() {
        return bytecodes != null ? Collections.unmodifiableMap(bytecodes) : Collections.emptyMap();
    }

    /**
     * {@inheritDoc}
     * <p>
     * Appends the in-memory compiled class header and source code to the RAG message.
     * </p>
     */
    @Override
    public void populateMessage(RagMessage ragMessage) throws Exception {
        SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
        StringBuilder sb = new StringBuilder();
        sb.append("Compiled Class: ").append(getFqn()).append("\n");
        sb.append("Compiled At   : ").append(sdf.format(new Date(compiledAtMillis))).append("\n");
        sb.append("Bytecode Size : ").append(String.format("%,d bytes", getTotalBytecodeSize())).append("\n");
        sb.append("Binary Classes: ").append(bytecodes.keySet()).append("\n");
        sb.append("Source Lines  : ").append(getSourceLines()).append("\n");
        if (extraClassPath != null && !extraClassPath.isBlank()) {
            sb.append("Extra CP      : ").append(extraClassPath).append("\n");
        }
        sb.append("```java\n").append(getSourceCode()).append("\n```");
        ragMessage.addTextPart(sb.toString());
    }
}
