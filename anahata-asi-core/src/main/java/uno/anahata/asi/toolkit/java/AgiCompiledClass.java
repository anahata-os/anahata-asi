/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.toolkit.java;

import java.io.Serializable;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

/**
 * Represents an in-memory compiled Java class within an AGI session.
 * Stores source code, compiled bytecode (including any nested/inner classes),
 * and compilation metadata.
 *
 * @author anahata
 */
@Getter
@Setter
@NoArgsConstructor
public class AgiCompiledClass implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * The fully qualified class name (e.g. 'uno.anahata.benchmarks.FlightContact').
     */
    private String fqn;

    /**
     * The original Java source code.
     */
    private String sourceCode;

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
     * Number of lines in the source code.
     */
    private int sourceLines;

    /**
     * Constructs a new {@link AgiCompiledClass}.
     *
     * @param fqn the fully qualified class name.
     * @param sourceCode the Java source code.
     * @param bytecodes the compiled bytecode map.
     * @param extraClassPath optional extra classpath entries used during compilation.
     * @param compiledAtMillis epoch timestamp in milliseconds.
     * @param sourceLines total source line count.
     */
    public AgiCompiledClass(String fqn, String sourceCode, Map<String, byte[]> bytecodes, String extraClassPath, long compiledAtMillis, int sourceLines) {
        this.fqn = fqn;
        this.sourceCode = sourceCode;
        this.bytecodes = bytecodes != null ? new HashMap<>(bytecodes) : new HashMap<>();
        this.extraClassPath = extraClassPath;
        this.compiledAtMillis = compiledAtMillis;
        this.sourceLines = sourceLines;
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
}
