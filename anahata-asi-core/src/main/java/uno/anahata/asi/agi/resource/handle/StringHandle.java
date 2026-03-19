/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.agi.resource.handle;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.util.Objects;
import lombok.Getter;
import lombok.Setter;
import lombok.SneakyThrows;

/**
 * A memory-backed resource handle for virtual or proposed content.
 * <p>
 * This handle allows arbitrary strings to be managed as V2 Resources. 
 * It is primarily used for chat code blocks and tool parameters, enabling 
 * them to use the same high-fidelity viewer pipeline as physical files.
 * </p>
 * 
 * @author anahata
 */
public class StringHandle extends AbstractResourceHandle {

    /** The memory URI of the snippet. */
    @Getter
    private final URI uri;
    /** The display name of the snippet. */
    @Getter
    private final String name;
    
    /** The actual text content held in memory. */
    @Getter
    @Setter
    private String content;
    
    /** The last modification timestamp. */
    private long lastModified = System.currentTimeMillis();

    /**
     * Constructs a new StringHandle.
     * @param name The display name (e.g., "proposed.java").
     * @param content The initial text content.
     */
    @SneakyThrows
    public StringHandle(String name, String content) {
        this.name = name;
        this.content = content;
        this.uri = URI.create("mem:///" + URLEncoder.encode(name, "UTF-8"));
    }

    /** 
     * {@inheritDoc} 
     * <p>Returns the mutable modification timestamp.</p>
     */
    @Override
    public long getLastModified() {
        return lastModified;
    }

    /** 
     * {@inheritDoc} 
     * <p>Memory snippets always exist while the handle is alive.</p>
     */
    @Override
    public boolean exists() {
        return true;
    }

    /** 
     * {@inheritDoc} 
     * <p>Opens a stream over the internal UTF-8 byte array.</p>
     */
    @Override
    public InputStream openStream() throws IOException {
        return new ByteArrayInputStream(content.getBytes(StandardCharsets.UTF_8));
    }

    /** 
     * {@inheritDoc} 
     * <p>Virtual snippets are always writable in memory.</p>
     */
    @Override
    public boolean isWritable() {
        return true; 
    }

    /** 
     * {@inheritDoc} 
     * <p>Updates the internal string and notifies the owner orchestrator. 
     * Implements an equality gate to prevent redundant interpretation cycles.</p>
     */
    @Override
    public void write(String content) throws IOException {
        if (Objects.equals(this.content, content)) {
            return;
        }
        
        this.content = content;
        this.lastModified = System.currentTimeMillis();
        if (owner != null) {
            owner.markDirty();
        }
    }

    /** 
     * {@inheritDoc} 
     * <p>Implementation details: returns {@code true} as this is a memory handle.</p>
     */
    @Override
    public boolean isVirtual() {
        return true;
    }

    /**
     * {@inheritDoc} 
     * <p>Memory handles are always considered textual in the current platform version.</p>
     */
    @Override
    public boolean isTextual() {
        return true;
    }

    /**
     * {@inheritDoc} 
     * <p>Memory handles default to text/plain. The viewer is responsible for 
     * environment-specific language detection via the resource name.</p>
     */
    @Override
    public String getMimeType() {
        return "text/plain";
    }
}
