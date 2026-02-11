/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.tools.files.nb;

import com.fasterxml.jackson.annotation.JsonIgnore;
import java.io.IOException;
import lombok.extern.slf4j.Slf4j;
import org.openide.filesystems.FileObject;
import uno.anahata.asi.model.resource.files.TextFileResource;
import uno.anahata.asi.resource.ResourceManager;

/**
 * A NetBeans-specific implementation of {@link TextFileResource} that wraps a 
 * {@link FileObject} and listens for filesystem changes.
 * <p>
 * It uses {@link NbFileObjectResourceHelper} to manage the underlying FileObject 
 * and its listeners. It uses the default LIVE refresh policy to ensure the 
 * model always receives the latest content.
 * </p>
 * 
 * @author anahata
 */
@Slf4j
public class NbTextFileResource extends TextFileResource {

    /** 
     * The helper for FileObject management. 
     * Ignored by Jackson to prevent circular serialization loops with the IDE runtime.
     */
    @JsonIgnore
    private final NbFileObjectResourceHelper helper;

    /**
     * Constructs a new NbTextFileResource.
     * 
     * @param manager The parent resource manager.
     * @param fo The NetBeans FileObject to wrap.
     * @throws Exception if the initial setup fails.
     */
    public NbTextFileResource(ResourceManager manager, FileObject fo) throws Exception {
        super(manager, org.openide.filesystems.FileUtil.toFile(fo).toPath());
        this.helper = new NbFileObjectResourceHelper(this, fo);
    }

    /** {@inheritDoc} */
    @Override
    public String getHtmlDisplayName() {
        FileObject fo = helper.getFileObject();
        if (fo != null) {
            try {
                org.openide.nodes.Node node = org.openide.loaders.DataObject.find(fo).getNodeDelegate();
                String html = node.getHtmlDisplayName();
                return html != null ? html : node.getDisplayName();
            } catch (Exception e) {
                // Ignore and fall back
            }
        }
        return super.getHtmlDisplayName();
    }

    /**
     * {@inheritDoc}
     * Implementation details: Uses {@link FileObject#asText()} for efficient 
     * reading and updates the load timestamp from the FileObject metadata.
     */
    @Override
    public void reload() throws Exception {
        FileObject fo = helper.getFileObject();
        if (fo == null) {
            throw new IOException("FileObject not available for: " + getPath());
        }
        
        log.info("Reloading NbTextFileResource: {}", getPath());
        String content = fo.asText();
        this.setLoadLastModified(fo.lastModified().getTime());
        
        this.getViewport().process(content);
        
        String oldCache = this.cache;
        this.cache = getViewport().getProcessedText();
        propertyChangeSupport.firePropertyChange("cache", oldCache, cache);
    }

    /** {@inheritDoc} */
    @Override
    public long getCurrentLastModified() throws IOException {
        FileObject fo = helper.getFileObject();
        return fo != null ? fo.lastModified().getTime() : 0L;
    }

    /** {@inheritDoc} */
    @Override
    public boolean exists() {
        FileObject fo = helper.getFileObject();
        return fo != null && fo.isValid();
    }

    /** {@inheritDoc} */
    @Override
    public void rebind() {
        super.rebind();
        helper.rebind();
    }

    /** {@inheritDoc} */
    @Override
    public void dispose() {
        super.dispose();
        helper.dispose();
    }
}
