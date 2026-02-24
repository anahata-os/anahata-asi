/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.tools.files.nb;

import java.io.IOException;
import lombok.extern.slf4j.Slf4j;
import org.openide.filesystems.FileObject;
import uno.anahata.asi.model.resource.files.BinaryFileResource;
import uno.anahata.asi.resource.ResourceManager;

/**
 * A NetBeans-specific implementation of {@link BinaryFileResource} for handling 
 * multimodal content (images, videos, etc.).
 * <p>
 * It uses {@link NbFileObjectResourceHelper} to manage the underlying FileObject 
 * and its listeners. It uses the default LIVE refresh policy to ensure the 
 * model always receives the latest content.
 * </p>
 * 
 * @author anahata
 */
@Slf4j
public class NbBinaryFileResource extends BinaryFileResource {

    /** The helper for FileObject management. */
    private final NbFileObjectResourceHelper helper;

    /**
     * Constructs a new NbBinaryFileResource.
     * 
     * @param manager The parent resource manager.
     * @param fo The NetBeans FileObject to wrap.
     * @throws Exception if the initial setup fails.
     */
    public NbBinaryFileResource(ResourceManager manager, FileObject fo) throws Exception {
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
                log.debug("Failed to get HTML display name for " + fo, e);
            }
        }
        return super.getHtmlDisplayName();
    }

    /** {@inheritDoc} */
    @Override
    protected byte[] reloadContent() throws Exception {
        FileObject fo = helper.getFileObject();
        if (fo == null) {
            throw new IOException("FileObject not available for: " + getPath());
        }
        return fo.asBytes();
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
