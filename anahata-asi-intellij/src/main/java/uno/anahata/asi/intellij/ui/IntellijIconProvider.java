/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.intellij.ui;

import com.intellij.icons.AllIcons;
import com.intellij.openapi.application.ReadAction;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.util.IconUtil;
import javax.swing.Icon;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.context.ContextProvider;
import uno.anahata.asi.agi.resource.Resource;
import uno.anahata.asi.agi.resource.handle.PathHandle;
import uno.anahata.asi.agi.tool.spi.AbstractTool;
import uno.anahata.asi.agi.tool.spi.AbstractToolkit;
import uno.anahata.asi.intellij.internal.JavaPsi;
import uno.anahata.asi.intellij.resources.handle.IntellijHandle;
import uno.anahata.asi.intellij.tools.project.context.ProjectContextProvider;
import uno.anahata.asi.swing.icons.IconProvider;
import uno.anahata.asi.swing.icons.IconUtils;

/**
 * An IntelliJ-specific implementation of {@link IconProvider} that fetches
 * authentic IDE file type icons and project icons for the context tree.
 * <p>
 * Ensures that the Context tab displays the authentic IntelliJ file type icons
 * and project icons matching the IntelliJ Project view.
 * </p>
 *
 * @author anahata
 */
@Slf4j
public class IntellijIconProvider implements IconProvider {

    /**
     * Constructs a new IntelliJ icon provider.
     */
    public IntellijIconProvider() {
    }

    /**
     * {@inheritDoc}
     * <p>
     * 1. If the provider is a project, retrieves the authentic IntelliJ project icon.
     * 2. If the provider is a resource with an {@link IntellijHandle}, queries the
     *    live {@link VirtualFile}'s file type to get its authentic IDE icon.
     * 3. Falls back to the global registry for static icons.
     * </p>
     */
    @Override
    public Icon getIconFor(ContextProvider cp) {
        if (cp instanceof ProjectContextProvider) {
            return AllIcons.Nodes.Project;
        } else if (cp instanceof Resource res) {
            VirtualFile vf = null;
            if (res.getHandle() instanceof IntellijHandle ih) {
                vf = ih.getVirtualFile();
            } else if (res.getHandle() instanceof PathHandle ph) {
                vf = JavaPsi.findVirtualFile(ph.getPath());
            }

            if (vf != null) {
                if ("anahata.md".equalsIgnoreCase(vf.getName())) {
                    return AnahataFileIconProvider.getFileIcon();
                }
                try {
                    final VirtualFile targetVf = vf;
                    Project project = JavaPsi.findHostProject(targetVf);
                    Icon richIcon = ReadAction.compute(() -> IconUtil.getIcon(targetVf, 0, project));
                    if (richIcon != null) {
                        return richIcon;
                    }
                } catch (Throwable t) {
                    log.debug("Failed to resolve rich icon for resource: {} ({})", res, t.getMessage());
                }
                Icon fileIcon = vf.getFileType().getIcon();
                if (fileIcon != null) {
                    return fileIcon;
                }
            }
        }
        return IconUtils.getIcon(cp.getIconId());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Icon getIconFor(AbstractToolkit<?> toolkit) {
        return null;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Icon getIconFor(AbstractTool<?, ?> tool) {
        return null;
    }
}
