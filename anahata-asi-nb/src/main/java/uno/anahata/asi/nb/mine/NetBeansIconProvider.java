/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.mine;

import java.awt.Image;
import java.io.File;
import javax.swing.Icon;
import org.netbeans.api.project.Project;
import org.netbeans.api.project.ProjectUtils;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import org.openide.loaders.DataObject;
import org.openide.util.ImageUtilities;
import uno.anahata.asi.context.ContextProvider;
import uno.anahata.asi.model.resource.AbstractPathResource;
import uno.anahata.asi.model.tool.AbstractTool;
import uno.anahata.asi.model.tool.AbstractToolkit;
import uno.anahata.asi.nb.tools.project.context.ProjectContextProvider;
import uno.anahata.asi.swing.icons.IconProvider;
import uno.anahata.asi.swing.icons.IconUtils;

/**
 * A NetBeans-specific implementation of {@link IconProvider} that fetches 
 * authentic, annotated IDE icons for projects and files.
 * <p>
 * This provider ensures that the Context tab displays the exact same icons 
 * as the Projects and Files tabs, including Git badges and error markers.
 * </p>
 * 
 * @author anahata
 */
public class NetBeansIconProvider implements IconProvider {

    /** {@inheritDoc} */
    @Override
    public Icon getIconFor(ContextProvider cp) {
        if (cp instanceof ProjectContextProvider pcp) {
            Project p = pcp.getProject();
            if (p != null) {
                return ProjectUtils.getInformation(p).getIcon();
            }
        } else if (cp instanceof AbstractPathResource<?, ?> apr) {
            FileObject fo = FileUtil.toFileObject(new File(apr.getPath()));
            if (fo != null) {
                try {
                    DataObject dobj = DataObject.find(fo);
                    Image img = dobj.getNodeDelegate().getIcon(java.beans.BeanInfo.ICON_COLOR_16x16);
                    return ImageUtilities.image2Icon(img);
                } catch (Exception e) {
                    // Fallback to default
                }
            }
        }
        
        return IconUtils.getIcon(cp.getIconId());
    }

    /** {@inheritDoc} */
    @Override
    public Icon getIconFor(AbstractToolkit<?> toolkit) {
        // Return null to allow the renderer to use its own default toolkit icon (java.png)
        return null;
    }

    /** {@inheritDoc} */
    @Override
    public Icon getIconFor(AbstractTool<?, ?> tool) {
        // Return null to allow the renderer to use its own default tool icon
        return null;
    }
}
