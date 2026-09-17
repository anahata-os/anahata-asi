/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.intellij.tools.ide.context;

import com.intellij.ide.projectView.PresentationData;
import com.intellij.ide.projectView.ProjectViewNode;
import com.intellij.ide.projectView.ProjectViewNodeDecorator;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.ui.LayeredIcon;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import javax.swing.Icon;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.agi.context.ContextAnnotationFormatter;
import uno.anahata.asi.agi.resource.Resource;
import uno.anahata.asi.intellij.internal.AgiContext;
import uno.anahata.asi.intellij.ui.AnahataFileIconProvider;

/**
 * Badges Project-view nodes whose file or directory is in one or more active AGI session contexts.
 * <p>
 * Displays sleek session annotations via {@link ContextAnnotationFormatter} and layers an
 * Anahata badge icon onto the node icon.
 * </p>
 *
 * @author anahata
 */
public class AgiContextDecorator implements ProjectViewNodeDecorator {

    /**
     * Constructs the decorator (instantiated by the platform via its public no-arg constructor).
     */
    public AgiContextDecorator() {
    }

    /**
     * {@inheritDoc}
     * <p>
     * Decorates both files and directories with context presence counts, session nicknames,
     * tooltips, and badge icons.
     * </p>
     */
    @Override
    public void decorate(ProjectViewNode<?> node, PresentationData data) {
        VirtualFile file = node.getVirtualFile();
        if (file == null || !file.isInLocalFileSystem()) {
            return;
        }

        Map<Agi, List<Resource>> sessionRes = AgiContext.sessionResources(file);
        if (sessionRes.isEmpty()) {
            return;
        }

        Map<Agi, Integer> sessionCounts = new LinkedHashMap<>();
        sessionRes.forEach((agi, list) -> sessionCounts.put(agi, list.size()));

        String annotation = ContextAnnotationFormatter.formatAnnotation(sessionCounts, file.isDirectory());
        if (annotation != null && !annotation.isBlank()) {
            data.setLocationString(annotation);
        }

        String tooltip = ContextAnnotationFormatter.buildTooltipText(sessionCounts, file.isDirectory());
        if (tooltip != null) {
            data.setTooltip(tooltip);
        }

        Icon baseIcon = data.getIcon(false);
        if (baseIcon != null) {
            Icon badge = AnahataFileIconProvider.getBadgeIcon();
            if (badge != null) {
                LayeredIcon layered = new LayeredIcon(2);
                layered.setIcon(baseIcon, 0);
                int hShift = Math.max(0, baseIcon.getIconWidth() - badge.getIconWidth()) + 2;
                int vShift = -1;
                layered.setIcon(badge, 1, hShift, vShift);
                data.setIcon(layered);
            }
        }
    }
}
