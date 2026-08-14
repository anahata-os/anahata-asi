/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.intellij.tools.ide.context;

import com.intellij.ide.projectView.PresentationData;
import com.intellij.ide.projectView.ProjectViewNode;
import com.intellij.ide.projectView.ProjectViewNodeDecorator;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.ui.SimpleTextAttributes;
import uno.anahata.asi.intellij.internal.AgiContext;

/**
 * Badges Project-view nodes whose file is in one or more active AGI session contexts.
 * <p>
 * The IntelliJ analogue of the NetBeans annotation provider: instead of masterfs icon
 * annotation, it appends a greyed marker (and a per-session count) to the node's
 * presentation via the {@link ProjectViewNodeDecorator} extension point. Membership is
 * resolved through {@link AgiContext} against the live session registry.
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
     * Appends " ● AGI" (with a "×N" suffix when several sessions hold the file) to files
     * that are currently in context; leaves directories and out-of-context files untouched.
     * </p>
     */
    @Override
    public void decorate(ProjectViewNode<?> node, PresentationData data) {
        VirtualFile file = node.getVirtualFile();
        if (file == null || file.isDirectory()) {
            return;
        }
        int sessions = AgiContext.sessionsContaining(file);
        if (sessions > 0) {
            String marker = sessions > 1 ? " ● AGI ×" + sessions : " ● AGI";
            data.addText(marker, SimpleTextAttributes.GRAYED_ATTRIBUTES);
        }
    }
}
