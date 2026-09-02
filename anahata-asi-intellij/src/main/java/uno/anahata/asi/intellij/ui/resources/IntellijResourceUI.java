/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.intellij.ui.resources;

import com.intellij.icons.AllIcons;
import com.intellij.openapi.fileEditor.OpenFileDescriptor;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.resource.Resource;
import uno.anahata.asi.agi.resource.handle.PathHandle;
import uno.anahata.asi.intellij.internal.JavaPsi;
import uno.anahata.asi.intellij.tools.ide.IDE;
import uno.anahata.asi.swing.agi.AgiPanel;
import uno.anahata.asi.swing.agi.resources.DefaultResourceUI;

import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JPanel;

/**
 * IntelliJ-specific implementation of {@link uno.anahata.asi.swing.agi.resources.ResourceUI}.
 * <p>
 * This strategy leverages IntelliJ native OpenAPI for high-fidelity navigation and
 * visualization within the IDE. It provides {@link IntellijTextResourceViewer} for
 * 100% IDE editor fidelity (syntax highlighting, line numbers, folding, error annotators).
 * </p>
 *
 * @author anahata
 */
@Slf4j
public class IntellijResourceUI extends DefaultResourceUI {

    /**
     * {@inheritDoc}
     * <p>
     * Returns an IntelliJ-native {@link IntellijTextResourceViewer} for textual resources,
     * providing authentic IDE editor frames.
     * </p>
     */
    @Override
    public JComponent createContent(Resource resource, AgiPanel agiPanel) {
        if (resource.getHandle().isTextual()) {
            if (resource.getName().toLowerCase().endsWith(".log")) {
                return super.createContent(resource, agiPanel);
            }
            return new IntellijTextResourceViewer(agiPanel, resource);
        }
        return super.createContent(resource, agiPanel);
    }

    /**
     * {@inheritDoc}
     * <p>
     * Injects IDE-specific 'Open in Editor' and 'Select in Project' actions for physical resources.
     * </p>
     */
    @Override
    public void populateActions(JPanel actionContainer, Resource resource, AgiPanel agiPanel) {
        if (!resource.getHandle().isVirtual()) {
            JButton openBtn = createLinkButton("Open in Editor", "Open the file in the IntelliJ code editor.", AllIcons.Actions.Edit);
            openBtn.addActionListener(e -> open(resource, agiPanel));
            actionContainer.add(openBtn);

            JButton selectBtn = createLinkButton("Select in Project", "Locate and highlight the file in the IDE Project view.", AllIcons.Nodes.Project);
            selectBtn.addActionListener(e -> select(resource, agiPanel));
            actionContainer.add(selectBtn);
        } else {
            super.populateActions(actionContainer, resource, agiPanel);
        }
    }

    /**
     * {@inheritDoc}
     * <p>
     * Opens the physical file in the IntelliJ editor via {@link OpenFileDescriptor}.
     * </p>
     */
    @Override
    public void open(Resource resource, AgiPanel agiPanel) {
        String path = getPath(resource);
        if (path != null) {
            VirtualFile vf = JavaPsi.findVirtualFile(path);
            if (vf != null) {
                Project project = JavaPsi.findHostProject(vf);
                if (project != null) {
                    new OpenFileDescriptor(project, vf).navigate(true);
                    return;
                }
            }
        }
        super.open(resource, agiPanel);
    }

    /**
     * {@inheritDoc}
     * <p>
     * Uses the ASI {@link IDE} tool to focus the resource in the Project view.
     * </p>
     */
    @Override
    public void select(Resource resource, AgiPanel agiPanel) {
        String path = getPath(resource);
        if (path != null) {
            try {
                IDE.selectIn(path);
            } catch (Exception e) {
                log.error("Failed to select resource in IDE: " + path, e);
            }
        }
    }

    /**
     * Resolves the physical path from the handle for IDE navigation.
     *
     * @param resource the resource.
     * @return the absolute path, or null if virtual.
     */
    private String getPath(Resource resource) {
        if (resource.getHandle() instanceof PathHandle ph) {
            return ph.getPath();
        }
        return null;
    }
}
