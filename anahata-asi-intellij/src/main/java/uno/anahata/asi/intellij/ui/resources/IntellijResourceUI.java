/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.intellij.ui.resources;

import com.intellij.icons.AllIcons;
import com.intellij.openapi.fileEditor.OpenFileDescriptor;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JPanel;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.AbstractAsiContainer;
import uno.anahata.asi.agi.resource.Resource;
import uno.anahata.asi.ide.ui.resources.IdeResourceUI;
import uno.anahata.asi.intellij.internal.ProjectUtils;
import uno.anahata.asi.intellij.resources.handle.IntellijHandle;
import uno.anahata.asi.intellij.tools.ide.IDE;
import uno.anahata.asi.intellij.tools.ide.SelectInTarget;
import uno.anahata.asi.swing.agi.AgiPanel;

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
public class IntellijResourceUI extends IdeResourceUI {

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
     * Returns an IntelliJ-native {@link IntellijTextResourceViewer} bound to a container context
     * for textual resources, providing authentic IDE editor frames.
     * </p>
     */
    @Override
    public JComponent createContent(Resource resource, AbstractAsiContainer container) {
        if (resource.getHandle().isTextual()) {
            if (resource.getName().toLowerCase().endsWith(".log")) {
                return super.createContent(resource, container);
            }
            return new IntellijTextResourceViewer(container, resource);
        }
        return super.createContent(resource, container);
    }

    /**
     * {@inheritDoc}
     * <p>
     * Returns an {@link IntellijHandlePanel} for {@link IntellijHandle} instances,
     * providing authentic IDE connectivity and VCS metadata.
     * </p>
     */
    @Override
    public JPanel createHandlePanel(Resource resource, AgiPanel agiPanel) {
        if (resource.getHandle() instanceof IntellijHandle ih) {
            IntellijHandlePanel ihp = new IntellijHandlePanel();
            ihp.setHandle(ih);
            return ihp;
        }
        return super.createHandlePanel(resource, agiPanel);
    }

    @Override
    protected Icon getOpenInEditorIcon(AgiPanel agiPanel) {
        return AllIcons.Actions.Edit;
    }

    @Override
    protected Icon getSelectInProjectIcon(AgiPanel agiPanel) {
        return AllIcons.Nodes.Project;
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
            VirtualFile vf = ProjectUtils.findVirtualFile(path);
            if (vf != null) {
                Project project = ProjectUtils.findHostProject(vf);
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
                IDE.selectIn(path, SelectInTarget.PROJECTS);
            } catch (Exception e) {
                log.error("Failed to select resource in IDE: " + path, e);
            }
        }
    }


}
