/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.intellij.ui.resources;

import com.intellij.openapi.project.Project;
import com.intellij.openapi.vcs.FileStatus;
import com.intellij.openapi.vcs.FileStatusManager;
import com.intellij.openapi.vfs.VirtualFile;
import java.awt.Color;
import javax.swing.JLabel;
import javax.swing.JTextField;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.ide.ui.resources.IdeHandlePanel;
import uno.anahata.asi.intellij.internal.ProjectUtils;
import uno.anahata.asi.intellij.resources.handle.IntellijHandle;

/**
 * Specialized metadata panel for the {@link IntellijHandle}.
 * <p>
 * Displays IntelliJ VFS and VCS connectivity attributes, such as absolute
 * filesystem path, VFS validity, and Version Control status.
 * </p>
 *
 * @author anahata
 */
@Slf4j
public class IntellijHandlePanel extends IdeHandlePanel<IntellijHandle> {

    /**
     * Label displaying the Version Control System (VCS) status text and color.
     */
    private final JLabel vcsStatusLabel = new JLabel();

    /**
     * Constructs a new IntelliJ handle metadata panel and initializes property fields.
     */
    public IntellijHandlePanel() {
        super();
        addProperty("VCS Status:", vcsStatusLabel);
    }

    /**
     * {@inheritDoc}
     * <p>
     * Populates IntelliJ-specific metadata such as VFS validity and VCS status.
     * </p>
     */
    @Override
    public void refresh() {
        super.refresh();
        pathField.setText(handle.getPath() != null ? handle.getPath() : "N/A");

        VirtualFile vf = handle.getVirtualFile();
        if (vf != null && vf.isValid()) {
            validityLabel.setText("VALID");
            validityLabel.setForeground(new Color(0, 150, 0));
        } else {
            validityLabel.setText("OFFLINE (Unresolved)");
            validityLabel.setForeground(Color.RED);
        }

        Project project = ProjectUtils.findHostProject(vf);
        if (project != null && vf != null) {
            FileStatus status = FileStatusManager.getInstance(project).getStatus(vf);
            vcsStatusLabel.setText(status.getText());
            Color color = status.getColor();
            if (color != null) {
                vcsStatusLabel.setForeground(color);
            }
        } else {
            vcsStatusLabel.setText("N/A");
            vcsStatusLabel.setEnabled(false);
        }
    }
}
