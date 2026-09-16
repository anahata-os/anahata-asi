/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.intellij.ui;

import com.intellij.ide.FileIconProvider;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.IconLoader;
import com.intellij.openapi.vfs.VirtualFile;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.Icon;

/**
 * Provides a branded Anahata logo icon for {@code anahata.md} files in IntelliJ IDEA.
 * <p>
 * Ensures {@code anahata.md} files stand out visually in the Project view, editor tabs,
 * and navigation bars without altering or injecting synthetic tree nodes.
 * </p>
 *
 * @author anahata
 */
public class AnahataFileIconProvider implements FileIconProvider {

    /**
     * The branded Anahata icon for instructions files.
     */
    private static final Icon ICON = IconLoader.getIcon("/icons/anahataToolWindow.png", AnahataFileIconProvider.class);

    /**
     * Constructs the file icon provider (instantiated by the platform via its public no-arg constructor).
     */
    public AnahataFileIconProvider() {
    }

    /**
     * {@inheritDoc}
     * <p>
     * Returns the Anahata logo icon if the file is named {@code anahata.md}; otherwise returns {@code null}
     * to allow standard file type icon resolution.
     * </p>
     */
    @Override
    @Nullable
    public Icon getIcon(@NotNull VirtualFile file, int flags, @Nullable Project project) {
        if ("anahata.md".equalsIgnoreCase(file.getName())) {
            return ICON;
        }
        return null;
    }
}
