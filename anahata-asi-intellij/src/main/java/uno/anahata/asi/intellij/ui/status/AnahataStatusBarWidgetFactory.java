/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.intellij.ui.status;

import com.intellij.openapi.project.Project;
import com.intellij.openapi.wm.StatusBarWidget;
import com.intellij.openapi.wm.StatusBarWidgetFactory;
import org.jetbrains.annotations.NotNull;

/**
 * Factory that contributes the Anahata active-sessions status-bar widget.
 * <p>
 * Registered via the {@code statusBarWidgetFactory} extension point (see {@code plugin.xml}).
 * </p>
 *
 * @author anahata
 */
public class AnahataStatusBarWidgetFactory implements StatusBarWidgetFactory {

    /**
     * The widget id, shared with the widget and the plugin.xml registration.
     */
    public static final String ID = "AnahataAsiStatusWidget";

    /**
     * Constructs the widget factory (instantiated by the platform via its public no-arg constructor).
     */
    public AnahataStatusBarWidgetFactory() {
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public @NotNull String getId() {
        return ID;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public @NotNull String getDisplayName() {
        return "Anahata ASI";
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public @NotNull StatusBarWidget createWidget(@NotNull Project project) {
        return new AnahataStatusBarWidget(project);
    }
}
