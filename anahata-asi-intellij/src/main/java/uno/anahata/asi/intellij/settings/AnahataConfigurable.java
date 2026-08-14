/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.intellij.settings;

import com.intellij.openapi.options.Configurable;
import com.intellij.ui.components.JBLabel;
import com.intellij.ui.components.JBTextField;
import com.intellij.util.ui.FormBuilder;
import org.jetbrains.annotations.Nls;
import org.jetbrains.annotations.Nullable;

import javax.swing.JComponent;
import javax.swing.JPanel;

/**
 * A native Settings page (Settings → Tools → Anahata ASI) for the plugin defaults.
 * <p>
 * Edits the persisted {@link AnahataSettings} — the default provider and model applied to newly
 * created sessions. Deeper, per-session configuration remains available through the tool
 * window's Preferences action.
 * </p>
 *
 * @author anahata
 */
public class AnahataConfigurable implements Configurable {

    /**
     * Editor for the default provider UUID.
     */
    private JBTextField providerField;

    /**
     * Editor for the default model id.
     */
    private JBTextField modelField;

    /**
     * The settings panel root.
     */
    private JPanel panel;

    /**
     * Constructs the configurable (instantiated by the platform via its public no-arg constructor).
     */
    public AnahataConfigurable() {
    }

    /**
     * {@inheritDoc}
     */
    @Nls
    @Override
    public String getDisplayName() {
        return "Anahata ASI";
    }

    /**
     * {@inheritDoc}
     * <p>
     * Builds a simple two-field form for the session defaults.
     * </p>
     */
    @Nullable
    @Override
    public JComponent createComponent() {
        providerField = new JBTextField();
        modelField = new JBTextField();
        panel = FormBuilder.createFormBuilder()
                .addLabeledComponent("Default provider UUID:", providerField)
                .addLabeledComponent("Default model id:", modelField)
                .addComponent(new JBLabel("Applied to newly created Anahata sessions. Use the tool window's Preferences for per-session settings."))
                .addComponentFillVertically(new JPanel(), 0)
                .getPanel();
        reset();
        return panel;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isModified() {
        AnahataSettings settings = AnahataSettings.getInstance();
        return !providerField.getText().trim().equals(settings.defaultProviderUuid)
                || !modelField.getText().trim().equals(settings.defaultModelId);
    }

    /**
     * {@inheritDoc}
     * <p>
     * Persists the edited defaults.
     * </p>
     */
    @Override
    public void apply() {
        AnahataSettings settings = AnahataSettings.getInstance();
        settings.defaultProviderUuid = providerField.getText().trim();
        settings.defaultModelId = modelField.getText().trim();
    }

    /**
     * {@inheritDoc}
     * <p>
     * Reloads the fields from the persisted settings.
     * </p>
     */
    @Override
    public void reset() {
        AnahataSettings settings = AnahataSettings.getInstance();
        providerField.setText(settings.defaultProviderUuid);
        modelField.setText(settings.defaultModelId);
    }
}
