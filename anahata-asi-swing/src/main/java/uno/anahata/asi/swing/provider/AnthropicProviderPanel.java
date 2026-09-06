/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.provider;

import java.io.IOException;
import java.util.Objects;
import javax.swing.JLabel;
import javax.swing.JTextField;
import lombok.NonNull;
import uno.anahata.asi.agi.provider.AbstractAiProvider;
import uno.anahata.asi.anthropic.AnthropicProvider;
import uno.anahata.asi.swing.AbstractSwingAsiContainer;
import uno.anahata.asi.swing.AbstractAiProviderPanel;

/**
 * Specialized Swing configuration panel for Anthropic Claude providers.
 * <p>
 * Exposes Anthropic-specific options like the {@code anthropic-version} header.
 * </p>
 *
 * @author anahata
 */
public class AnthropicProviderPanel extends AbstractAiProviderPanel<AnthropicProvider> {

    /**
     * Text field for configuring the Anthropic API version header.
     */
    private JTextField anthropicVersionField;

    /**
     * Constructs a new uninitialized AnthropicProviderPanel.
     */
    public AnthropicProviderPanel() {
        super();
    }

    /**
     * {@inheritDoc}
     * <p>Adds the 'Anthropic Version' text field to the form.</p>
     */
    @Override
    public void init(@NonNull AbstractSwingAsiContainer container, @NonNull AnthropicProvider provider, Runnable removeCallback) {
        super.init(container, provider, removeCallback);
        formPanel.add(new JLabel("Anthropic Version:"), "gaptop 5");
        anthropicVersionField = new JTextField(provider.getAnthropicVersion() != null ? provider.getAnthropicVersion() : "");
        formPanel.add(anthropicVersionField, "span 2, wrap");
    }

    /**
     * {@inheritDoc}
     * <p>Checks both standard fields and the Anthropic version text field.</p>
     */
    @Override
    public boolean isModified() {
        if (super.isModified()) {
            return true;
        }
        if (anthropicVersionField != null) {
            String current = provider.getAnthropicVersion() != null ? provider.getAnthropicVersion() : "";
            return !Objects.equals(anthropicVersionField.getText().trim(), current);
        }
        return false;
    }

    /**
     * {@inheritDoc}
     * <p>Synchronizes standard fields and the Anthropic version header.</p>
     */
    @Override
    public void syncToProvider() throws IOException {
        super.syncToProvider();
        if (anthropicVersionField != null) {
            provider.setAnthropicVersion(anthropicVersionField.getText().trim());
        }
    }
}
