/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.provider;

import java.io.IOException;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import lombok.NonNull;
import uno.anahata.asi.agi.provider.AbstractAiProvider;
import uno.anahata.asi.openai.OpenAiResponsesProvider;
import uno.anahata.asi.swing.AbstractSwingAsiContainer;
import uno.anahata.asi.swing.AbstractAiProviderPanel;

/**
 * Specialized Swing configuration panel for OpenAI Responses API providers.
 * <p>
 * Exposes options like the verified organization toggle for stateful sessions.
 * </p>
 *
 * @author anahata
 */
public class OpenAiResponsesProviderPanel extends AbstractAiProviderPanel<OpenAiResponsesProvider> {

    /**
     * Checkbox to toggle verified organization mode.
     */
    private JCheckBox verifiedCheck;

    /**
     * Constructs a new uninitialized OpenAiResponsesProviderPanel.
     */
    public OpenAiResponsesProviderPanel() {
        super();
    }

    /**
     * {@inheritDoc}
     * <p>Adds the verified organization checkbox control.</p>
     */
    @Override
    public void init(@NonNull AbstractSwingAsiContainer container, @NonNull OpenAiResponsesProvider provider, Runnable removeCallback) {
        super.init(container, provider, removeCallback);
        formPanel.add(new JLabel("Verified Organization:"), "gaptop 5");
        verifiedCheck = new JCheckBox("", provider.isVerifiedOrganization());
        verifiedCheck.setOpaque(false);
        verifiedCheck.setToolTipText("Enable if your API key belongs to a verified OpenAI organization. Allows stateful API calls and plain-text reasoning summaries.");
        formPanel.add(verifiedCheck, "span 2, wrap");
    }

    /**
     * {@inheritDoc}
     * <p>Checks verified organization checkbox modifications.</p>
     */
    @Override
    public boolean isModified() {
        if (super.isModified()) {
            return true;
        }
        if (verifiedCheck != null) {
            return verifiedCheck.isSelected() != provider.isVerifiedOrganization();
        }
        return false;
    }

    /**
     * {@inheritDoc}
     * <p>Synchronizes verified organization state.</p>
     */
    @Override
    public void syncToProvider() throws IOException {
        super.syncToProvider();
        if (verifiedCheck != null) {
            provider.setVerifiedOrganization(verifiedCheck.isSelected());
        }
    }
}
