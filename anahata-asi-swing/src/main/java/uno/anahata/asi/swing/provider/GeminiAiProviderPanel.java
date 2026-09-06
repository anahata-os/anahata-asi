/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.provider;

import java.io.IOException;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import lombok.NonNull;
import uno.anahata.asi.agi.provider.AbstractAiProvider;
import uno.anahata.asi.gemini.GeminiAiProvider;
import uno.anahata.asi.swing.AbstractSwingAsiContainer;
import uno.anahata.asi.swing.AbstractAiProviderPanel;

/**
 * Specialized Swing configuration panel for Google Gemini AI providers.
 * <p>
 * Exposes Gemini-specific options such as toggling Google Cloud Vertex AI endpoints.
 * </p>
 *
 * @author anahata
 */
public class GeminiAiProviderPanel extends AbstractAiProviderPanel<GeminiAiProvider> {

    /**
     * Checkbox to toggle Google Cloud Vertex AI endpoint usage.
     */
    private JCheckBox vertexCheck;

    /**
     * Constructs a new uninitialized GeminiAiProviderPanel.
     */
    public GeminiAiProviderPanel() {
        super();
    }

    /**
     * {@inheritDoc}
     * <p>Adds the Gemini-specific 'Use Vertex AI' toggle control to the form panel.</p>
     */
    @Override
    public void init(@NonNull AbstractSwingAsiContainer container, @NonNull GeminiAiProvider provider, Runnable removeCallback) {
        super.init(container, provider, removeCallback);
        formPanel.add(new JLabel("Use Vertex AI:"), "gaptop 5");
        vertexCheck = new JCheckBox("", provider.isVertex());
        vertexCheck.setOpaque(false);
        vertexCheck.setToolTipText("Use Google Cloud Vertex AI endpoint instead of the standard Google AI Studio.");
        formPanel.add(vertexCheck, "span 2, wrap");
    }

    /**
     * {@inheritDoc}
     * <p>Checks both standard fields and the Vertex AI toggle state.</p>
     */
    @Override
    public boolean isModified() {
        if (super.isModified()) {
            return true;
        }
        if (vertexCheck != null) {
            return vertexCheck.isSelected() != provider.isVertex();
        }
        return false;
    }

    /**
     * {@inheritDoc}
     * <p>Synchronizes standard fields and the Vertex AI toggle state to the provider domain.</p>
     */
    @Override
    public void syncToProvider() throws IOException {
        super.syncToProvider();
        if (vertexCheck != null) {
            provider.setVertex(vertexCheck.isSelected());
        }
    }
}
