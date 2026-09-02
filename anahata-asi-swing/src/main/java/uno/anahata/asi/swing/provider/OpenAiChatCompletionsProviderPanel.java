/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.provider;

import java.awt.Font;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import lombok.NonNull;
import org.jdesktop.swingx.prompt.PromptSupport;
import uno.anahata.asi.agi.provider.AbstractAiProvider;
import uno.anahata.asi.openai.compatible.OpenAiChatCompletionsProvider;
import uno.anahata.asi.swing.AbstractSwingAsiContainer;
import uno.anahata.asi.swing.AiProviderPanel;
import uno.anahata.asi.swing.internal.SwingUtils;

/**
 * Specialized Swing configuration panel for OpenAI-compatible providers.
 * <p>
 * Exposes custom HTTP request headers and HTTP/1.1 protocol preference toggles.
 * </p>
 *
 * @author anahata
 */
public class OpenAiChatCompletionsProviderPanel extends AiProviderPanel {

    /**
     * Text area for multi-line custom HTTP header configuration.
     */
    private JTextArea customHeadersArea;

    /**
     * Toggle for forcing HTTP/1.1 on local inference servers and routers.
     */
    private JCheckBox preferHttp11Check;

    /**
     * Constructs a new uninitialized OpenAiChatCompletionsProviderPanel.
     */
    public OpenAiChatCompletionsProviderPanel() {
        super();
    }

    /**
     * {@inheritDoc}
     * <p>Adds custom headers multi-line editor and prefer HTTP/1.1 checkbox.</p>
     */
    @Override
    public void init(@NonNull AbstractSwingAsiContainer container, @NonNull AbstractAiProvider provider, Runnable removeCallback) {
        super.init(container, provider, removeCallback);
        if (provider instanceof OpenAiChatCompletionsProvider oai) {
            formPanel.add(new JLabel("Custom Headers:"), "top, gaptop 5");
            customHeadersArea = new JTextArea(3, 20);
            customHeadersArea.setFont(new Font(Font.MONOSPACED, Font.PLAIN, 12));
            customHeadersArea.addMouseWheelListener(e -> SwingUtils.redispatchMouseWheelEvent(customHeadersArea, e));
            if (oai.getCustomHeaders() != null) {
                String headers = oai.getCustomHeaders().entrySet().stream()
                        .map(entry -> entry.getKey() + ": " + entry.getValue())
                        .collect(Collectors.joining("\n"));
                customHeadersArea.setText(headers);
            }
            PromptSupport.setPrompt("Header-Name: Header-Value\nOne per line...", customHeadersArea);
            JScrollPane headersScroll = new JScrollPane(customHeadersArea);
            formPanel.add(headersScroll, "span 2, growx, wrap");

            formPanel.add(new JLabel("Prefer HTTP/1.1:"), "gaptop 5");
            preferHttp11Check = new JCheckBox("", oai.isPreferHttp11());
            preferHttp11Check.setOpaque(false);
            preferHttp11Check.setToolTipText("Force HTTP/1.1 to avoid protocol hangs on some local servers/routers.");
            formPanel.add(preferHttp11Check, "span 2, wrap");
        }
    }

    /**
     * {@inheritDoc}
     * <p>Checks custom headers and prefer HTTP/1.1 modifications.</p>
     */
    @Override
    public boolean isModified() {
        if (super.isModified()) {
            return true;
        }
        if (provider instanceof OpenAiChatCompletionsProvider oai) {
            if (preferHttp11Check != null && preferHttp11Check.isSelected() != oai.isPreferHttp11()) {
                return true;
            }
            if (customHeadersArea != null) {
                Map<String, String> parsed = parseHeaders();
                Map<String, String> existing = oai.getCustomHeaders() != null ? oai.getCustomHeaders() : Map.of();
                return !Objects.equals(parsed, existing);
            }
        }
        return false;
    }

    /**
     * {@inheritDoc}
     * <p>Synchronizes custom headers and HTTP/1.1 preference.</p>
     */
    @Override
    public void syncToProvider() throws IOException {
        super.syncToProvider();
        if (provider instanceof OpenAiChatCompletionsProvider oai) {
            if (preferHttp11Check != null) {
                oai.setPreferHttp11(preferHttp11Check.isSelected());
            }
            if (customHeadersArea != null) {
                oai.setCustomHeaders(parseHeaders());
            }
        }
    }

    /**
     * Parses the raw lines in {@link #customHeadersArea} into a key-value header map.
     *
     * @return The parsed header map.
     */
    private Map<String, String> parseHeaders() {
        Map<String, String> headers = new HashMap<>();
        String text = customHeadersArea.getText().trim();
        if (!text.isEmpty()) {
            for (String line : text.split("\n")) {
                int colon = line.indexOf(":");
                if (colon > 0) {
                    headers.put(line.substring(0, colon).trim(), line.substring(colon + 1).trim());
                }
            }
        }
        return headers;
    }
}
