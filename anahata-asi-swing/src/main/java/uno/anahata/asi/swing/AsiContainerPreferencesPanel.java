/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.util.List;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.AgiConfig;
import uno.anahata.asi.agi.provider.AbstractAgiProvider;

/**
 * A centralized preferences panel for the ASI container.
 * It provides a tabbed interface to manage settings for all registered
 * AI providers, including API keys and specific configuration options.
 * 
 * @author anahata
 */
@Slf4j
public class AsiContainerPreferencesPanel extends JPanel {

    /**
     * Constructs a new preferences panel.
     * 
     * @param container The ASI container instance.
     */
    public AsiContainerPreferencesPanel(AbstractSwingAsiContainer container) {
        super(new BorderLayout());
        setPreferredSize(new Dimension(600, 450));

        JTabbedPane tabbedPane = new JTabbedPane();

        // Use the global AgiConfig template to discover active providers
        AgiConfig template = container.getPreferences().createAgiConfig(container);
        List<Class<? extends AbstractAgiProvider>> providerClasses = template.getProviderClasses();

        if (providerClasses.isEmpty()) {
             log.warn("No AI providers found in the global template.");
             // Fallback: If template is empty, try to instantiate the default Gemini provider 
             // to ensure the user has a way to enter their first key.
             try {
                 Class<? extends AbstractAgiProvider> geminiClass = (Class<? extends AbstractAgiProvider>) 
                         Class.forName("uno.anahata.asi.gemini.GeminiAgiProvider");
                 addProviderTab(tabbedPane, geminiClass);
             } catch (ClassNotFoundException e) {
                 log.debug("Gemini provider not available on classpath.");
             }
        } else {
            for (Class<? extends AbstractAgiProvider> providerClass : providerClasses) {
                addProviderTab(tabbedPane, providerClass);
            }
        }

        add(tabbedPane, BorderLayout.CENTER);
    }

    /**
     * Instantiates a provider and adds its dedicated keys panel to the tabbed pane.
     * 
     * @param tabbedPane The tabbed pane to add to.
     * @param providerClass The class of the provider.
     */
    private void addProviderTab(JTabbedPane tabbedPane, Class<? extends AbstractAgiProvider> providerClass) {
        try {
            AbstractAgiProvider provider = providerClass.getDeclaredConstructor().newInstance();
            ProviderKeysPanel keysPanel = new ProviderKeysPanel(provider);
            tabbedPane.addTab(provider.getProviderId(), keysPanel);
        } catch (Exception e) {
            log.error("Failed to instantiate provider class for preferences: {}", providerClass, e);
        }
    }
}
