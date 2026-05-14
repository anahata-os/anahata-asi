/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.agi.config;

import java.awt.BorderLayout;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import javax.swing.JTabbedPane;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.agi.AgiConfig;
import uno.anahata.asi.agi.provider.RequestConfig;
import uno.anahata.asi.swing.agi.AgiPanel;
import uno.anahata.asi.swing.components.ScrollablePanel;

/**
 * A tabbed aggregator panel for editing both the framework-level (AgiConfig) 
 * and model-specific (RequestConfig) settings of an AI session.
 * <p>
 * This component acts as the primary configuration hub, hosting specialized 
 * sub-panels for metabolism, loop logic, and model parameters.
 * </p>
 * 
 * @author anahata
 */
@Slf4j
public class SessionConfigPanel extends ScrollablePanel implements PropertyChangeListener {

    /** The framework-level configuration panel. */
    private final AgiConfigPanel agiConfigPanel;
    /** The model-level configuration panel. */
    private final RequestConfigPanel requestConfigPanel;

    /**
     * Constructs a new SessionConfigPanel for a live Agi session.
     * 
     * @param agiPanel The parent agi panel.
     */
    public SessionConfigPanel(AgiPanel agiPanel) {
        this(agiPanel.getAgi().getConfig(), agiPanel.getAgi().getRequestConfig(), agiPanel.getAgi());
    }

    /**
     * Internal constructor for full flexibility (used for templates and live sessions).
     * 
     * @param agiConfig The framework configuration.
     * @param requestConfig The request configuration.
     * @param agi The associated Agi session (can be null for global templates).
     */
    public SessionConfigPanel(AgiConfig agiConfig, RequestConfig requestConfig, Agi agi) {
        setScrollableTracksViewportWidth(false);
        setLayout(new BorderLayout());

        this.agiConfigPanel = new AgiConfigPanel(agiConfig);
        this.requestConfigPanel = new RequestConfigPanel(requestConfig, agi);

        JTabbedPane tabbedPane = new JTabbedPane();
        tabbedPane.addTab("Framework (DNA)", agiConfigPanel);
        tabbedPane.addTab("Model (Execution)", requestConfigPanel);

        add(tabbedPane, BorderLayout.CENTER);
        
        if (agi != null) {
            agi.addPropertyChangeListener(this);
        }
    }

    /**
     * {@inheritDoc}
     * <p>Proxies property change events to the underlying specialized panels 
     * to ensure the entire configuration stack remains synchronized.</p>
     */
    @Override
    public void propertyChange(PropertyChangeEvent evt) {
        agiConfigPanel.propertyChange(evt);
        requestConfigPanel.propertyChange(evt);
    }
}
