/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.agi;

import uno.anahata.asi.swing.agi.support.SupportPanel;
import uno.anahata.asi.swing.agi.context.gc.CwGcPanel;
import uno.anahata.asi.swing.agi.status.StatusPanel;
import uno.anahata.asi.swing.agi.chat.ConversationPanel;
import uno.anahata.asi.swing.agi.input.InputPanel;
import uno.anahata.asi.swing.agi.context.ContextPanel;
import uno.anahata.asi.swing.agi.config.SessionConfigPanel;
import java.awt.BorderLayout;
import java.awt.Component;
import javax.swing.BorderFactory;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTabbedPane;
import lombok.Getter;
import lombok.NonNull;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.swing.agi.chat.CandidateSelectionPanel;
import uno.anahata.asi.swing.components.ScrollablePanel;

/**
 * The main, top-level panel for the Anahata AI Swing UI.
 * This class acts as an aggregator for all other UI components (e.g., input panel, conversation view, status bar).
 * <p>
 * This component is designed to be thread-aware. While the UI structure is initialized on the 
 * Event Dispatch Thread (EDT), interactions with the underlying {@link Agi} orchestrator 
 * often trigger background tasks. Use {@link uno.anahata.asi.swing.internal.SwingUtils} 
 * to safely transition between threads.
 * </p>
 *
 * @author anahata
 */
@Getter
public class AgiPanel extends ScrollablePanel {

    /** The agi session orchestrator that manages model interactions and tool execution. */
    private Agi agi; 
    /** The UI-specific configuration for this agi session, including themes and providers. */
    private SwingAgiConfig agiConfig; 
    /** The tabbed pane for switching between chat, configuration, and metabolic metrics. */
    private final JTabbedPane tabbedPane;
    /** The panel for editing request configuration such as temperature and thinking levels. */
    private final SessionConfigPanel configPanel;
    /** The panel for managing the AI context, including history pruning and resource browsing. */
    private final ContextPanel contextPanel;
    /** The panel providing support links, documentation, and community resources. */
    private final SupportPanel supportPanel;
    /** The panel for monitoring Context Window Garbage Collection (CwGC) metrics. */
    private final CwGcPanel cwGcPanel;
    /** The primary user input panel, supporting multimodal attachments and command completion. */
    private final InputPanel inputPanel;
    /** The header panel for session identity and model/provider selection. */
    private final HeaderPanel headerPanel;
    /** The vertical toolbar panel for primary session-level action toggles. */
    private final ToolbarPanel toolbarPanel;
    /** The status panel for real-time monitoring of API calls and token usage. */
    private final StatusPanel statusPanel; 
    /** The main scrollable conversation view where message panels are rendered. */
    private final ConversationPanel conversationPanel; 
    /** The panel for displaying and selecting between multiple response candidates. */
    private final CandidateSelectionPanel candidateSelectionPanel;

    /**
     * Constructs a new AgiPanel by creating a new Agi session with the provided configuration.
     *
     * @param config The agi configuration.
     */
    public AgiPanel(@NonNull SwingAgiConfig config) { 
        this(new Agi(config));
    }

    /**
     * Constructs a new AgiPanel for an existing Agi session.
     * 
     * @param agi The existing agi session.
     */
    public AgiPanel(@NonNull Agi agi) {
        this.agi = agi;
        this.agiConfig = (SwingAgiConfig) agi.getConfig();
        this.tabbedPane = new JTabbedPane();
        this.configPanel = new SessionConfigPanel(this);
        this.contextPanel = new ContextPanel(this);
        this.supportPanel = new SupportPanel();
        this.cwGcPanel = new CwGcPanel(this);
        this.inputPanel = new InputPanel(this); 
        this.headerPanel = new HeaderPanel(this);
        this.toolbarPanel = new ToolbarPanel(this); 
        this.statusPanel = new StatusPanel(this); 
        this.conversationPanel = new ConversationPanel(this); 
        this.candidateSelectionPanel = new CandidateSelectionPanel(this);
        
        // GLOBAL TRANSFER HANDLER: Install on the root panel so files can be dropped 
        // anywhere in the Agi area to be attached.
        setTransferHandler(new AgiTransferHandler(this));
    }

    /**
     * Initializes the components and layout of the panel.
     */
    public void initComponents() {
        setLayout(new BorderLayout(10, 10));

        // Initialize child components first
        contextPanel.initComponents();
        headerPanel.initComponents();
        toolbarPanel.initComponents();

        // Configure Tabbed Pane
        tabbedPane.addTab("Chat", conversationPanel);
        tabbedPane.addTab("Config", createScrollPane(configPanel));
        tabbedPane.addTab("Context", contextPanel);
        tabbedPane.addTab("CwGC", createScrollPane(cwGcPanel));
        tabbedPane.addTab("Support", createScrollPane(supportPanel));
        
        // TAB SELECTION LISTENER: Just-in-time refresh for metabolism metrics
        tabbedPane.addChangeListener(e -> {
            if (cwGcPanel.isShowing()) {
                cwGcPanel.refresh();
            }
        });

        // Create a panel to hold CandidateSelectionPanel, InputPanel and StatusPanel
        JPanel southPanel = new JPanel(new BorderLayout());
        // CandidateSelectionPanel sits between conversation and input.
        southPanel.add(candidateSelectionPanel, BorderLayout.NORTH);
        // Use CENTER for inputPanel so it grows vertically when the split pane is resized.
        southPanel.add(inputPanel, BorderLayout.CENTER); 
        southPanel.add(statusPanel, BorderLayout.SOUTH); 

        // Use a SplitPane for the main content and the input area
        JSplitPane mainSplitPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT, tabbedPane, southPanel);
        mainSplitPane.setResizeWeight(1.0); // Give all extra space to the tabbed pane
        mainSplitPane.setDividerLocation(0.7); // Initial balance
        mainSplitPane.setOneTouchExpandable(true);

        // Add components to the main panel
        add(headerPanel, BorderLayout.NORTH);
        add(toolbarPanel, BorderLayout.WEST);
        add(mainSplitPane, BorderLayout.CENTER);
    }
    
    /**
     * Creates a standardized JScrollPane for wrapping inner panels.
     * @param component The component to wrap.
     * @return A styled JScrollPane.
     */
    private JScrollPane createScrollPane(Component component) {
        JScrollPane scroller = new JScrollPane(component);
        scroller.setBorder(BorderFactory.createEmptyBorder());
        scroller.setViewportBorder(BorderFactory.createEmptyBorder());
        scroller.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        scroller.getVerticalScrollBar().setUnitIncrement(16);
        return scroller;
    }

}
