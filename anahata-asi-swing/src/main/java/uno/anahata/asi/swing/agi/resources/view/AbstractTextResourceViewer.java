/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.agi.resources.view;

import java.awt.BorderLayout;
import java.awt.CardLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Insets;
import java.awt.Rectangle;
import java.awt.event.MouseWheelListener;
import java.io.IOException;
import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSpinner;
import javax.swing.JTextField;
import javax.swing.JToolBar;
import javax.swing.SpinnerNumberModel;
import javax.swing.SwingUtilities;
import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.resource.v2.Resource;
import uno.anahata.asi.resource.v2.view.TextView;
import uno.anahata.asi.resource.v2.view.TextViewportSettings;
import uno.anahata.asi.swing.agi.AgiPanel;
import uno.anahata.asi.swing.icons.RestartIcon;
import uno.anahata.asi.swing.internal.AnyChangeDocumentListener;
import uno.anahata.asi.swing.internal.EdtPropertyChangeListener;
import uno.anahata.asi.swing.internal.SwingTask;
import uno.anahata.asi.swing.internal.SwingUtils;

/**
 * Common base for high-fidelity text resource viewers with autonomous Edit/Save UI.
 * <p>
 * This class provides the integrated control strip containing both the 
 * Viewport Toolbar (Tail, Grep, Lines) and the Edit/Save toggle. 
 * </p>
 * <p>
 * Persistence is decoupled via the {@link SaveAction} interface, allowing 
 * this component to be reused for managed resources (saving to disk), 
 * tool parameters (updating DTOs), or chat snippets.
 * </p>
 * 
 * @author anahata
 */
@Slf4j
public abstract class AbstractTextResourceViewer extends JPanel {

    /** Contract for handling the persistent write of the editor content. */
    @FunctionalInterface
    public interface SaveAction {
        /** Called when the user clicks 'Save'. 
         * @param content The new text content to persist.
         */
        void save(String content);
    }

    /** The parent AgiPanel providing the session context. */
    protected final AgiPanel agiPanel;
    /** The resource orchestrator being viewed. */
    protected final Resource resource;
    /** The semantic text view interpreter. Might be null for virtual snippets. */
    protected TextView textView;

    /** The layout manager for swapping between preview and editor views. */
    protected final CardLayout cardLayout = new CardLayout();
    /** The container panel for the card layout. */
    protected final JPanel cardPanel = new JPanel(cardLayout);

    /** The integrated toolbar for viewport and edit actions. */
    protected JToolBar controlStrip;
    /** The panel hosting the viewport-specific controls (Tail, Grep, etc.). */
    protected JPanel viewportControls;
    /** The checkbox to enable/disable tailing mode. */
    protected JCheckBox tailCheck;
    /** The spinner for the number of tail lines. */
    protected JSpinner tailLinesSpinner;
    /** The text field for grep filtering patterns. */
    protected JTextField grepField;
    /** The checkbox to toggle line numbers in the preview. */
    protected JCheckBox lineNumbersCheck;

    /** The panel hosting the edit and save buttons. */
    protected JPanel actionNexus;
    /** The button to toggle between View and Edit modes. */
    protected JButton editBtn;
    /** Flag indicating the current UI mode (view vs edit). */
    @Getter
    protected boolean editing = false;

    /** 
     * Flag used to disable listeners during UI synchronization to prevent 
     * recursive feedback loops.
     */
    private boolean syncing = false;
    
    /** 
     * Flag indicating if vertical scrolling is enabled. 
     * Defaults to true (Resource Panel). Should be set to false for Chat Snippets.
     */
    @Getter
    protected boolean verticalScrollEnabled = true;

    /**
     * If true, the 'Preview' mode uses the high-fidelity editor component (read-only)
     * instead of the viewport. This ensures snippets don't lose text when saving.
     * <p><b>Clean-Room Default:</b> snippets (virtual resources) default to true.</p>
     */
    @Getter @Setter
    protected boolean previewAsEditor = false;

    /** The delegate responsible for persistence. */
    @Setter
    protected SaveAction saveAction;

    /** The reactive listener for resource state changes. */
    private EdtPropertyChangeListener resourceListener;

    /**
     * Constructs a new AbstractTextResourceViewer.
     * 
     * @param agiPanel The parent AgiPanel.
     * @param resource The text resource.
     */
    protected AbstractTextResourceViewer(AgiPanel agiPanel, Resource resource) {
        this.agiPanel = agiPanel;
        this.resource = resource;
        // The view might be null if reloadIfNeeded() hasn't been called yet.
        this.textView = (resource.getView() instanceof TextView tv) ? tv : null;
        
        // Authoritative Default: Virtual snippets use 'Preview-as-Editor' mode
        this.previewAsEditor = resource.getHandle().isVirtual();
        
        setLayout(new BorderLayout());
        initComponents();
        
        this.resourceListener = new EdtPropertyChangeListener(this, resource, null, evt -> syncWithResource());
    }

    /**
     * Returns the managed resource orchestrator.
     * 
     * @return The resource instance.
     */
    public Resource getResource() {
        return resource;
    }

    /**
     * Initializes the UI components and assemblies the integrated control strip.
     */
    private void initComponents() {
        // 1. Integrated Control Strip
        controlStrip = new JToolBar();
        controlStrip.setFloatable(false);
        controlStrip.setBorder(BorderFactory.createMatteBorder(0, 0, 1, 0, Color.LIGHT_GRAY));

        // 1a. Viewport Controls
        viewportControls = new JPanel(new FlowLayout(FlowLayout.LEFT, 5, 0));
        viewportControls.setOpaque(false);

        tailCheck = new JCheckBox("Tail");
        tailCheck.addItemListener(e -> updateViewportSettings());
        viewportControls.add(tailCheck);

        viewportControls.add(new JLabel("Lines:"));
        tailLinesSpinner = new JSpinner(new SpinnerNumberModel(100, 1, 10000, 50));
        tailLinesSpinner.setMaximumSize(new Dimension(60, 22));
        tailLinesSpinner.addChangeListener(e -> updateViewportSettings());
        viewportControls.add(tailLinesSpinner);

        viewportControls.add(new JLabel("Grep:"));
        grepField = new JTextField();
        grepField.setPreferredSize(new Dimension(150, 22));
        grepField.getDocument().addDocumentListener(new AnyChangeDocumentListener(this::updateViewportSettings));
        viewportControls.add(grepField);

        lineNumbersCheck = new JCheckBox("Lines");
        lineNumbersCheck.addItemListener(e -> updateViewportSettings());
        viewportControls.add(lineNumbersCheck);

        controlStrip.add(viewportControls);
        controlStrip.addSeparator();

        // 1b. Action Nexus (Edit/Save)
        actionNexus = new JPanel(new FlowLayout(FlowLayout.RIGHT, 5, 0));
        actionNexus.setOpaque(false);
        
        editBtn = new JButton("📝 EDIT");
        editBtn.addActionListener(e -> toggleEditMode());
        actionNexus.add(editBtn);
        
        controlStrip.add(actionNexus);

        add(controlStrip, BorderLayout.NORTH);

        // 2. Content Area
        cardPanel.add(createPreviewComponent(), "preview");
        cardPanel.add(createEditorComponent(), "editor");
        add(cardPanel, BorderLayout.CENTER);
        
        syncWithResource();
    }

    /**
     * Toggles between read-only viewport view and high-fidelity editor.
     */
    private void toggleEditMode() {
        if (editing) {
            // PERFORM SAVE
            String newContent = getEditorContent();
            log.info("toggleEditMode Saving new content to {} ", saveAction);
            if (newContent != null && saveAction != null) {                
                saveAction.save(newContent);
            } else {
                setEditing(false);
            }
        } else {
            setEditing(true);
        }
    }

    /**
     * Updates the UI to reflect the current editing state. 
     * <p>
     * For snippets, this method ensures we stay on the high-fidelity card 
     * but toggle component editability.
     * </p>
     * 
     * @param editing true to enable editing.
     */
    public void setEditing(boolean editing) {
        this.editing = editing;
        viewportControls.setVisible(!editing && !previewAsEditor);
        
        if (editing) {
            editBtn.setText("💾 SAVE");
            editBtn.setIcon(new RestartIcon(16));
            cardLayout.show(cardPanel, "editor");
            setComponentEditable(true);
            onEditorActivated();
        } else {
            editBtn.setText("📝 EDIT");
            editBtn.setIcon(null);
            
            if (previewAsEditor) {
                cardLayout.show(cardPanel, "editor");
                setComponentEditable(false);
            } else {
                cardLayout.show(cardPanel, "preview");
            }
            onPreviewActivated();
        }
    }

    /**
     * Controls the visibility of the entire integrated control strip.
     * @param visible true to show the toolbar.
     */
    public void setToolbarVisible(boolean visible) {
        controlStrip.setVisible(visible);
    }
    
    /**
     * Configures whether vertical scrolling is enabled in the internal scroll pane.
     * <p>
     * <b>High-Fidelity Scroll Passthrough:</b> If vertical scrolling is disabled 
     * (e.g., in chat snippets), this method recursively finds the innermost 
     * text component and installs a boundary-aware mouse wheel redispatcher.
     * </p>
     * 
     * @param enabled true to enable standard vertical scrolling.
     */
    public void setVerticalScrollEnabled(boolean enabled) {
        this.verticalScrollEnabled = enabled;
        configureScrollBehavior();
    }

    /**
     * Discovers the internal JScrollPane and applies the current scroll behavior settings.
     * <p>
     * Implementation details: 
     * Finds the JScrollPane inside the assembled host frame and installs the 
     * boundary-aware redispatcher on the "Event Leaf" component.
     * </p>
     */
    protected void configureScrollBehavior() {
        JScrollPane scroll = SwingUtils.findComponent(this, JScrollPane.class);
        if (scroll != null) {
            scroll.setVerticalScrollBarPolicy(verticalScrollEnabled ? JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED : JScrollPane.VERTICAL_SCROLLBAR_NEVER);
            scroll.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
            
            if (!verticalScrollEnabled && scroll.getViewport().getView() != null) {
                // SIZING SINGULARITY: Force the scroll pane to report its content height as 
                // its preferred height. This allows containing panels (like ToolCallPanel) 
                // to "breathe" to the exact size of the text area.
                Dimension viewPref = scroll.getViewport().getView().getPreferredSize();
                scroll.setPreferredSize(new Dimension(scroll.getPreferredSize().width, viewPref.height));
            }
            
            // AUTHORITATIVE DISCOVERY: Install redispatcher on the innermost component (the leaf)
            // to ensure vertical wheel events pass through to the conversation.
            Component leaf = SwingUtils.findComponentLeaf(scroll);
            if (leaf != null) {
                // Purity Fix: Remove existing wheel forwarders to prevent double-dispatching during re-syncs
                for (MouseWheelListener mwl : leaf.getMouseWheelListeners()) {
                    leaf.removeMouseWheelListener(mwl);
                }
                
                leaf.addMouseWheelListener(e -> {
                    SwingUtils.redispatchMouseWheelEvent(leaf, e);
                    e.consume();
                });
            }
        }
    }

    /**
     * Creates the read-only preview component for displaying processed resource content.
     * @return The preview component.
     */
    protected abstract JComponent createPreviewComponent();

    /**
     * Creates the editable high-fidelity component for full-file modifications.
     * @return The editor component.
     */
    protected abstract JComponent createEditorComponent();

    /**
     * Authoritatively toggles the editability of the internal text component.
     * @param editable true to allow input.
     */
    protected abstract void setComponentEditable(boolean editable);

    /** 
     * Hook invoked when switching to the editor card.
     */
    protected abstract void onEditorActivated();
    
    /** 
     * Hook invoked when returning to the preview card.
     */
    protected abstract void onPreviewActivated();

    /**
     * Retrieves the current text content from the editor component for persistence.
     * @return The content string.
     */
    public abstract String getEditorContent();

    /**
     * Pushes incremental or full content updates to the preview component. 
     * Used for streaming fidelity during generation.
     * @param content The new preview content.
     */
    protected abstract void updatePreviewContent(String content);

    /** 
     * Synchronizes UI controls with current resource state. 
     * <p>
     * Ensures immediate visibility for snippets by authoritatively pulling from 
     * the raw handle content, avoiding duplicate line numbering from the viewport engine.
     * </p>
     */
    protected void syncWithResource() {
        if (syncing) {
            return;
        }
        
        this.syncing = true;
        try {
            // Late binding: re-check the view if it was null initially
            if (textView == null && resource.getView() instanceof TextView tv) {
                this.textView = tv;
            }

            // AUTHORITATIVE RESOLUTION: If it's a virtual snippet (previewAsEditor), 
            // always use raw content to avoid duplicate line numbering.
            try {
                String content = (previewAsEditor) 
                        ? resource.asText() 
                        : (textView != null && textView.getViewport().getVisibleContent() != null) ? textView.getViewport().getVisibleContent() : resource.asText();
                updatePreviewContent(content);
            } catch (IOException e) {
                log.error("Failed to synchronize content from resource: {}", resource.getName(), e);
            }

            // HEIGHT SINGULARITY: Re-trigger scroll configuration whenever content changes in 
            // passthrough mode. We wrap in invokeLater to ensure the text component has 
            // updated its own preferred size calculation after the text was set.
            if (!verticalScrollEnabled) {
                SwingUtilities.invokeLater(this::configureScrollBehavior);
            }

            if (textView != null) {
                TextViewportSettings settings = textView.getViewport().getSettings();
                tailCheck.setSelected(settings.isTail());
                tailLinesSpinner.setValue(settings.getTailLines());
                grepField.setText(settings.getGrepPattern());
                lineNumbersCheck.setSelected(settings.isIncludeLineNumbers());
            }
        } finally {
            this.syncing = false;
        }
    }

    /**
     * Updates the underlying viewport settings based on the UI state 
     * and triggers a background reload.
     */
    private void updateViewportSettings() {
        if (syncing || textView == null) {
            return;
        }
        
        TextViewportSettings settings = textView.getViewport().getSettings();
        settings.setTail(tailCheck.isSelected());
        settings.setTailLines((Integer) tailLinesSpinner.getValue());
        settings.setGrepPattern(grepField.getText());
        settings.setIncludeLineNumbers(lineNumbersCheck.isSelected());
        
        textView.markDirty();
        
        new SwingTask<>(this, "Update Viewport", () -> {
            resource.reloadIfNeeded();
            return null;
        }).execute();
    }
}
