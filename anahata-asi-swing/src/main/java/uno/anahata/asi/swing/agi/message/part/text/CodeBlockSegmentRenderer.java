/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 *
 */
package uno.anahata.asi.swing.agi.message.part.text;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.FlowLayout;
import java.awt.Font;
import java.util.Objects;
import java.util.function.Consumer;
import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.resource.Resource;
import uno.anahata.asi.agi.resource.handle.StringHandle;
import uno.anahata.asi.swing.agi.AgiPanel;
import uno.anahata.asi.swing.agi.resources.view.AbstractTextResourceViewer;
import uno.anahata.asi.swing.agi.resources.ResourceUI;
import uno.anahata.asi.swing.agi.resources.ResourceUiRegistry;
import uno.anahata.asi.swing.icons.CancelIcon;
import uno.anahata.asi.swing.icons.RestartIcon;
import uno.anahata.asi.swing.icons.CopyIcon;
import uno.anahata.asi.swing.internal.SwingUtils;

/**
 * The authoritative renderer for all code block segments in the platform.
 * <p>
 * This class implements the <b>Universal Eye</b> strategy: it wraps code 
 * snippets or parameters in a virtual {@link Resource} and requests a 
 * host-native viewer from the {@link ResourceUiRegistry}. 
 * </p>
 * <p>
 * This eliminates environment-specific subclasses and ensures 100% fidelity 
 * (NetBeans frames in the IDE, RSyntax in standalone) for every line of code.
 * </p>
 *
 * @author anahata
 */
@Slf4j
public class CodeBlockSegmentRenderer extends AbstractTextSegmentRenderer {

    /** The programming language of the code block. */
    @Getter
    protected final String language;
    
    /** The host-specific high-fidelity viewer component. */
    @Getter
    protected AbstractTextResourceViewer viewer;

    /** Whether the code block is currently in edit mode. */
    @Getter
    protected boolean editing = false;

    /** Whether this code block is allowed to be edited. Defaults to false. */
    @Getter @Setter
    protected boolean editable = false;

    /** Whether the header panel (Language, Copy, Edit) is visible. Defaults to true. */
    @Getter @Setter
    protected boolean headerVisible = true;

    /** 
     * Whether vertical scrolling is enabled in the scroll pane. 
     * Defaults to false for conversation view.
     */
    @Getter @Setter
    protected boolean verticalScrollEnabled = false;
    
    /** The button used to toggle between edit and view modes. */
    protected JButton editButton;
    /** The button used to cancel editing. */
    protected JButton cancelButton;
    
    /** Callback triggered when the user clicks 'Save' after editing. */
    @Setter
    protected Consumer<String> onSave;

    /**
     * Constructs a new code block renderer.
     *
     * @param agiPanel The agi panel instance.
     * @param initialContent The initial code content.
     * @param language The programming language.
     */
    public CodeBlockSegmentRenderer(AgiPanel agiPanel, String initialContent, String language) {
        super(agiPanel, initialContent);
        this.language = language;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Renders the code block, optionally including a header with language metadata 
     * and copy/edit actions. It leverages the host's {@link ResourceUI} strategy 
     * to provide the actual editor component.
     * </p>
     */
    @Override
    public boolean render() {
        boolean changed = hasContentChanged();

        if (component == null) {
            // 1. Initialize the viewer (The Sense)
            initViewer();

            JPanel container = new JPanel(new BorderLayout());
            container.setOpaque(false);
            container.setBorder(BorderFactory.createLineBorder(new Color(200, 200, 200), 1, true));
            
            // 2. Header Panel
            if (headerVisible) {
                JPanel headerPanel = new JPanel(new BorderLayout());
                headerPanel.setOpaque(true);
                headerPanel.setBackground(new Color(240, 240, 240, 180)); 
                headerPanel.setBorder(BorderFactory.createMatteBorder(0, 0, 1, 0, new Color(210, 210, 210)));
                
                JPanel leftHeaderPanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 5, 2));
                leftHeaderPanel.setOpaque(false);

                JLabel langLabel = new JLabel((language != null ? language.toUpperCase() : "CODE"));
                langLabel.setFont(new Font("SansSerif", Font.BOLD, 10));
                langLabel.setForeground(new Color(120, 120, 120));
                leftHeaderPanel.add(langLabel);

                JButton copyButton = new JButton("Copy", new CopyIcon(12));
                copyButton.setToolTipText("Copy Code to Clipboard");
                copyButton.setFont(new Font("SansSerif", Font.PLAIN, 11));
                copyButton.setMargin(new java.awt.Insets(1, 5, 1, 5));
                copyButton.setFocusPainted(false);
                copyButton.addActionListener(e -> {
                    SwingUtils.copyToClipboard(getCurrentContentFromComponent());
                });
                leftHeaderPanel.add(copyButton);
                
                // Allow subclasses (like Mermaid) to inject extra buttons
                addExtraHeaderButtons(leftHeaderPanel);
                
                if (editable) {
                    cancelButton = new JButton("Cancel", new CancelIcon(12));
                    cancelButton.setFont(new Font("SansSerif", Font.PLAIN, 11));
                    cancelButton.setMargin(new java.awt.Insets(1, 5, 1, 5));
                    cancelButton.setFocusPainted(false);
                    cancelButton.setVisible(false);
                    cancelButton.addActionListener(e -> {
                        editing = false;
                        setComponentEditable(false);
                        editButton.setText("Edit");
                        editButton.setIcon(null);
                        cancelButton.setVisible(false);
                    });
                    leftHeaderPanel.add(cancelButton);

                    editButton = new JButton("Edit");
                    editButton.setToolTipText("Toggle Edit Mode");
                    editButton.setFont(new Font("SansSerif", Font.PLAIN, 11));
                    editButton.setMargin(new java.awt.Insets(1, 5, 1, 5));
                    editButton.setFocusPainted(false);
                    editButton.addActionListener(e -> {
                        toggleEdit();
                    });
                    leftHeaderPanel.add(editButton);
                }
                
                headerPanel.add(leftHeaderPanel, BorderLayout.WEST);
                container.add(headerPanel, BorderLayout.NORTH);
            }
            
            // 3. Central Component (The Body)
            container.add(createInnerComponent(), BorderLayout.CENTER);

            this.component = container;
            changed = true; 
        }

        if (changed) {
            updateComponentContent(currentContent);
            contentRendered(); 
        }
        return changed;
    }

    /**
     * Initializes the host-native high-fidelity viewer via the registry.
     * <p>
     * It configures the viewer for <b>Streaming Fidelity</b>: using the editor 
     * card for both modes (previewAsEditor) to prevent text loss during savings 
     * and starting in a read-only state.
     * </p>
     */
    protected void initViewer() {
        // THE UNIVERSAL WAY: Wrap in virtual resource and ask Registry.
        // CERO HARDCODING: Mimes are resolved by the viewer based on name.
        StringHandle handle = new StringHandle("snippet." + language, currentContent);
        Resource ephemeral = new Resource(handle);
        
        try {
            // CRITICAL: Perform authoritative reload before viewer creation to bind the View
            ephemeral.reloadIfNeeded();
        } catch (Exception ex) {
            log.error("Failed to initialize ephemeral resource: {}", language, ex);
        }
        
        ResourceUI strategy = ResourceUiRegistry.getInstance().getResourceUI();
        if (strategy != null) {
            JComponent content = strategy.createContent(ephemeral, agiPanel);
            if (content instanceof AbstractTextResourceViewer atv) {
                this.viewer = atv;
                viewer.setToolbarVisible(false); // Keep chat clean
                viewer.setVerticalScrollEnabled(verticalScrollEnabled);
                viewer.setPreviewAsEditor(true); // Snippet fidelity: use editor area for both modes
                viewer.setEditing(false); // Start in read-only preview mode
                
                // Wire the persistence loop (for toolbar save button)
                viewer.setSaveAction(contentStr -> {
                    toggleEdit(); 
                });
            }
        }
    }

    /**
     * Template method for subclasses to define the central component.
     * Defaults to the high-fidelity viewer.
     * 
     * @return The central JComponent.
     */
    protected JComponent createInnerComponent() {
        if (viewer != null) {
            return viewer;
        }
        return new JLabel("No ResourceUI strategy registered.");
    }

    /**
     * Hook for subclasses to inject specialized buttons into the header area.
     * 
     * @param leftHeaderPanel The panel containing the header buttons.
     */
    protected void addExtraHeaderButtons(JPanel leftHeaderPanel) {
        // Default implementation does nothing.
    }

    /**
     * Accessor for the scroll pane within the assembled frame.
     * Used by dialogs to enable navigation.
     * 
     * @return The internal JScrollPane, or null if not found.
     */
    public JScrollPane getScrollPane() {
        return SwingUtils.findComponent(component, JScrollPane.class);
    }

    /**
     * {@inheritDoc}
     * <p>
     * Authoritative streaming ingestion point. It updates the virtual handle 
     * of the ephemeral resource and triggers a reload, forcing the high-fidelity 
     * viewer (NetBeans or RSyntax) to refresh reactively.
     * </p>
     */
    @Override
    protected void updateComponentContent(String content) {
        if (viewer != null && viewer.getResource().getHandle() instanceof StringHandle sh) {
            try {
                sh.write(content);
                viewer.getResource().reloadIfNeeded();
            } catch (Exception e) {
                log.error("Failed to update streaming content for snippet: {}", language, e);
            }
        }
    }
    
    /**
     * Retrieves the current content from the inner component.
     * 
     * @return The current code content.
     */
    protected String getCurrentContentFromComponent() {
        return (viewer != null) ? viewer.getEditorContent() : currentContent;
    }
    
    /**
     * Sets the editability of the inner component.
     * 
     * @param editable True to enable editing, false to disable.
     */
    protected void setComponentEditable(boolean editable) {
        if (viewer != null) {
            viewer.setEditing(editable);
        }
    }

    /**
     * Toggles the edit mode of the renderer.
     * <p>
     * <b>State Persistence:</b> When exiting edit mode, this method captures the 
     * component's content and updates the underlying ephemeral resource handle 
     * <i>before</i> switching the UI state. This prevents the viewer's synchronization 
     * loop from reverting the text to its original state.
     * </p>
     */
    protected void toggleEdit() {
        if (editing) {
            // CAPTURE TRANSITION: capture before changing state
            currentContent = getCurrentContentFromComponent();
            
            // AUTHORITATIVE UPDATE: update handle immediately to ensure viewer sync loop 
            // sees the new data during onPreviewActivated.
            updateComponentContent(currentContent);
        }

        editing = !editing;
        setComponentEditable(editing);
        
        if (editButton != null) {
            editButton.setText(editing ? "Save" : "Edit");
            editButton.setIcon(editing ? new RestartIcon(12) : null);
        }
        if (cancelButton != null) {
            cancelButton.setVisible(editing);
        }
        if (!editing) {
            // Signal higher-level persistence (e.g., updating message history)
            if (onSave != null) {
                onSave.accept(currentContent);
            }
        }
        
        component.revalidate();
        component.repaint();
    }

    /**
     * {@inheritDoc}
     * <p>Matches the segment if the type is CODE and the language matches.</p>
     */
    @Override
    public boolean matches(TextSegmentDescriptor descriptor) {
        return descriptor.type() == TextSegmentType.CODE && Objects.equals(language, descriptor.language());
    }
}
