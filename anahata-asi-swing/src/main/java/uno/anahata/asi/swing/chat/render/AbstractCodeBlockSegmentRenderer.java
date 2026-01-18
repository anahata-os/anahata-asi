/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.chat.render;

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
import javax.swing.text.EditorKit;
import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.swing.chat.ChatPanel;
import uno.anahata.asi.swing.chat.render.editorkit.EditorKitProvider;
import uno.anahata.asi.swing.icons.CopyIcon;
import uno.anahata.asi.swing.internal.SwingUtils;

/**
 * Base class for code block segment renderers. It provides the common UI
 * structure, including the header with language label, copy button, and edit toggle.
 *
 * @author anahata
 */
@Slf4j
public abstract class AbstractCodeBlockSegmentRenderer extends AbstractTextSegmentRenderer {

    /** The programming language of the code block. */
    @Getter
    protected final String language;
    
    /** The inner component that actually displays/edits the code. */
    @Getter
    protected JComponent innerComponent;
    
    /** Whether the code block is currently in edit mode. */
    @Getter
    protected boolean editable = false;
    
    /** The button used to toggle between edit and view modes. */
    protected JButton editButton;
    
    /** Callback triggered when the user clicks 'Save' after editing. */
    @Setter
    private Consumer<String> onSave;

    /**
     * Constructs a new AbstractCodeBlockSegmentRenderer.
     *
     * @param chatPanel The chat panel instance.
     * @param initialContent The initial code content.
     * @param language The programming language.
     */
    public AbstractCodeBlockSegmentRenderer(ChatPanel chatPanel, String initialContent, String language) {
        super(chatPanel, initialContent);
        this.language = language;
    }

    /**
     * Factory method to create the appropriate code block renderer based on the
     * available EditorKitProvider.
     * 
     * @param chatPanel The chat panel.
     * @param content The initial content.
     * @param language The language.
     * @return A concrete AbstractCodeBlockSegmentRenderer instance.
     */
    public static AbstractCodeBlockSegmentRenderer create(ChatPanel chatPanel, String content, String language) {
        EditorKitProvider editorKitProvider = chatPanel.getChatConfig().getEditorKitProvider();
        if (editorKitProvider != null) {
            try {
                EditorKit kit = editorKitProvider.getEditorKitForLanguage(language);
                if (kit != null) {
                    return new JEditorPaneCodeBlockSegmentRenderer(chatPanel, content, language, kit);
                }
            } catch (Exception e) {
                log.error("Failed to obtain EditorKit for language: {}", language, e);
            }
        }
        return new RSyntaxTextAreaCodeBlockSegmentRenderer(chatPanel, content, language);
    }

    /**
     * {@inheritDoc}
     * Renders the code block with a header containing the language, a copy button,
     * and an edit toggle.
     */
    @Override
    public boolean render() {
        boolean changed = hasContentChanged();

        if (component == null) {
            innerComponent = createInnerComponent();
            
            // Redispatch mouse wheel events from the inner component to the parent scroll pane
            innerComponent.addMouseWheelListener(e -> SwingUtils.redispatchMouseWheelEvent(innerComponent, e));

            JPanel container = new JPanel(new BorderLayout());
            container.setOpaque(false);
            container.setBorder(BorderFactory.createLineBorder(new Color(200, 200, 200), 1, true));
            
            // Header Panel
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
            copyButton.addActionListener(e -> SwingUtils.copyToClipboard(getCurrentContentFromComponent()));
            leftHeaderPanel.add(copyButton);
            
            editButton = new JButton("Edit");
            editButton.setToolTipText("Toggle Edit Mode");
            editButton.setFont(new Font("SansSerif", Font.PLAIN, 11));
            editButton.setMargin(new java.awt.Insets(1, 5, 1, 5));
            editButton.setFocusPainted(false);
            editButton.addActionListener(e -> toggleEdit());
            leftHeaderPanel.add(editButton);
            
            headerPanel.add(leftHeaderPanel, BorderLayout.WEST);
            container.add(headerPanel, BorderLayout.NORTH);
            
            // ScrollPane
            JScrollPane scrollPane = new JScrollPane(innerComponent);
            scrollPane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_NEVER);
            scrollPane.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
            scrollPane.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
            scrollPane.setOpaque(false);
            scrollPane.getViewport().setOpaque(false);
            
            scrollPane.addMouseWheelListener(e -> SwingUtils.redispatchMouseWheelEvent(scrollPane, e));
            container.add(scrollPane, BorderLayout.CENTER);

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
     * Creates the inner component that will display or edit the code.
     * 
     * @return The inner JComponent.
     */
    protected abstract JComponent createInnerComponent();
    
    /**
     * Updates the content of the inner component.
     * 
     * @param content The new code content.
     */
    protected abstract void updateComponentContent(String content);
    
    /**
     * Retrieves the current content from the inner component.
     * 
     * @return The current code content.
     */
    protected abstract String getCurrentContentFromComponent();
    
    /**
     * Sets the editability of the inner component.
     * 
     * @param editable True to enable editing, false to disable.
     */
    protected abstract void setComponentEditable(boolean editable);

    /**
     * Toggles the edit mode of the renderer.
     */
    protected void toggleEdit() {
        editable = !editable;
        setComponentEditable(editable);
        editButton.setText(editable ? "Save" : "Edit");
        
        if (!editable) {
            currentContent = getCurrentContentFromComponent();
            if (onSave != null) {
                onSave.accept(currentContent);
            }
        }
        
        component.revalidate();
        component.repaint();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean matches(TextSegmentDescriptor descriptor) {
        return descriptor.type() == TextSegmentType.CODE && Objects.equals(language, descriptor.language());
    }
}
