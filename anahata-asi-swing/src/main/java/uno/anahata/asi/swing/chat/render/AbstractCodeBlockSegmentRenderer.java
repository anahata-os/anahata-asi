/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.chat.render;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.FlowLayout;
import java.awt.Font;
import java.util.Objects;
import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.text.EditorKit;
import lombok.Getter;
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
@Getter
public abstract class AbstractCodeBlockSegmentRenderer extends AbstractTextSegmentRenderer {

    protected final String language;
    protected JComponent innerComponent;
    protected boolean editable = false;
    protected JButton editButton;

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

    @Override
    public JComponent getComponent() {
        return component;
    }

    protected abstract JComponent createInnerComponent();
    
    protected abstract void updateComponentContent(String content);
    
    protected abstract String getCurrentContentFromComponent();
    
    protected abstract void setComponentEditable(boolean editable);

    protected void toggleEdit() {
        editable = !editable;
        setComponentEditable(editable);
        editButton.setText(editable ? "Save" : "Edit");
        
        if (!editable) {
            currentContent = getCurrentContentFromComponent();
        }
        
        component.revalidate();
        component.repaint();
    }

    @Override
    public boolean matches(TextSegmentDescriptor descriptor) {
        return descriptor.type() == TextSegmentType.CODE && Objects.equals(language, descriptor.language());
    }
}
