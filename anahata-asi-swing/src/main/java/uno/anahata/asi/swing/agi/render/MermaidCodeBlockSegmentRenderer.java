/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.agi.render;

import java.awt.CardLayout;
import java.awt.Color;
import java.awt.Image;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.util.Base64;
import javax.imageio.ImageIO;
import javax.swing.ImageIcon;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.SwingConstants;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.swing.agi.AgiPanel;
import uno.anahata.asi.swing.internal.SwingTask;
import uno.anahata.asi.swing.internal.SwingUtils;

/**
 * A specialized renderer for Mermaid diagrams.
 * This renderer translates Mermaid code blocks into visual diagrams using the 
 * mermaid.ink service. It supports a visual 'Diagram' mode and an 'Editor' mode 
 * for modifying the source.
 * 
 * @author anahata
 */
@Slf4j
public class MermaidCodeBlockSegmentRenderer extends AbstractCodeBlockSegmentRenderer {

    /** Card layout for switching between the diagram image and the code editor. */
    private final CardLayout cardLayout = new CardLayout();
    /** Panel containing the image and editor components. */
    private final JPanel cardPanel = new JPanel(cardLayout);
    /** Label used to display the rendered diagram or status messages. */
    private final JLabel imageLabel = new JLabel("Loading diagram...", SwingConstants.CENTER);
    /** The inner editor used for modifying the Mermaid code. */
    private AbstractCodeBlockSegmentRenderer innerEditor;

    /**
     * Constructs a new MermaidCodeBlockSegmentRenderer.
     * 
     * @param agiPanel The agi panel instance.
     * @param initialContent The initial Mermaid code.
     * @param language The language identifier (expected to be 'mermaid').
     */
    public MermaidCodeBlockSegmentRenderer(AgiPanel agiPanel, String initialContent, String language) {
        super(agiPanel, initialContent, language);
        this.editable = true;
    }

    /**
     * Creates the inner component for this renderer, which is a card panel 
     * containing the image label and an RSyntax editor.
     */
    @Override
    protected JComponent createInnerComponent() {
        // 1. The Image Card: Shows the rendered diagram
        imageLabel.setOpaque(true);
        imageLabel.setBackground(Color.WHITE);
        cardPanel.add(imageLabel, "IMAGE");

        // 2. The Editor Card: Uses RSyntaxTextArea for code editing
        innerEditor = new RSyntaxTextAreaCodeBlockSegmentRenderer(agiPanel, currentContent, "mermaid");
        innerEditor.render(); // Initialize the sub-renderer
        cardPanel.add(innerEditor.getInnerComponent(), "EDITOR");

        cardLayout.show(cardPanel, "IMAGE");
        cardPanel.setOpaque(false);
        
        updateDiagram();
        
        return cardPanel;
    }

    /**
     * Generates the diagram image asynchronously using mermaid.ink.
     */
    private void updateDiagram() {
        if (currentContent == null || currentContent.trim().isEmpty()) {
            imageLabel.setText("No diagram content.");
            imageLabel.setIcon(null);
            return;
        }

        imageLabel.setText("Generating diagram...");
        
        // Use standard SwingTask for robust background execution and error handling
        new SwingTask<Image>(null, "Mermaid Renderer", () -> {
            // simple base64 encoding of the source code for mermaid.ink
            String encoded = Base64.getEncoder().encodeToString(currentContent.getBytes(StandardCharsets.UTF_8));
            String urlString = "https://mermaid.ink/img/" + encoded;
            URL url = new URL(urlString);
            Image image = ImageIO.read(url);
            if (image == null) throw new Exception("Failed to decode image from server.");
            return image;
        }, image -> {
            imageLabel.setText("");
            imageLabel.setIcon(new ImageIcon(image));
            imageLabel.revalidate();
            imageLabel.repaint();
        }, e -> {
            log.error("Failed to render Mermaid diagram", e);
            imageLabel.setText("<html><center>Failed to render diagram<br>" + e.getMessage() + "</center></html>");
            imageLabel.setIcon(null);
        }).execute();
    }

    /** {@inheritDoc} */
    @Override
    protected void updateComponentContent(String content) {
        if (innerEditor != null) {
            innerEditor.updateContent(content);
        }
        updateDiagram();
    }

    /** {@inheritDoc} */
    @Override
    protected String getCurrentContentFromComponent() {
        return innerEditor != null ? innerEditor.getCurrentContentFromComponent() : currentContent;
    }

    /** {@inheritDoc} */
    @Override
    protected void setComponentEditable(boolean editable) {
        // Handled by card switching and inner editor
    }
    
    /**
     * Overrides the default toggle logic to switch between cards and trigger 
     * diagram updates.
     */
    @Override
    protected void toggleEdit() {
        editing = !editing;
        editButton.setText(editing ? "View Diagram" : "Edit Source");
        
        if (editing) {
            cardLayout.show(cardPanel, "EDITOR");
        } else {
            // User clicked 'Save' (View Diagram)
            currentContent = getCurrentContentFromComponent();
            updateDiagram();
            cardLayout.show(cardPanel, "IMAGE");
        }
        
        component.revalidate();
        component.repaint();
    }
}
