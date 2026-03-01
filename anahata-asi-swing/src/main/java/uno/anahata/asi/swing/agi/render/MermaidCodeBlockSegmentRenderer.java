/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.agi.render;

import java.awt.CardLayout;
import java.awt.Color;
import java.awt.Font;
import java.awt.Image;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.util.Base64;
import java.util.Objects;
import javax.imageio.ImageIO;
import javax.swing.ImageIcon;
import javax.swing.JButton;
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
    
    /** Button to copy the rendered image to the clipboard. */
    private JButton copyImageButton;
    /** The last successfully rendered image. */
    private Image currentImage;
    /** The Mermaid source used for the last successful render. */
    private String lastRenderedDiagramContent;

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

        // 2. The Editor Card: Use the authoritative provider to get line numbers/IDE fidelity
        innerEditor = agiConfig.getEditorKitProvider().createRenderer(agiPanel, currentContent, "mermaid");
        innerEditor.render(); 
        cardPanel.add(innerEditor.getInnerComponent(), "EDITOR");

        cardPanel.setOpaque(false);
        
        if (closed) {
            cardLayout.show(cardPanel, "IMAGE");
            updateDiagram();
        } else {
            imageLabel.setText("Waiting for diagram code to complete...");
            cardLayout.show(cardPanel, "EDITOR");
        }
        
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

        // Deferred Rendering: Don't call the server while the block is still streaming.
        if (!closed) {
            imageLabel.setText("Streaming diagram code...");
            imageLabel.setIcon(null);
            return;
        }
        
        // Guard: Don't re-render if the content hasn't changed.
        if (Objects.equals(currentContent, lastRenderedDiagramContent)) {
            return;
        }

        imageLabel.setText("Generating diagram...");
        if (copyImageButton != null) copyImageButton.setEnabled(false);
        
        // Use standard SwingTask with showError=false to prevent popups during transient failures
        new SwingTask<Image>(null, "Mermaid Renderer", () -> {
            String encoded = Base64.getEncoder().encodeToString(currentContent.getBytes(StandardCharsets.UTF_8));
            String urlString = "https://mermaid.ink/img/" + encoded;
            URL url = new URL(urlString);
            Image image = ImageIO.read(url);
            if (image == null) throw new Exception("Failed to decode image from server.");
            return image;
        }, image -> {
            this.currentImage = image;
            this.lastRenderedDiagramContent = currentContent;
            imageLabel.setText("");
            imageLabel.setIcon(new ImageIcon(image));
            if (copyImageButton != null) copyImageButton.setEnabled(true);
            imageLabel.revalidate();
            imageLabel.repaint();
        }, e -> {
            log.error("Failed to render Mermaid diagram", e);
            imageLabel.setText("<html><center>Failed to render diagram<br>" + e.getMessage() + "</center></html>");
            imageLabel.setIcon(null);
            if (copyImageButton != null) copyImageButton.setEnabled(false);
        }, false).execute();
    }

    /** {@inheritDoc} */
    @Override
    protected void updateComponentContent(String content) {
        if (innerEditor != null) {
            innerEditor.updateContent(content);
        }
        
        // Automatic transition: if we just closed, show the image card and update
        if (closed && !editing) {
            cardLayout.show(cardPanel, "IMAGE");
            updateDiagram();
        } else if (!closed && !editing) {
            cardLayout.show(cardPanel, "EDITOR");
        }
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
    
    /** {@inheritDoc} */
    @Override
    protected void addExtraHeaderButtons(JPanel leftHeaderPanel) {
        copyImageButton = new JButton("Copy Image");
        copyImageButton.setToolTipText("Copy Diagram to OS Clipboard");
        copyImageButton.setFont(new Font("SansSerif", Font.PLAIN, 11));
        copyImageButton.setMargin(new java.awt.Insets(1, 5, 1, 5));
        copyImageButton.setFocusPainted(false);
        copyImageButton.setEnabled(currentImage != null);
        copyImageButton.addActionListener(e -> SwingUtils.copyImageToClipboard(currentImage));
        leftHeaderPanel.add(copyImageButton);
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
