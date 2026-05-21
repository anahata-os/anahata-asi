/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.agi.message.part.text;

import java.awt.CardLayout;
import java.awt.Color;
import java.awt.Font;
import java.awt.Image;
import java.awt.Insets;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.util.Base64;
import java.util.Objects;
import javax.imageio.ImageIO;
import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.SwingConstants;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.swing.agi.AgiPanel;
import uno.anahata.asi.swing.internal.SwingTask;
import uno.anahata.asi.swing.internal.SwingUtils;

/**
 * A specialized renderer for Mermaid diagrams.
 * <p>
 * This renderer translates Mermaid code blocks into visual diagrams using the 
 * mermaid.ink service. It supports a visual 'Diagram' mode and an 'Editor' mode 
 * for modifying the source.
 * </p>
 * <p>
 * <b>Persistence:</b> Switching from 'Edit Source' back to 'View Diagram' 
 * authoritatively triggers the save callback, updating the ASI's message history.
 * </p>
 * 
 * @author anahata
 */
@Slf4j
public class MermaidCodeBlockSegmentRenderer extends CodeBlockSegmentRenderer {

    /** Card layout for switching between the diagram image and the code editor. */
    private final CardLayout cardLayout = new CardLayout();
    /** Panel containing the image and editor components. */
    private final JPanel cardPanel = new JPanel(cardLayout);
    /** Label used to display the rendered diagram or status messages. */
    private final JLabel imageLabel = new JLabel("Loading diagram...", SwingConstants.CENTER);
    /** The inner editor used for modifying the Mermaid code. */
    private CodeBlockSegmentRenderer innerEditor;
    
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
     * {@inheritDoc}
     * <p>
     * Implementation details:
     * Creates a card panel containing a scrollable visual diagram label and a 
     * nested CodeBlockSegmentRenderer for source editing. The inner editor 
     * is configured to be headerless to prevent redundant UI controls.
     * </p>
     */
    @Override
    protected JComponent createInnerComponent() {
        // 1. The Image Card: Shows the rendered diagram inside a scroll pane
        imageLabel.setOpaque(true);
        imageLabel.setBackground(Color.WHITE);
        
        JScrollPane imageScroll = new JScrollPane(imageLabel);
        imageScroll.setBorder(BorderFactory.createEmptyBorder());
        imageScroll.setOpaque(false);
        imageScroll.getViewport().setOpaque(false);
        // Ensure vertical scrolling is disabled to pass through to conversation
        imageScroll.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_NEVER);
        imageScroll.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        
        imageScroll.addMouseWheelListener(e -> SwingUtils.redispatchMouseWheelEvent(imageScroll, e));
        
        cardPanel.add(imageScroll, "IMAGE");

        // 2. The Editor Card: Use the authoritative provider to get line numbers/IDE fidelity
        // RECURSION FIX: Directly instantiate a generic code renderer for the source 
        // to avoid infinitely calling the mermaid renderer.
        innerEditor = new CodeBlockSegmentRenderer(agiPanel, currentContent, "text");
        innerEditor.setHeaderVisible(false); // Fix: prevent double header
        innerEditor.setEditable(true);
        innerEditor.render(); 
        cardPanel.add(innerEditor.getComponent(), "EDITOR");

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
        if (copyImageButton != null) {
            copyImageButton.setEnabled(false);
        }
        
        // Use standard SwingTask with showError=false to prevent popups during transient failures
        new SwingTask<Image>(agiPanel, "Mermaid Renderer", () -> {
            String encoded = Base64.getEncoder().encodeToString(currentContent.getBytes(StandardCharsets.UTF_8));
            String urlString = "https://mermaid.ink/img/" + encoded;
            URL url = new URL(urlString);
            Image image = ImageIO.read(url);
            if (image == null) {
                throw new Exception("Failed to decode image from server.");
            }
            return image;
        }, image -> {
            this.currentImage = image;
            this.lastRenderedDiagramContent = currentContent;
            imageLabel.setText("");
            imageLabel.setIcon(new ImageIcon(image));
            if (copyImageButton != null) {
                copyImageButton.setEnabled(true);
            }
            imageLabel.revalidate();
            imageLabel.repaint();
        }, e -> {
            log.error("Failed to render Mermaid diagram", e);
            imageLabel.setText("<html><center>Failed to render diagram<br>" + e.getMessage() + "</center></html>");
            imageLabel.setIcon(null);
            if (copyImageButton != null) {
                copyImageButton.setEnabled(false);
            }
        }, false).start();
    }

    /** 
     * {@inheritDoc} 
     * <p>
     * Implementation details:
     * Propagates streaming content updates to the internal source editor card 
     * and handles the automatic transition to Diagram mode once generation 
     * is complete.
     * </p>
     */
    @Override
    protected void updateComponentContent(String content) {
        if (innerEditor != null) {
            innerEditor.updateContent(content);
            innerEditor.render(); // Propagation: Ensure high-fidelity streaming in source view
        }
        
        // Automatic transition: if we just closed, show the image card and update
        if (closed && !editing) {
            cardLayout.show(cardPanel, "IMAGE");
            updateDiagram();
        } else if (!closed && !editing) {
            cardLayout.show(cardPanel, "EDITOR");
        }
    }

    /** 
     * {@inheritDoc} 
     * <p>
     * Implementation details:
     * Retrieves the current source code from the internal editor card.
     * </p>
     */
    @Override
    protected String getCurrentContentFromComponent() {
        return (innerEditor != null) ? innerEditor.getCurrentContentFromComponent() : currentContent;
    }

    /** 
     * {@inheritDoc} 
     * <p>
     * Implementation details:
     * Toggles the editability of the internal source editor component.
     * </p>
     */
    @Override
    protected void setComponentEditable(boolean editable) {
        if (innerEditor != null) {
            innerEditor.setComponentEditable(editable);
        }
    }
    
    /** 
     * {@inheritDoc} 
     * <p>
     * Implementation details:
     * Injects the 'Copy Image' button into the control header.
     * </p>
     */
    @Override
    protected void addExtraHeaderButtons(JPanel leftHeaderPanel) {
        copyImageButton = new JButton("Copy Image");
        copyImageButton.setToolTipText("Copy Diagram to OS Clipboard");
        copyImageButton.setFont(new Font("SansSerif", Font.PLAIN, 11));
        copyImageButton.setMargin(new Insets(1, 5, 1, 5));
        copyImageButton.setFocusPainted(false);
        copyImageButton.setEnabled(currentImage != null);
        copyImageButton.addActionListener(e -> SwingUtils.copyImageToClipboard(currentImage));
        leftHeaderPanel.add(copyImageButton);
    }
    
    /**
     * {@inheritDoc} 
     * <p>
     * Implementation details:
     * Extends the base toggle logic to switch between Diagram/Editor cards. 
     * Returning to Diagram mode authoritatively captures the source, resets 
     * the feedback label, and triggers the save callback to update the 
     * message history.
     * </p>
     */
    @Override
    protected void toggleEdit() {
        if (editing) {
            // CAPTURE TRANSITION: capture before changing state to prevent sync-reversion
            currentContent = getCurrentContentFromComponent();
            
            // AUTHORITATIVE UPDATE: update handle immediately so sync loop sees new data
            updateComponentContent(currentContent);
        }

        editing = !editing;
        setComponentEditable(editing);
        
        if (editButton != null) {
            editButton.setText(editing ? "View Diagram" : "Edit Source");
        }
        
        if (editing) {
            cardLayout.show(cardPanel, "EDITOR");
        } else {
            // User clicked 'Save' (View Diagram)
            imageLabel.setIcon(null);
            imageLabel.setText("Generating diagram...");
            updateDiagram();
            cardLayout.show(cardPanel, "IMAGE");
            
            // AUTHORITATIVE SYNC: Trigger the save callback to update message history
            if (onSave != null) {
                onSave.accept(currentContent);
            }
        }
        
        component.revalidate();
        component.repaint();
    }
}
