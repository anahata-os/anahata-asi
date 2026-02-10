/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.ui.diff;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Image;
import java.awt.event.ActionEvent;
import java.util.ArrayList;
import java.util.List;
import javax.swing.AbstractAction;
import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JEditorPane;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;
import javax.swing.undo.UndoManager;
import lombok.extern.slf4j.Slf4j;
import net.miginfocom.swing.MigLayout;
import org.jdesktop.swingx.prompt.PromptSupport;
import org.openide.windows.TopComponent;
import org.openide.windows.WindowManager;
import uno.anahata.asi.model.resource.TextReplacement;
import uno.anahata.asi.swing.icons.DeleteIcon;
import uno.anahata.asi.swing.icons.FramesIcon;
import uno.anahata.asi.swing.internal.UICapture;

import uno.anahata.asi.swing.components.WrappingEditorPane;

/**
 * A UI component representing a single {@link TextReplacement} with a checkbox for cherry-picking
 * and a feedback area.
 * 
 * @author anahata
 */
@Slf4j
public class ReplacementCard extends JPanel {
    /** The checkbox for selecting this replacement. */
    private final JCheckBox checkBox = new JCheckBox();
    /** The text area for providing feedback. */
    private final JTextArea commentArea = new JTextArea();
    /** The panel displaying thumbnail previews of captured screenshots. */
    private final JPanel screenshotPreviewPanel = new JPanel(new MigLayout("insets 0", "[]5[]", "[]"));
    /** The list of captured screenshots as byte arrays. */
    private final List<byte[]> screenshots = new ArrayList<>();
    /** The underlying replacement data model. */
    private final TextReplacement replacement;

    /**
     * Constructs a new ReplacementCard.
     * 
     * @param replacement The replacement model.
     * @param validationError An optional validation error message.
     * @param onToggle A callback triggered when the selection checkbox is toggled.
     */
    public ReplacementCard(TextReplacement replacement, String validationError, Runnable onToggle) {
        this.replacement = replacement;
        setLayout(new MigLayout("fillx, insets 8", "[][grow]", "[]2[]5[]5[60:120, grow]5[]5[]"));
        setBorder(BorderFactory.createEtchedBorder());
        
        if (validationError != null) {
            checkBox.setSelected(false);
            checkBox.setEnabled(false);
            setBackground(new Color(255, 235, 235)); // Light red for errors
        } else {
            checkBox.setSelected(true);
        }
        checkBox.addActionListener(e -> onToggle.run());
        
        String targetPreview = escape(replacement.getTarget());
        WrappingEditorPane summary = new WrappingEditorPane();
        summary.setContentType("text/html");
        summary.setText("<html>Replace: <b>" + targetPreview + "</b></html>");
        summary.setFont(new Font("SansSerif", Font.PLAIN, 12));
        summary.setEditable(false);
        summary.setOpaque(false);
        summary.setToolTipText(replacement.getTarget());
        
        add(checkBox, "spany, top");
        add(summary, "growx, wrap");

        if (validationError != null) {
            JLabel errLabel = new JLabel("<html><b>ERROR:</b> " + validationError + "</html>");
            errLabel.setForeground(Color.RED.darker());
            errLabel.setFont(new Font("SansSerif", Font.BOLD, 11));
            add(errLabel, "growx, wrap");
        }
        
        String reason = replacement.getReason() != null ? replacement.getReason() : "No reason provided.";
        JEditorPane reasonPane = new JEditorPane("text/html", "<html><b>Reason:</b> " + reason + "</html>");
        reasonPane.setEditable(false);
        reasonPane.setOpaque(false);
        reasonPane.setFont(new Font("SansSerif", Font.PLAIN, 11));
        reasonPane.setForeground(java.awt.Color.GRAY);
        reasonPane.putClientProperty(JEditorPane.HONOR_DISPLAY_PROPERTIES, Boolean.TRUE);
        add(reasonPane, "growx, wrap");

        add(new JLabel("Feedback:"), "growx, wrap");
        
        commentArea.setRows(2);
        commentArea.setFont(new Font("SansSerif", Font.PLAIN, 12));
        commentArea.setLineWrap(true);
        commentArea.setWrapStyleWord(true);
        PromptSupport.setPrompt("Feedback for this change...", commentArea);
        
        // Undo/Redo Support
        UndoManager undoManager = new UndoManager();
        commentArea.getDocument().addUndoableEditListener(undoManager);
        commentArea.getInputMap().put(KeyStroke.getKeyStroke("control Z"), "Undo");
        commentArea.getActionMap().put("Undo", new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent e) {
                if (undoManager.canUndo()) {
                    undoManager.undo();
                }
            }
        });
        commentArea.getInputMap().put(KeyStroke.getKeyStroke("control Y"), "Redo");
        commentArea.getActionMap().put("Redo", new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent e) {
                if (undoManager.canRedo()) {
                    undoManager.redo();
                }
            }
        });
        
        JScrollPane scroll = new JScrollPane(commentArea);
        scroll.setMinimumSize(new Dimension(100, 60));
        add(scroll, "grow, push, wrap"); 
        
        JButton takeScreenshotBtn = new JButton("Take Screenshot", new FramesIcon(16));
        takeScreenshotBtn.setFont(new Font("SansSerif", Font.PLAIN, 10));
        takeScreenshotBtn.addActionListener(e -> takeScreenshot());
        add(takeScreenshotBtn, "right, wrap");
        
        add(screenshotPreviewPanel, "growx, wrap");
    }

    /**
     * Captures a screenshot of the currently active TopComponent or the parent window.
     */
    private void takeScreenshot() {
        try {
            Component compToCapture = null;
            
            // Focus on the diff panel for better context
            compToCapture = (Component) SwingUtilities.getAncestorOfClass(CherryPickDiffPanel.class, this);
            
            if (compToCapture == null) {
                compToCapture = SwingUtilities.getWindowAncestor(this);
            }
            
            if (compToCapture != null) {
                byte[] png = UICapture.screenshotComponent(compToCapture);
                addScreenshot(png);
            }
        } catch (Exception ex) {
            log.error("Failed to take screenshot", ex);
        }
    }

    /**
     * Adds a new screenshot to the card and refreshes the preview.
     * @param png The PNG data.
     */
    private void addScreenshot(byte[] png) {
        screenshots.add(png);
        updateScreenshotPreviews();
    }

    /**
     * Rebuilds the screenshot preview panel.
     */
    private void updateScreenshotPreviews() {
        screenshotPreviewPanel.removeAll();
        for (int i = 0; i < screenshots.size(); i++) {
            byte[] data = screenshots.get(i);
            final int index = i;
            
            JPanel item = new JPanel(new BorderLayout());
            item.setBorder(BorderFactory.createLineBorder(Color.LIGHT_GRAY));
            
            ImageIcon icon = new ImageIcon(data);
            Image img = icon.getImage().getScaledInstance(80, 60, Image.SCALE_SMOOTH);
            JLabel label = new JLabel(new ImageIcon(img));
            item.add(label, BorderLayout.CENTER);
            
            JButton removeBtn = new JButton("Remove", new DeleteIcon(12));
            removeBtn.setFont(new Font("SansSerif", Font.PLAIN, 9));
            removeBtn.setMargin(new java.awt.Insets(0, 2, 0, 2));
            removeBtn.addActionListener(e -> {
                screenshots.remove(index);
                updateScreenshotPreviews();
            });
            item.add(removeBtn, BorderLayout.SOUTH);
            
            screenshotPreviewPanel.add(item);
        }
        revalidate();
        repaint();
    }

    /**
     * Escapes HTML characters and truncates long strings for the summary label.
     * @param s The string to escape.
     * @return The escaped and potentially truncated string.
     */
    private String escape(String s) {
        if (s == null) return "";
        String escaped = s.replace("<", "&lt;").replace(">", "&gt;");
        if (escaped.length() > 60) {
            return escaped.substring(0, 60) + "...";
        }
        return escaped;
    }

    /**
     * Checks if this replacement is selected.
     * @return true if selected.
     */
    public boolean isSelected() {
        return checkBox.isSelected();
    }

    /**
     * Programmatically sets the selection state.
     * @param selected true to select.
     */
    public void setSelected(boolean selected) {
        checkBox.setSelected(selected);
    }

    /**
     * Returns the underlying replacement model.
     * @return the replacement.
     */
    public TextReplacement getReplacement() {
        return replacement;
    }

    /**
     * Returns the user's feedback comment, prefixed with the approval status.
     * @return The formatted comment string, or empty if no comment was provided.
     */
    public String getComment() {
        String comment = commentArea.getText().trim();
        if (comment.isEmpty()) return "";
        String status = isSelected() ? "[APPROVED]" : "[REJECTED]";
        return status + " " + comment;
    }
    
    /**
     * Gets the list of screenshots captured for this specific card.
     * @return A list of PNG byte arrays.
     */
    public List<byte[]> getScreenshots() {
        return screenshots;
    }
}
