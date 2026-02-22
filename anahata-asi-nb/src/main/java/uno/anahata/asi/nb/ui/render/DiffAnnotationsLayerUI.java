/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.ui.render;

import java.awt.AWTEvent;
import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.event.MouseEvent;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import javax.swing.JComponent;
import javax.swing.JEditorPane;
import javax.swing.JLayer;
import javax.swing.JSplitPane;
import javax.swing.SwingUtilities;
import javax.swing.plaf.LayerUI;
import javax.swing.text.Element;
import org.openide.util.ImageUtilities;
import uno.anahata.asi.toolkit.files.LineComment;

/**
 * A highly specialized {@link LayerUI} designed to decorate NetBeans components—specifically 
 * the native Diff Viewer—with agentic annotations that reveal themselves on hover.
 * 
 * <p>This decorator uses the {@link JLayer} pattern to paint Anahata branding icons
 * next to commented lines. When the user hovers over an icon, a "Comic-style" translucent 
 * comment bubble expands to show the AI's feedback. This ensures that the code remains 
 * fully readable until the user explicitly requests to see the comments.</p>
 * 
 * @author anahata
 */
public class DiffAnnotationsLayerUI extends LayerUI<JComponent> {

    /** The mapping of 1-based line numbers to their respective AI-generated comments. */
    private final Map<Integer, String> comments = new HashMap<>();
    
    /** Master toggle for visibility controlled by the renderer's checkbox. */
    private boolean showingComments = true;
    
    /** The current position of the mouse relative to the layer. */
    private Point mousePos = new Point(-1, -1);
    
    /** The official Anahata branding icon used as a line marker. */
    private final Image anahataIcon;
    
    /** The translucent background color for the comment bubbles. */
    private final Color bubbleBg = new Color(255, 255, 220); // Solid cream for readability on hover
    
    /** The subtle border color for the comment bubbles. */
    private final Color bubbleBorder = new Color(160, 140, 0, 150);
    
    /** The color used for the comment text itself. */
    private final Color textColor = Color.BLACK;

    /**
     * Constructs a new decorator with the provided comment list.
     * 
     * @param lineComments A list of {@link LineComment} objects.
     */
    public DiffAnnotationsLayerUI(List<LineComment> lineComments) {
        if (lineComments != null) {
            for (LineComment lc : lineComments) {
                this.comments.put(lc.getLineNumber(), lc.getComment());
            }
        }
        this.anahataIcon = ImageUtilities.loadImage("icons/anahata_16.png");
    }

    /**
     * Toggles the visibility of the agentic annotations.
     * 
     * @param showing {@code true} to show markers, {@code false} to hide them.
     */
    public void setShowingComments(boolean showing) {
        this.showingComments = showing;
    }

    /**
     * {@inheritDoc}
     * 
     * <p>Configures the layer to receive mouse motion events to track hovers.</p>
     */
    @Override
    public void installUI(JComponent c) {
        super.installUI(c);
        if (c instanceof JLayer l) {
            l.setLayerEventMask(AWTEvent.MOUSE_MOTION_EVENT_MASK);
        }
    }

    /**
     * {@inheritDoc}
     * 
     * <p>Tracks the mouse position and triggers repaints to update hover states.</p>
     */
    @Override
    protected void processMouseMotionEvent(MouseEvent e, JLayer<? extends JComponent> l) {
        // Map the event point from the source component to the JLayer's space
        mousePos = SwingUtilities.convertPoint(e.getComponent(), e.getPoint(), l);
        l.repaint();
    }

    /**
     * {@inheritDoc}
     * 
     * <p>Overridden to perform agentic decoration after the original component tree 
     * has finished its standard painting pass.</p>
     */
    @Override
    public void paint(Graphics g, JComponent c) {
        super.paint(g, c); 
        
        if (!showingComments || comments == null || comments.isEmpty()) {
            return;
        }

        // 1. Target Discovery: Find the proposed side's editor within the Diff structure.
        JEditorPane editor = findRightEditor(c);
        if (editor == null || !editor.isShowing()) {
            return;
        }

        Graphics2D g2 = (Graphics2D) g.create();
        try {
            g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
            g2.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON);

            g2.setFont(new Font("SansSerif", Font.PLAIN, 11));
            FontMetrics fm = g2.getFontMetrics();

            // 2. Viewport Optimization: Only process lines currently visible to the user.
            Rectangle visibleRect = editor.getVisibleRect();

            for (Map.Entry<Integer, String> entry : comments.entrySet()) {
                try {
                    Element root = editor.getDocument().getDefaultRootElement();
                    int lineIdx = entry.getKey() - 1;
                    
                    if (lineIdx >= 0 && lineIdx < root.getElementCount()) {
                        Rectangle rect = editor.modelToView(root.getElement(lineIdx).getStartOffset());
                        
                        if (rect != null && visibleRect.intersects(rect)) {
                            // 3. Coordinate Translation
                            Point p = SwingUtilities.convertPoint(editor, rect.x, rect.y, c);
                            int yCenter = p.y + rect.height / 2;
                            int xStart = p.x; 

                            // Define the hit-box for the icon
                            Rectangle iconBounds = new Rectangle(xStart + 5, yCenter - 8, 16, 16);
                            
                            // Draw the Anahata Branding Icon
                            if (anahataIcon != null) {
                                g2.drawImage(anahataIcon, iconBounds.x, iconBounds.y, null);
                            } else {
                                // Fallback if icon is missing
                                g2.setColor(new Color(0, 120, 215, 180));
                                g2.fillOval(iconBounds.x, iconBounds.y, 16, 16);
                            }
                            
                            // 4. Hover Reveal Logic: Only show the bubble if hovering over the icon
                            if (iconBounds.contains(mousePos)) {
                                String text = entry.getValue();
                                int bw = fm.stringWidth(text) + 12;
                                int bh = fm.getHeight() + 4;
                                int bx = xStart + 25;
                                int by = yCenter - bh / 2;

                                // Render the Bubble
                                g2.setColor(bubbleBg);
                                g2.fillRoundRect(bx, by, bw, bh, 8, 8);
                                g2.setColor(bubbleBorder);
                                g2.drawRoundRect(bx, by, bw, bh, 8, 8);
                                
                                // Render the pointer
                                int[] px = {bx, bx - 6, bx};
                                int[] py = {yCenter - 4, yCenter, yCenter + 4};
                                g2.fillPolygon(px, py, 3);
                                g2.drawPolyline(px, py, 3);

                                // Render the Comment Text
                                g2.setColor(textColor);
                                g2.drawString(text, bx + 6, by + fm.getAscent() + 1);
                            }
                        }
                    }
                } catch (Exception ex) {
                    // Ignore transient mapping errors
                }
            }
        } finally {
            g2.dispose();
        }
    }

    /**
     * Recursively traverses the component hierarchy to find the editor pane on the 
     * right side of the main split pane.
     * 
     * @param root The container to start the search from.
     * @return The identified {@link JEditorPane}, or {@code null} if not found.
     */
    private JEditorPane findRightEditor(Container root) {
        JSplitPane sp = findJSplitPane(root);
        if (sp == null) {
            return null;
        }
        return findEditorPane(sp.getRightComponent());
    }

    private JSplitPane findJSplitPane(Container c) {
        if (c instanceof JSplitPane sp) {
            return sp;
        }
        for (Component child : c.getComponents()) {
            if (child instanceof Container cont) {
                JSplitPane found = findJSplitPane(cont);
                if (found != null) {
                    return found;
                }
            }
        }
        return null;
    }

    private JEditorPane findEditorPane(Component c) {
        if (c instanceof JEditorPane pane && pane.getClass().getName().contains("DecoratedEditorPane")) {
            return pane;
        }
        if (c instanceof Container cont) {
            for (Component child : cont.getComponents()) {
                JEditorPane found = findEditorPane(child);
                if (found != null) {
                    return found;
                }
            }
        }
        return null;
    }
}
