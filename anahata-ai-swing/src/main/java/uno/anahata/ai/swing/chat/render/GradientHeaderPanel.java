/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.ai.swing.chat.render;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.GradientPaint;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import javax.swing.BorderFactory;
import javax.swing.JLabel;
import javax.swing.JPanel;
import lombok.Getter;
import lombok.NonNull;

/**
 * A custom JPanel that renders a gradient background and holds a JLabel for the header text.
 * This component is designed to be used as the header for JXTitledPanels in the chat UI.
 *
 * @author anahata
 */
@Getter
public class GradientHeaderPanel extends JPanel {

    private final Color startColor;
    private final Color endColor;
    private final Color foregroundColor;
    private final JLabel infoLabel;

    /**
     * Constructs a new GradientHeaderPanel.
     *
     * @param startColor The starting color for the gradient.
     * @param endColor The ending color for the gradient.
     * @param foregroundColor The foreground color for the info label.
     */
    public GradientHeaderPanel(@NonNull Color startColor, @NonNull Color endColor, @NonNull Color foregroundColor) {
        super(new BorderLayout(10, 0));
        this.startColor = startColor;
        this.endColor = endColor;
        this.foregroundColor = foregroundColor;
        setOpaque(false); // Ensure paintComponent is called
        setBorder(BorderFactory.createEmptyBorder(8, 12, 8, 12));

        infoLabel = new JLabel();
        infoLabel.setForeground(foregroundColor);
        add(infoLabel, BorderLayout.CENTER);
    }

    /**
     * Updates the text displayed in the header's info label.
     * @param text The new text to display.
     */
    public void setInfoText(String text) {
        infoLabel.setText(text);
    }

    @Override
    protected void paintComponent(Graphics g) {
        super.paintComponent(g);
        Graphics2D g2d = (Graphics2D) g.create();
        g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

        GradientPaint gp = new GradientPaint(0, 0, startColor, getWidth(), 0, endColor);
        g2d.setPaint(gp);
        g2d.fillRect(0, 0, getWidth(), getHeight());
        g2d.dispose();
    }
}
