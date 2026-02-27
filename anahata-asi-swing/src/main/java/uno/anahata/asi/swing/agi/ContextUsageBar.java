/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.agi;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import javax.swing.JPanel;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.context.ContextManager;
import uno.anahata.asi.status.AgiStatus;
import uno.anahata.asi.status.StatusManager;

/**
 * A custom JPanel that renders a progress bar for context window usage.
 *
 * @author anahata
 */
public class ContextUsageBar extends JPanel {

    private static final DecimalFormat PERCENT_FORMAT = new DecimalFormat("0.0%");
    private static final NumberFormat NUMBER_FORMAT = NumberFormat.getInstance();
    private static final int BAR_HEIGHT = 20;
    private static final int MIN_WIDTH = 250;

    private final AgiPanel agiPanel;

    // State fields, updated in refresh()
    private int totalTokens = 0;
    private int maxTokens;
    private double percentage = 0.0;
    private String usageText = "0% (0 / 0)";
    private AgiStatus status = AgiStatus.IDLE; // Fixed: IDLE_WAITING_FOR_USER -> IDLE

    /**
     * Constructs a new ContextUsageBar.
     *
     * @param agiPanel The agi panel instance.
     */
    public ContextUsageBar(AgiPanel agiPanel) {
        this.agiPanel = agiPanel;
        this.maxTokens = agiPanel.getAgi().getContextManager().getTokenThreshold();
        setPreferredSize(new Dimension(MIN_WIDTH, BAR_HEIGHT));
        setMinimumSize(new Dimension(MIN_WIDTH, BAR_HEIGHT));
        setFont(new Font("SansSerif", Font.BOLD, 12));
        refresh(); // Initial update
    }

    /**
     * Reloads the bar with the new agi state.
     */
    public void reload() {
        this.maxTokens = agiPanel.getAgi().getContextManager().getTokenThreshold();
        refresh();
    }

    /**
     * Refreshes the bar's state by querying the agi's status and context managers.
     */
    public void refresh() {
        StatusManager statusManager = agiPanel.getAgi().getStatusManager();
        ContextManager contextManager = agiPanel.getAgi().getContextManager();

        this.status = statusManager.getCurrentStatus();
        
        // Use the total token count from the ContextManager (which includes the last response)
        this.totalTokens = contextManager.getTotalTokenCount();
        
        this.maxTokens = contextManager.getTokenThreshold();
        this.percentage = (maxTokens == 0) ? 0.0 : (double) totalTokens / maxTokens;

        String percentStr = PERCENT_FORMAT.format(percentage);
        String totalStr = NUMBER_FORMAT.format(totalTokens);
        String maxStr = NUMBER_FORMAT.format(maxTokens);

        this.usageText = String.format("%s (%s / %s)", percentStr, totalStr, maxStr);

        repaint();
    }

    @Override
    protected void paintComponent(Graphics g) {
        super.paintComponent(g);
        Graphics2D g2d = (Graphics2D) g.create();
        g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

        int width = getWidth();
        int height = getHeight();

        // Determine Colors
        Color barColor;
        Color textColor;

        if (status == AgiStatus.WAITING_WITH_BACKOFF) {
            barColor = agiPanel.getAgiConfig().getColor(status);
            textColor = Color.WHITE;
        } else {
            barColor = agiPanel.getAgiConfig().getColorForContextUsage(percentage);
            // Choose black or white text based on the brightness of the bar color for readability
            double brightness = (barColor.getRed() * 0.299) + (barColor.getGreen() * 0.587) + (barColor.getBlue() * 0.114);
            textColor = (brightness > 186) ? Color.BLACK : Color.WHITE;
        }

        // Draw Background
        g2d.setColor(getBackground().darker());
        g2d.fillRect(0, 0, width, height);

        // Draw Progress Bar
        int barWidth = (int) (width * Math.min(1.0, percentage));
        g2d.setColor(barColor);
        g2d.fillRect(0, 0, barWidth, height);

        // Draw Text Overlay
        g2d.setColor(textColor);
        g2d.setFont(getFont());
        FontMetrics fm = g2d.getFontMetrics();
        int textX = (width - fm.stringWidth(usageText)) / 2;
        int textY = ((height - fm.getHeight()) / 2) + fm.getAscent();

        g2d.drawString(usageText, textX, textY);

        g2d.dispose();
    }
}
