/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.uc;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Component;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.geom.Ellipse2D;
import java.awt.geom.Path2D;
import java.awt.geom.RoundRectangle2D;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import org.openide.util.ImageUtilities;

/**
 * Pure Java vector and platform icon factory for the Anahata ASI Update Center.
 * <p>
 * Renders anti-aliased shields.io-grade badges, connectivity status dots, toggle switches,
 * refresh sync icons, and NetBeans plugin management icons.
 * </p>
 *
 * @author anahata
 */
public final class AnahataUcIcons {

    /**
     * Standard online green color.
     */
    public static final Color COLOR_ONLINE = new Color(22, 163, 74);
    public static final Color COLOR_ONLINE_BORDER = new Color(134, 239, 172);

    /**
     * Standard offline red color.
     */
    public static final Color COLOR_OFFLINE = new Color(220, 38, 38);
    public static final Color COLOR_OFFLINE_BORDER = new Color(252, 165, 165);

    /**
     * Standard checking/warning amber color.
     */
    public static final Color COLOR_CHECKING = new Color(217, 119, 6);
    public static final Color COLOR_CHECKING_BORDER = new Color(253, 230, 138);

    /**
     * Shields.io badge background colors.
     */
    public static final Color COLOR_SHIELD_LEFT_BG = new Color(85, 85, 85);
    public static final Color COLOR_SHIELD_RIGHT_BLUE = new Color(0, 126, 198);

    /**
     * Private constructor to prevent instantiation.
     */
    private AnahataUcIcons() {
    }

    /**
     * Creates an authentic, pixel-perfect two-tone shields.io badge icon.
     *
     * @param leftText The left label (e.g. "maven-central").
     * @param rightText The right version text (e.g. "v1.1.4" or "v1.1.4.300").
     * @param rightBgColor The background color for the right side.
     * @return The configured {@link Icon}.
     */
    public static Icon createShieldBadgeIcon(String leftText, String rightText, Color rightBgColor) {
        return new Icon() {
            private final Font font = new Font(Font.SANS_SERIF, Font.BOLD, 10);
            private final int height = 20;

            @Override
            public void paintIcon(Component c, Graphics g, int x, int y) {
                Graphics2D g2 = (Graphics2D) g.create();
                try {
                    g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
                    g2.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON);

                    g2.setFont(font);
                    FontMetrics fm = g2.getFontMetrics();

                    int leftPad = 6;
                    int rightPad = 6;
                    int leftWidth = fm.stringWidth(leftText) + (leftPad * 2);
                    int rightWidth = fm.stringWidth(rightText) + (rightPad * 2);
                    int totalWidth = leftWidth + rightWidth;

                    // Clip outer shape to rounded rectangle
                    RoundRectangle2D outerClip = new RoundRectangle2D.Float(x, y, totalWidth, height, 6, 6);
                    g2.clip(outerClip);

                    // Left background
                    g2.setColor(COLOR_SHIELD_LEFT_BG);
                    g2.fillRect(x, y, leftWidth, height);

                    // Right background
                    g2.setColor(rightBgColor);
                    g2.fillRect(x + leftWidth, y, rightWidth, height);

                    // Text vertical baseline calculation
                    int textY = y + ((height - fm.getHeight()) / 2) + fm.getAscent();

                    // Left text with subtle shadow
                    g2.setColor(new Color(0, 0, 0, 110));
                    g2.drawString(leftText, x + leftPad, textY + 1);
                    g2.setColor(Color.WHITE);
                    g2.drawString(leftText, x + leftPad, textY);

                    // Right text with subtle shadow
                    g2.setColor(new Color(0, 0, 0, 110));
                    g2.drawString(rightText, x + leftWidth + rightPad, textY + 1);
                    g2.setColor(Color.WHITE);
                    g2.drawString(rightText, x + leftWidth + rightPad, textY);

                } finally {
                    g2.dispose();
                }
            }

            @Override
            public int getIconWidth() {
                // Compute font width on dummy graphics or approximate
                FontMetrics fm = java.awt.Toolkit.getDefaultToolkit().getFontMetrics(font);
                int leftWidth = (fm != null ? fm.stringWidth(leftText) : leftText.length() * 6) + 12;
                int rightWidth = (fm != null ? fm.stringWidth(rightText) : rightText.length() * 6) + 12;
                return leftWidth + rightWidth;
            }

            @Override
            public int getIconHeight() {
                return height;
            }
        };
    }

    /**
     * Creates a circular anti-aliased status dot icon.
     *
     * @param fillColor The primary fill color.
     * @param borderColor The subtle outer halo/border color.
     * @param size The diameter of the icon in pixels.
     * @return The configured {@link Icon}.
     */
    public static Icon createStatusDotIcon(Color fillColor, Color borderColor, int size) {
        return new Icon() {
            @Override
            public void paintIcon(Component c, Graphics g, int x, int y) {
                Graphics2D g2 = (Graphics2D) g.create();
                try {
                    g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

                    // Outer subtle border
                    g2.setColor(borderColor);
                    g2.fill(new Ellipse2D.Float(x, y, size, size));

                    // Inner solid dot
                    int innerSize = Math.max(size - 4, 4);
                    int offset = (size - innerSize) / 2;
                    g2.setColor(fillColor);
                    g2.fill(new Ellipse2D.Float(x + offset, y + offset, innerSize, innerSize));
                } finally {
                    g2.dispose();
                }
            }

            @Override
            public int getIconWidth() {
                return size;
            }

            @Override
            public int getIconHeight() {
                return size;
            }
        };
    }

    /**
     * Creates an anti-aliased "Enabled" checkmark badge icon.
     *
     * @return A 16x16 icon showing a green rounded rectangle with a crisp white checkmark.
     */
    public static Icon createEnabledIcon() {
        return new Icon() {
            @Override
            public void paintIcon(Component c, Graphics g, int x, int y) {
                Graphics2D g2 = (Graphics2D) g.create();
                try {
                    g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
                    g2.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_PURE);

                    // Green rounded badge
                    g2.setColor(new Color(22, 163, 74));
                    g2.fill(new RoundRectangle2D.Float(x + 1, y + 1, 14, 14, 4, 4));

                    // Crisp white checkmark
                    g2.setColor(Color.WHITE);
                    g2.setStroke(new BasicStroke(2.0f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND));
                    Path2D check = new Path2D.Float();
                    check.moveTo(x + 4.5f, y + 8.0f);
                    check.lineTo(x + 7.0f, y + 11.0f);
                    check.lineTo(x + 11.5f, y + 5.0f);
                    g2.draw(check);
                } finally {
                    g2.dispose();
                }
            }

            @Override
            public int getIconWidth() {
                return 16;
            }

            @Override
            public int getIconHeight() {
                return 16;
            }
        };
    }

    /**
     * Creates an anti-aliased "Disabled" badge icon.
     *
     * @return A 16x16 icon showing a slate/gray rounded rectangle with a horizontal dash.
     */
    public static Icon createDisabledIcon() {
        return new Icon() {
            @Override
            public void paintIcon(Component c, Graphics g, int x, int y) {
                Graphics2D g2 = (Graphics2D) g.create();
                try {
                    g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

                    // Muted gray rounded badge
                    g2.setColor(new Color(148, 163, 184));
                    g2.fill(new RoundRectangle2D.Float(x + 1, y + 1, 14, 14, 4, 4));

                    // White horizontal dash
                    g2.setColor(Color.WHITE);
                    g2.setStroke(new BasicStroke(2.0f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND));
                    g2.drawLine(x + 5, y + 8, x + 11, y + 8);
                } finally {
                    g2.dispose();
                }
            }

            @Override
            public int getIconWidth() {
                return 16;
            }

            @Override
            public int getIconHeight() {
                return 16;
            }
        };
    }

    /**
     * Creates an anti-aliased "Update / Upgrade" action icon.
     *
     * @return A 16x16 icon depicting an upward upgrade arrow inside a styled badge.
     */
    public static Icon createUpdateActionIcon() {
        return new Icon() {
            @Override
            public void paintIcon(Component c, Graphics g, int x, int y) {
                Graphics2D g2 = (Graphics2D) g.create();
                try {
                    g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

                    // Badge
                    g2.setColor(new Color(37, 99, 235));
                    g2.fill(new RoundRectangle2D.Float(x + 1, y + 1, 14, 14, 4, 4));

                    // Upward arrow
                    g2.setColor(Color.WHITE);
                    g2.setStroke(new BasicStroke(1.8f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND));
                    Path2D arrow = new Path2D.Float();
                    arrow.moveTo(x + 4.5f, y + 7.5f);
                    arrow.lineTo(x + 8.0f, y + 4.0f);
                    arrow.lineTo(x + 11.5f, y + 7.5f);
                    g2.draw(arrow);
                    g2.drawLine(x + 8, y + 5, x + 8, y + 11);
                } finally {
                    g2.dispose();
                }
            }

            @Override
            public int getIconWidth() {
                return 16;
            }

            @Override
            public int getIconHeight() {
                return 16;
            }
        };
    }

    /**
     * Creates a high-fidelity refresh/check-for-updates action icon.
     *
     * @return A 16x16 icon showing circular refresh arrows.
     */
    public static Icon createRefreshActionIcon() {
        // Try NetBeans native refresh icon first
        ImageIcon nbIcon = ImageUtilities.loadImageIcon("org/netbeans/modules/autoupdate/ui/resources/refresh.png", true);
        if (nbIcon != null) {
            return nbIcon;
        }

        return new Icon() {
            @Override
            public void paintIcon(Component c, Graphics g, int x, int y) {
                Graphics2D g2 = (Graphics2D) g.create();
                try {
                    g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
                    g2.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_PURE);

                    g2.setColor(new Color(2, 132, 199));
                    g2.setStroke(new BasicStroke(2.0f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND));

                    // Top-right clockwise arc
                    g2.drawArc(x + 2, y + 2, 12, 12, 35, 130);
                    // Bottom-left clockwise arc
                    g2.drawArc(x + 2, y + 2, 12, 12, 215, 130);

                    // Top arrowhead
                    Path2D topHead = new Path2D.Float();
                    topHead.moveTo(x + 11, y + 2);
                    topHead.lineTo(x + 14, y + 4.5f);
                    topHead.lineTo(x + 11, y + 7);
                    topHead.closePath();
                    g2.fill(topHead);

                    // Bottom arrowhead
                    Path2D botHead = new Path2D.Float();
                    botHead.moveTo(x + 5, y + 14);
                    botHead.lineTo(x + 2, y + 11.5f);
                    botHead.lineTo(x + 5, y + 9);
                    botHead.closePath();
                    g2.fill(botHead);
                } finally {
                    g2.dispose();
                }
            }

            @Override
            public int getIconWidth() {
                return 16;
            }

            @Override
            public int getIconHeight() {
                return 16;
            }
        };
    }

    /**
     * Creates a high-fidelity NetBeans Plugins Manager action icon.
     *
     * @return A 16x16 icon representing NetBeans plugins.
     */
    public static Icon createPluginsManagerIcon() {
        // Try NetBeans native plugins icon first
        ImageIcon nbIcon = ImageUtilities.loadImageIcon("org/netbeans/modules/autoupdate/ui/resources/plugin.png", true);
        if (nbIcon == null) {
            nbIcon = ImageUtilities.loadImageIcon("org/netbeans/modules/autoupdate/ui/resources/update.png", true);
        }
        if (nbIcon != null) {
            return nbIcon;
        }

        return new Icon() {
            @Override
            public void paintIcon(Component c, Graphics g, int x, int y) {
                Graphics2D g2 = (Graphics2D) g.create();
                try {
                    g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

                    // Solid puzzle piece / module icon
                    g2.setColor(new Color(51, 65, 85));
                    Path2D puzzle = new Path2D.Float();
                    puzzle.moveTo(x + 2, y + 4);
                    // Top tab
                    puzzle.lineTo(x + 6, y + 4);
                    puzzle.curveTo(x + 6, y + 2, x + 10, y + 2, x + 10, y + 4);
                    puzzle.lineTo(x + 14, y + 4);
                    // Right edge
                    puzzle.lineTo(x + 14, y + 7);
                    puzzle.curveTo(x + 16, y + 7, x + 16, y + 11, x + 14, y + 11);
                    puzzle.lineTo(x + 14, y + 14);
                    // Bottom edge
                    puzzle.lineTo(x + 2, y + 14);
                    puzzle.closePath();
                    g2.fill(puzzle);

                    // Inner detail highlight
                    g2.setColor(new Color(96, 165, 250));
                    g2.fill(new RoundRectangle2D.Float(x + 4, y + 6, 4, 4, 1, 1));
                } finally {
                    g2.dispose();
                }
            }

            @Override
            public int getIconWidth() {
                return 16;
            }

            @Override
            public int getIconHeight() {
                return 16;
            }
        };
    }
}
