/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.provider;

import java.awt.Component;
import java.awt.FlowLayout;
import java.awt.Font;
import java.util.List;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTable;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.TableCellRenderer;
import uno.anahata.asi.agi.provider.ResponseModality;
import uno.anahata.asi.swing.icons.FramesIcon;
import uno.anahata.asi.swing.icons.MicrophoneIcon;
import uno.anahata.asi.swing.icons.ScreenshotIcon;

/**
 * A specialized table cell renderer for displaying {@link ResponseModality} sets as graphical icons.
 * <p>
 * Renders icons for TEXT, IMAGE, AUDIO, and VIDEO modalities, providing clear visual feedback
 * on model generative capabilities in table views.
 * </p>
 * 
 * @author anahata
 */
public class ResponseModalitiesRenderer implements TableCellRenderer {

    /** Default renderer used for background and selection colors. */
    private final DefaultTableCellRenderer defaultRenderer = new DefaultTableCellRenderer();
    /** Container panel for laying out modality icons. */
    private final JPanel panel = new JPanel(new FlowLayout(FlowLayout.LEFT, 4, 0));

    /** Icon label for IMAGE modality. */
    private final JLabel imageLabel = new JLabel(new ScreenshotIcon(14));
    /** Icon label for AUDIO modality. */
    private final JLabel audioLabel = new JLabel(new MicrophoneIcon(14));
    /** Icon label for VIDEO modality. */
    private final JLabel videoLabel = new JLabel(new FramesIcon(14));

    /**
     * Constructs a new ResponseModalitiesRenderer.
     */
    public ResponseModalitiesRenderer() {
        panel.setOpaque(true);
        imageLabel.setToolTipText("IMAGE (Generates Images)");
        audioLabel.setToolTipText("AUDIO (Generates Audio/Speech)");
        videoLabel.setToolTipText("VIDEO (Generates Video)");
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int column) {
        Component c = defaultRenderer.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column);
        panel.setBackground(c.getBackground());
        panel.removeAll();

        if (value instanceof List<?> list) {
            StringBuilder tooltip = new StringBuilder("Supported Response Modalities: ");
            boolean first = true;
            for (Object obj : list) {
                if (obj instanceof ResponseModality modality) {
                    if (!first) {
                        tooltip.append(", ");
                    }
                    tooltip.append(modality.name());
                    first = false;

                    switch (modality) {
                        case TEXT -> {
                            JLabel lbl = new JLabel("TXT");
                            lbl.setFont(lbl.getFont().deriveFont(Font.BOLD, 10f));
                            lbl.setForeground(c.getForeground());
                            lbl.setToolTipText("TEXT (Generates Text/Code)");
                            panel.add(lbl);
                        }
                        case IMAGE -> panel.add(imageLabel);
                        case AUDIO -> panel.add(audioLabel);
                        case VIDEO -> panel.add(videoLabel);
                    }
                }
            }
            panel.setToolTipText(tooltip.toString());
        }
        return panel;
    }
}
