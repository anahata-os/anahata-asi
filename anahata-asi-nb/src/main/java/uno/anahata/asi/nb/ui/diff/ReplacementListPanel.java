/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.ui.diff;

import java.awt.BorderLayout;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import net.miginfocom.swing.MigLayout;
import uno.anahata.asi.toolkit.files.TextReplacement;
import uno.anahata.asi.swing.icons.CancelIcon;

/**
 * A scrollable list of {@link ReplacementCard}s.
 * 
 * @author anahata
 */
public class ReplacementListPanel extends JPanel {
    private final List<ReplacementCard> cards = new ArrayList<>();

    /**
     * Constructs a ReplacementListPanel.
     * 
     * @param replacements The list of replacements to display.
     * @param validationErrors validation errors mapping.
     * @param onUpdate A callback to trigger when selection changes.
     */
    public ReplacementListPanel(List<TextReplacement> replacements, Map<TextReplacement, String> validationErrors, Runnable onUpdate) {
        setLayout(new BorderLayout());
        
        JPanel toolbar = new JPanel(new java.awt.FlowLayout(java.awt.FlowLayout.LEFT));
        JButton deselectAllBtn = new JButton("Deselect All", new CancelIcon(16));
        deselectAllBtn.setFont(new java.awt.Font("SansSerif", java.awt.Font.PLAIN, 10));
        deselectAllBtn.addActionListener(e -> {
            cards.forEach(c -> c.setSelected(false));
            onUpdate.run();
        });
        toolbar.add(deselectAllBtn);
        add(toolbar, BorderLayout.NORTH);
        
        JPanel container = new JPanel(new MigLayout("wrap 1, fillx, insets 5", "[grow]"));
        for (TextReplacement r : replacements) {
            ReplacementCard card = new ReplacementCard(r, validationErrors.get(r), onUpdate);
            cards.add(card);
            container.add(card, "growx");
        }
        
        JScrollPane scroll = new JScrollPane(container);
        scroll.setBorder(BorderFactory.createEmptyBorder());
        scroll.getVerticalScrollBar().setUnitIncrement(25);
        add(scroll, BorderLayout.CENTER);
    }

    /**
     * Gets the list of replacements currently selected in the UI.
     * 
     * @return The list of checked replacements.
     */
    public List<TextReplacement> getSelectedReplacements() {
        return cards.stream()
                .filter(ReplacementCard::isSelected)
                .map(ReplacementCard::getReplacement)
                .collect(Collectors.toList());
    }
    
    /**
     * Aggregates comments from all cards in this list.
     * 
     * @return A formatted string of comments.
     */
    public String getAggregatedComments() {
        return cards.stream()
                .filter(c -> !c.getComment().trim().isEmpty())
                .map(c -> "Change to '" + c.getReplacement().getTarget() + "': " + c.getComment().trim())
                .collect(Collectors.joining("\n"));
    }

    /**
     * Collects all screenshots from all cards in this list.
     * 
     * @return A list of PNG byte arrays.
     */
    public List<byte[]> getScreenshots() {
        List<byte[]> all = new ArrayList<>();
        for (ReplacementCard card : cards) {
            all.addAll(card.getScreenshots());
        }
        return all;
    }
}
