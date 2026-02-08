/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.ui.diff;

import java.awt.BorderLayout;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import javax.swing.BorderFactory;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import net.miginfocom.swing.MigLayout;
import uno.anahata.asi.model.resource.TextReplacement;

/**
 * A scrollable list of {@link ReplacementCard}s.
 * 
 * @author anahata
 */
public class ReplacementListPanel extends JPanel {
    private final List<ReplacementCard> cards = new ArrayList<>();

    public ReplacementListPanel(List<TextReplacement> replacements, Runnable onUpdate) {
        setLayout(new BorderLayout());
        
        JPanel container = new JPanel(new MigLayout("wrap 1, fillx, insets 5", "[grow]"));
        for (TextReplacement r : replacements) {
            ReplacementCard card = new ReplacementCard(r, onUpdate);
            cards.add(card);
            container.add(card, "growx");
        }
        
        JScrollPane scroll = new JScrollPane(container);
        scroll.setBorder(BorderFactory.createEmptyBorder());
        scroll.getVerticalScrollBar().setUnitIncrement(16);
        add(scroll, BorderLayout.CENTER);
    }

    public List<TextReplacement> getSelectedReplacements() {
        return cards.stream()
                .filter(ReplacementCard::isSelected)
                .map(ReplacementCard::getReplacement)
                .collect(Collectors.toList());
    }
    
    public String getAggregatedComments() {
        return cards.stream()
                .filter(c -> !c.getComment().trim().isEmpty())
                .map(c -> "Change to '" + c.getReplacement().getTarget() + "': " + c.getComment().trim())
                .collect(Collectors.joining("\n"));
    }
}
