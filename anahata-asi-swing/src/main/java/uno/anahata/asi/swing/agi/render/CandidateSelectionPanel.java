/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.agi.render;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.util.List;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import lombok.NonNull;
import uno.anahata.asi.model.core.AbstractModelMessage;
import uno.anahata.asi.swing.agi.AgiPanel;
import uno.anahata.asi.swing.internal.EdtPropertyChangeListener;

/**
 * A specialized panel for displaying multiple response candidates side-by-side.
 * It is positioned between the conversation history and the input area.
 * It reactively updates when the agi's active candidates change.
 *
 * @author anahata
 */
public class CandidateSelectionPanel extends JPanel {

    private final AgiPanel agiPanel;
    private final JPanel cardsContainer;
    private final JScrollPane scrollPane;

    /**
     * Constructs a new CandidateSelectionPanel.
     *
     * @param agiPanel The parent agi panel.
     */
    public CandidateSelectionPanel(@NonNull AgiPanel agiPanel) {
        super(new BorderLayout());
        this.agiPanel = agiPanel;
        setOpaque(false);
        setVisible(false);
        setBorder(BorderFactory.createEmptyBorder(5, 10, 5, 10));

        // Use BoxLayout for horizontal, non-wrapping layout
        cardsContainer = new JPanel();
        cardsContainer.setLayout(new BoxLayout(cardsContainer, BoxLayout.X_AXIS));
        cardsContainer.setOpaque(false);

        scrollPane = new JScrollPane(cardsContainer);
        scrollPane.setBorder(null);
        scrollPane.setOpaque(false);
        scrollPane.getViewport().setOpaque(false);
        scrollPane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_NEVER);
        scrollPane.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        
        // Set a fixed height for the candidate strip.
        scrollPane.setPreferredSize(new Dimension(0, 480));

        add(scrollPane, BorderLayout.CENTER);

        // Reactive binding to the agi's active candidates.
        new EdtPropertyChangeListener(this, agiPanel.getAgi(), "activeCandidates", evt -> updateCandidates());
    }

    /**
     * Updates the UI to reflect the current list of active candidates.
     */
    private void updateCandidates() {
        List<AbstractModelMessage> candidates = agiPanel.getAgi().getActiveCandidates();
        cardsContainer.removeAll();
        
        if (candidates.isEmpty()) {
            setVisible(false);
        } else {
            for (AbstractModelMessage candidate : candidates) {
                cardsContainer.add(createCandidateCard(candidate));
                cardsContainer.add(Box.createRigidArea(new Dimension(15, 0))); // Spacing between cards
            }
            setVisible(true);
        }
        
        revalidate();
        repaint();
    }

    /**
     * Creates a "card" for a single candidate, containing its message panel and a selection button.
     *
     * @param candidate The candidate message.
     * @return The card panel.
     */
    private JPanel createCandidateCard(AbstractModelMessage candidate) {
        JPanel card = new JPanel(new BorderLayout(0, 10));
        card.setOpaque(false);
        // Fixed width for cards to ensure horizontal scrolling works correctly.
        card.setPreferredSize(new Dimension(550, 450));
        card.setMaximumSize(new Dimension(550, 450));
        card.setBorder(BorderFactory.createCompoundBorder(
                BorderFactory.createLineBorder(new Color(180, 180, 180), 1, true),
                BorderFactory.createEmptyBorder(10, 10, 10, 10)
        ));

        // Use the standard ModelMessagePanel for rendering the candidate's content.
        ModelMessagePanel messagePanel = new ModelMessagePanel(agiPanel, candidate);
        card.add(messagePanel, BorderLayout.CENTER);

        // Add a prominent selection button at the bottom of the card.
        JButton selectButton = new JButton("Select this Candidate");
        selectButton.setFont(selectButton.getFont().deriveFont(Font.BOLD, 13f));
        selectButton.setFocusPainted(false);
        selectButton.addActionListener(e -> agiPanel.getAgi().chooseCandidate(candidate));
        card.add(selectButton, BorderLayout.SOUTH);

        return card;
    }
}
