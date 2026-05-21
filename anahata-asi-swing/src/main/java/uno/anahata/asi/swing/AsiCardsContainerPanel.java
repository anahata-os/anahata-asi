/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing;

import java.awt.BorderLayout;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import lombok.NonNull;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.swing.components.WrapLayout;
import uno.anahata.asi.swing.components.ScrollablePanel;

/**
 * A container panel that displays active AI agi sessions as a collection of 
 * "sticky note" cards. This implementation is ideal for standalone applications
 * or sidebars where a more visual, dashboard-like overview is desired.
 * 
 * @author anahata
 */
public class AsiCardsContainerPanel extends AbstractAsiContainerPanel {

    /**
     * The panel containing the collection of AgiCard components.
     */
    private final JPanel cardContainer;
    /**
     * Cache mapping AGI sessions to their corresponding AgiCard UI representations.
     */
    private final Map<Agi, AgiCard> cachedCards = new HashMap<>();
    /**
     * The currently selected AGI session within the cards collection.
     */
    private Agi selectedAgi;

    /**
     * Constructs a new cards container panel.
     * 
     * @param container The ASI container.
     */
    public AsiCardsContainerPanel(@NonNull AbstractSwingAsiContainer container) {
        super(container);
        
        // Cards view is a stable dashboard; hide lifecycle management from the toolbar
        closeButton.setVisible(false);
        disposeButton.setVisible(false);
        
        this.cardContainer = new ScrollablePanel();
        this.cardContainer.setLayout(new WrapLayout(WrapLayout.LEFT, 10, 10));
        cardContainer.setOpaque(false);
        
        // Deselect when clicking on the background
        cardContainer.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                setSelectedAgi(null);
            }
        });

        JScrollPane scrollPane = new JScrollPane(cardContainer);
        scrollPane.setBorder(null);
        add(scrollPane, BorderLayout.CENTER);
    }

    @Override
    protected void refreshView() {
        List<Agi> activeAgis = asiContainer.getActiveAgis();
        
        // 1. Remove cards for sessions no longer present
        cachedCards.keySet().removeIf(agi -> {
            if (!activeAgis.contains(agi)) {
                AgiCard card = cachedCards.get(agi);
                card.cleanup();
                cardContainer.remove(card);
                if (agi == selectedAgi) setSelectedAgi(null);
                return true;
            }
            return false;
        });

        // 2. Add or update cards for current sessions
        for (int i = 0; i < activeAgis.size(); i++) {
            Agi agi = activeAgis.get(i);
            AgiCard card = cachedCards.get(agi);
            if (card == null) {
                card = new AgiCard(agi, asiContainer);
                card.addMouseListener(new MouseAdapter() {
                    @Override
                    public void mousePressed(MouseEvent e) {
                        setSelectedAgi(agi);
                    }
                });
                cachedCards.put(agi, card);
            }
            
            if (i >= cardContainer.getComponentCount() || cardContainer.getComponent(i) != card) {
                cardContainer.add(card, i);
            }
        }

        // 3. Clean up trailing components
        while (cardContainer.getComponentCount() > activeAgis.size()) {
            cardContainer.remove(cardContainer.getComponentCount() - 1);
        }

        cardContainer.revalidate();
        cardContainer.repaint();
    }

    /**
     * Updates the currently selected AGI session and handles selection highlighting across cards.
     * @param agi The AGI session to select, or null to clear selection.
     */
    private void setSelectedAgi(Agi agi) {
        if (this.selectedAgi != null) {
            AgiCard oldCard = cachedCards.get(this.selectedAgi);
            if (oldCard != null) oldCard.setSelected(false);
        }
        this.selectedAgi = agi;
        if (this.selectedAgi != null) {
            AgiCard newCard = cachedCards.get(this.selectedAgi);
            if (newCard != null) newCard.setSelected(true);
        }
        updateButtonState();
    }

    @Override
    protected Agi getSelectedAgi() {
        return selectedAgi;
    }
}
