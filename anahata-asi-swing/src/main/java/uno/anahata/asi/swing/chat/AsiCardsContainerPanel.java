/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.chat;

import java.awt.BorderLayout;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import lombok.NonNull;
import uno.anahata.asi.AsiContainer;
import uno.anahata.asi.chat.Chat;
import uno.anahata.asi.swing.internal.WrapLayout;

/**
 * A container panel that displays active AI chat sessions as a collection of 
 * "sticky note" cards. This implementation is ideal for standalone applications
 * or sidebars where a more visual, dashboard-like overview is desired.
 * 
 * @author anahata
 */
public class AsiCardsContainerPanel extends AbstractAsiContainerPanel {

    private final JPanel cardContainer;
    private final Map<Chat, ChatCard> cachedCards = new HashMap<>();
    private Chat selectedChat;

    /**
     * Constructs a new cards container panel.
     * 
     * @param container The ASI container.
     */
    public AsiCardsContainerPanel(@NonNull AsiContainer container) {
        super(container);
        
        this.cardContainer = new JPanel(new WrapLayout(WrapLayout.LEFT, 10, 10));
        cardContainer.setOpaque(false);
        
        // Deselect when clicking on the background
        cardContainer.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                setSelectedChat(null);
            }
        });

        JScrollPane scrollPane = new JScrollPane(cardContainer);
        scrollPane.setBorder(null);
        add(scrollPane, BorderLayout.CENTER);
    }

    @Override
    protected void refreshView() {
        List<Chat> activeChats = asiContainer.getActiveChats();
        
        // 1. Remove cards for sessions no longer present
        cachedCards.keySet().removeIf(chat -> {
            if (!activeChats.contains(chat)) {
                ChatCard card = cachedCards.get(chat);
                card.cleanup();
                cardContainer.remove(card);
                if (chat == selectedChat) setSelectedChat(null);
                return true;
            }
            return false;
        });

        // 2. Add or update cards for current sessions
        for (int i = 0; i < activeChats.size(); i++) {
            Chat chat = activeChats.get(i);
            ChatCard card = cachedCards.get(chat);
            if (card == null) {
                card = new ChatCard(chat, this);
                card.addMouseListener(new MouseAdapter() {
                    @Override
                    public void mousePressed(MouseEvent e) {
                        setSelectedChat(chat);
                    }
                });
                cachedCards.put(chat, card);
            }
            
            if (i >= cardContainer.getComponentCount() || cardContainer.getComponent(i) != card) {
                cardContainer.add(card, i);
            }
        }

        // 3. Clean up trailing components
        while (cardContainer.getComponentCount() > activeChats.size()) {
            cardContainer.remove(cardContainer.getComponentCount() - 1);
        }

        cardContainer.revalidate();
        cardContainer.repaint();
    }

    private void setSelectedChat(Chat chat) {
        if (this.selectedChat != null) {
            ChatCard oldCard = cachedCards.get(this.selectedChat);
            if (oldCard != null) oldCard.setSelected(false);
        }
        this.selectedChat = chat;
        if (this.selectedChat != null) {
            ChatCard newCard = cachedCards.get(this.selectedChat);
            if (newCard != null) newCard.setSelected(true);
        }
        updateButtonState();
    }

    @Override
    protected Chat getSelectedChat() {
        return selectedChat;
    }
}
