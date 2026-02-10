/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi.nb.util;

import java.util.List;
import javax.swing.DefaultListCellRenderer;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import org.openide.DialogDescriptor;
import org.openide.DialogDisplayer;
import uno.anahata.asi.chat.Chat;

/**
 * NetBeans-specific Swing utilities for the Anahata ASI.
 * 
 * @author anahata
 */
public class NbSwingUtils {
    
    /**
     * Shows a dialog to select one of the active chat sessions.
     * 
     * @param chats The list of active chats.
     * @return The selected chat, or null if cancelled or no chats available.
     */
    public static Chat showChatSelectionDialog(List<Chat> chats) {
        if (chats == null || chats.isEmpty()) {
            return null;
        }
        
        if (chats.size() == 1) {
            return chats.get(0);
        }

        JComboBox<Chat> comboBox = new JComboBox<>(chats.toArray(new Chat[0]));
        comboBox.setRenderer(new DefaultListCellRenderer() {
            @Override
            public java.awt.Component getListCellRendererComponent(javax.swing.JList<?> list, Object value, int index, boolean isSelected, boolean cellHasFocus) {
                super.getListCellRendererComponent(list, value, index, isSelected, cellHasFocus);
                if (value instanceof Chat chat) {
                    setText(chat.getDisplayName() + " (" + chat.getShortId() + ")");
                }
                return this;
            }
        });

        JPanel panel = new JPanel(new java.awt.BorderLayout(10, 10));
        panel.add(new JLabel("Select the chat session to add the resource to:"), java.awt.BorderLayout.NORTH);
        panel.add(comboBox, java.awt.BorderLayout.CENTER);

        DialogDescriptor dd = new DialogDescriptor(panel, "Select Chat Session");
        if (DialogDisplayer.getDefault().notify(dd) == DialogDescriptor.OK_OPTION) {
            return (Chat) comboBox.getSelectedItem();
        }
        return null;
    }
}
