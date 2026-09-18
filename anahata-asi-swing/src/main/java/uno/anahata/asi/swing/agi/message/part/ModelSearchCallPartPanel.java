/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.agi.message.part;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Font;
import java.util.stream.Collectors;
import javax.swing.BorderFactory;
import javax.swing.JLabel;
import javax.swing.JPanel;
import lombok.NonNull;
import uno.anahata.asi.agi.message.web.WebSearchCallPart;
import uno.anahata.asi.swing.agi.AgiPanel;
import uno.anahata.asi.swing.icons.SearchIcon;
import uno.anahata.asi.swing.icons.AsiIcons;

/**
 * A specialized panel for rendering {@link WebSearchCallPart} instances.
 * <p>
 * Displays the search queries initiated by the model with a clear "System Action"
 * visual style and a search icon.
 * </p>
 * 
 * @author anahata
 */
public class ModelSearchCallPartPanel extends AbstractPartPanel<WebSearchCallPart> {

    /**
     * Constructs a new ModelSearchCallPartPanel.
     * 
     * @param agiPanel The parent agi panel.
     * @param part The search call part to render.
     */
    public ModelSearchCallPartPanel(@NonNull AgiPanel agiPanel, @NonNull WebSearchCallPart part) {
        super(agiPanel, part);
    }

    /** 
     * {@inheritDoc} 
     * <p>
     * Renders a specialized header with a search icon and the list of queries 
     * being executed by the model.
     * </p>
     */
    @Override
    protected void renderContent() {
        getCentralContainer().removeAll();

        // 1. Create a "Searching" Header
        JPanel searchHeader = new JPanel(new BorderLayout());
        searchHeader.setOpaque(true);
        searchHeader.setBackground(new Color(43, 43, 43));
        searchHeader.setBorder(BorderFactory.createEmptyBorder(4, 10, 4, 10));

        String queries = part.getQueries().stream()
                .map(q -> "\"" + q + "\"")
                .collect(Collectors.joining(", "));
        
        JLabel label = new JLabel("SEARCHING WEB: " + queries);
        label.setFont(new Font("SansSerif", Font.BOLD, 11));
        label.setForeground(new Color(110, 156, 190)); // Search blue
        label.setIcon(AsiIcons.get(AsiIcons.Key.SEARCH, 14));
        label.setIconTextGap(8);
        
        searchHeader.add(label, BorderLayout.WEST);
        getCentralContainer().add(searchHeader);

        getCentralContainer().revalidate();
        getCentralContainer().repaint();
    }
}
