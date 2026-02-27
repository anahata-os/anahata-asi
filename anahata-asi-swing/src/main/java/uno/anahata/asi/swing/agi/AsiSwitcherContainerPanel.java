/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.agi;

import java.awt.BorderLayout;
import java.awt.CardLayout;
import javax.swing.JPanel;
import javax.swing.JToggleButton;
import javax.swing.JToolBar;
import lombok.NonNull;
import uno.anahata.asi.AsiContainer;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.swing.icons.CardsIcon;
import uno.anahata.asi.swing.icons.TableIcon;

/**
 * A container panel that allows switching between Table and Cards views.
 * It defaults to the Cards (Sticky Notes) view.
 * 
 * @author anahata
 */
public class AsiSwitcherContainerPanel extends JPanel {

    private final AsiTableContainerPanel tablePanel;
    private final AsiCardsContainerPanel cardsPanel;
    private final CardLayout viewLayout;
    private final JPanel mainView;
    private final JToggleButton viewToggle;
    private final JToolBar toolBar;

    public AsiSwitcherContainerPanel(@NonNull AsiContainer container) {
        setLayout(new BorderLayout());
        
        this.tablePanel = new AsiTableContainerPanel(container);
        this.cardsPanel = new AsiCardsContainerPanel(container);
        
        this.viewLayout = new CardLayout();
        this.mainView = new JPanel(viewLayout);
        
        mainView.add(tablePanel, "table");
        mainView.add(cardsPanel, "cards");
        
        // Setup Switcher Toolbar (Only Toggle)
        this.toolBar = new JToolBar();
        toolBar.setFloatable(false);
        
        viewToggle = new JToggleButton("Table", new TableIcon(16));
        viewToggle.setToolTipText("Toggle between Table and Sticky Notes view");
        viewToggle.addActionListener(e -> {
            boolean isCards = !viewToggle.isSelected();
            viewLayout.show(mainView, isCards ? "cards" : "table");
            viewToggle.setText(isCards ? "Table" : "Cards");
            viewToggle.setIcon(isCards ? new TableIcon(16) : new CardsIcon(16));
        });
        
        // Default to Cards view
        viewToggle.setSelected(false); 
        viewLayout.show(mainView, "cards");
        
        toolBar.add(viewToggle);
        
        add(toolBar, BorderLayout.NORTH);
        add(mainView, BorderLayout.CENTER);
    }

    public void setController(SessionController controller) {
        tablePanel.setController(controller);
        cardsPanel.setController(controller);
    }

    public void startRefresh() {
        tablePanel.startRefresh();
        cardsPanel.startRefresh();
    }

    public void stopRefresh() {
        tablePanel.stopRefresh();
        cardsPanel.stopRefresh();
    }

    public Agi getSelectedAgi() {
        if (viewToggle.isSelected()) { // Table view
            return tablePanel.getSelectedAgi();
        } else { // Cards view
            return cardsPanel.getSelectedAgi();
        }
    }
}
