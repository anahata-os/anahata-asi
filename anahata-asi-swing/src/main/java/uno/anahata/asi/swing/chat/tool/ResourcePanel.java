/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.chat.tool;

import java.awt.BorderLayout;
import javax.swing.BorderFactory;
import javax.swing.JLabel;
import javax.swing.JPanel;
import uno.anahata.asi.model.resource.AbstractResource;
import uno.anahata.asi.swing.chat.ContextPanel;

/**
 * A panel that displays the details of an {@link AbstractResource}.
 *
 * @author anahata
 */
public class ResourcePanel extends JPanel {

    private final ContextPanel parentPanel;
    private final JLabel infoLabel;

    public ResourcePanel(ContextPanel parentPanel) {
        this.parentPanel = parentPanel;
        setLayout(new BorderLayout());
        setBorder(BorderFactory.createEmptyBorder(4, 4, 4, 4));

        infoLabel = new JLabel();
        infoLabel.setBorder(BorderFactory.createTitledBorder("Resource Details"));
        add(infoLabel, BorderLayout.NORTH);
    }

    public void setResource(AbstractResource res) {
        infoLabel.setText("<html>Name: " + res.getName() + "<br>ID: " + res.getId() + "<br>Position: " + res.getContextPosition() + "<br>Type: " + res.getContentType() + "</html>");
        revalidate();
        repaint();
    }
}
