/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.ui.diff;

import java.awt.Font;
import javax.swing.BorderFactory;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import net.miginfocom.swing.MigLayout;
import org.jdesktop.swingx.prompt.PromptSupport;
import uno.anahata.asi.model.resource.TextReplacement;

/**
 * A UI component representing a single {@link TextReplacement} with a checkbox for cherry-picking
 * and a feedback area.
 * 
 * @author anahata
 */
public class ReplacementCard extends JPanel {
    private final JCheckBox checkBox = new JCheckBox();
    private final JTextArea commentArea = new JTextArea();
    private final TextReplacement replacement;

    public ReplacementCard(TextReplacement replacement, Runnable onToggle) {
        this.replacement = replacement;
        setLayout(new MigLayout("fillx, insets 5", "[][grow]", "[]0[]"));
        setBorder(BorderFactory.createEtchedBorder());
        
        checkBox.setSelected(true);
        checkBox.addActionListener(e -> onToggle.run());
        
        String targetPreview = escape(replacement.getTarget());
        JLabel summary = new JLabel("<html>Replace <b>" + targetPreview + "</b></html>");
        summary.setFont(new Font("SansSerif", Font.PLAIN, 11));
        summary.setToolTipText(replacement.getTarget());
        
        add(checkBox, "spany 2, top");
        add(summary, "growx, wrap");
        
        commentArea.setRows(2);
        commentArea.setFont(new Font("Monospaced", Font.PLAIN, 10));
        commentArea.setLineWrap(true);
        commentArea.setWrapStyleWord(true);
        PromptSupport.setPrompt("Feedback for this change...", commentArea);
        
        JScrollPane scroll = new JScrollPane(commentArea);
        scroll.setPreferredSize(new java.awt.Dimension(100, 40));
        add(scroll, "growx");
    }

    private String escape(String s) {
        if (s == null) return "";
        String escaped = s.replace("<", "&lt;").replace(">", "&gt;");
        if (escaped.length() > 60) {
            return escaped.substring(0, 60) + "...";
        }
        return escaped;
    }

    public boolean isSelected() {
        return checkBox.isSelected();
    }

    public TextReplacement getReplacement() {
        return replacement;
    }

    public String getComment() {
        return commentArea.getText();
    }
}
