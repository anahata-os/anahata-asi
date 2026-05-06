/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.agi.message.part;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Font;
import javax.swing.BorderFactory;
import javax.swing.JLabel;
import javax.swing.JPanel;
import lombok.NonNull;
import uno.anahata.asi.agi.message.ModelCodeExecutionResultPart;
import uno.anahata.asi.swing.agi.AgiPanel;
import uno.anahata.asi.swing.agi.message.part.text.CodeBlockSegmentRenderer;

/**
 * A specialized panel for rendering {@link ModelCodeExecutionResultPart} instances.
 * <p>
 * It displays the logs and textual output from a hosted code execution 
 * in a terminal-like format.
 * </p>
 * 
 * @author anahata
 */
public class ModelCodeExecutionResultPartPanel extends AbstractPartPanel<ModelCodeExecutionResultPart> {

    /** 
     * The internal renderer used to display the execution logs with terminal-style framing. 
     */
    private CodeBlockSegmentRenderer renderer;

    /**
     * Constructs a new ModelCodeOutputPartPanel.
     * 
     * @param agiPanel The parent agi panel.
     * @param part The output part containing logs or stdout/stderr.
     */
    public ModelCodeExecutionResultPartPanel(@NonNull AgiPanel agiPanel, @NonNull ModelCodeExecutionResultPart part) {
        super(agiPanel, part);
    }

    /** 
     * {@inheritDoc} 
     * <p>
     * Renders a terminal-themed header and uses a read-only code block 
     * (with "text" highlighting) to display the execution logs.
     * </p>
     */
    @Override
    protected void renderContent() {
        if (renderer == null) {
            getCentralContainer().removeAll();
            
            // 1. Terminal Header
            JPanel outputHeader = new JPanel(new BorderLayout());
            outputHeader.setOpaque(true);
            outputHeader.setBackground(new Color(43, 43, 43));
            outputHeader.setBorder(BorderFactory.createEmptyBorder(2, 8, 2, 8));
            
            JLabel label = new JLabel("EXECUTION LOGS");
            label.setFont(new Font("Monospaced", Font.BOLD, 10));
            label.setForeground(new Color(106, 135, 89)); // Terminal green
            outputHeader.add(label, BorderLayout.WEST);
            
            getCentralContainer().add(outputHeader);

            // 2. Output area (using CodeBlock with 'text' language for framing)
            renderer = new CodeBlockSegmentRenderer(agiPanel, part.getText(), "text");
            renderer.setHeaderVisible(false); // Clean terminal look
            renderer.setEditable(false);
            renderer.render();
            
            getCentralContainer().add(renderer.getComponent());
        } else {
            renderer.updateContent(part.getText());
            renderer.render();
        }
    }
}
