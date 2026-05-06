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
import uno.anahata.asi.agi.message.ModelCodeExecutionCallPart;
import uno.anahata.asi.swing.agi.AgiPanel;
import uno.anahata.asi.swing.agi.message.part.text.CodeBlockSegmentRenderer;

/**
 * A specialized panel for rendering {@link ModelCodeExecutionCallPart} instances.
 * <p>
 * It leverages the {@link CodeBlockSegmentRenderer} to provide high-fidelity 
 * code viewing (with syntax highlighting) and editing capabilities for model-generated 
 * execution requests.
 * </p>
 * 
 * @author anahata
 */
public class ModelCodeExecutionCallPartPanel extends AbstractPartPanel<ModelCodeExecutionCallPart> {

    /** 
     * The internal renderer responsible for code highlighting and interactive editing. 
     */
    private CodeBlockSegmentRenderer renderer;

    /**
     * Constructs a new ModelCodeCallPartPanel.
     * 
     * @param agiPanel The parent agi panel providing context and theme.
     * @param part The specific code call part to render.
     */
    public ModelCodeExecutionCallPartPanel(@NonNull AgiPanel agiPanel, @NonNull ModelCodeExecutionCallPart part) {
        super(agiPanel, part);
    }

    /** 
     * {@inheritDoc} 
     * <p>
     * Initializes a specialized terminal-like header indicating the execution 
     * language and wires up the {@link CodeBlockSegmentRenderer} for the source code.
     * </p>
     */
    @Override
    protected void renderContent() {
        if (renderer == null) {
            getCentralContainer().removeAll();
            
            // 1. Create a specialized header for the execution block
            JPanel callHeader = new JPanel(new BorderLayout());
            callHeader.setOpaque(true);
            callHeader.setBackground(new Color(60, 63, 65));
            callHeader.setBorder(BorderFactory.createEmptyBorder(2, 8, 2, 8));
            
            JLabel label = new JLabel("EXECUTING " + part.getLanguage().toUpperCase());
            label.setFont(new Font("Monospaced", Font.BOLD, 10));
            label.setForeground(new Color(187, 187, 187));
            callHeader.add(label, BorderLayout.WEST);
            
            getCentralContainer().add(callHeader);

            // 2. Initialize the CodeBlock renderer
            renderer = new CodeBlockSegmentRenderer(agiPanel, part.getText(), part.getLanguage());
            renderer.setHeaderVisible(true); // Shows language and copy button
            renderer.setEditable(true);
            renderer.setOnSave(newContent -> part.setText(newContent));
            renderer.render();
            
            getCentralContainer().add(renderer.getComponent());
        } else {
            renderer.updateContent(part.getText());
            renderer.render();
        }
    }
}
