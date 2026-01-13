/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Fora Bara!
 */
package uno.anahata.ai.swing.internal;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.Window;
import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.fife.ui.rsyntaxtextarea.RSyntaxTextArea;
import org.fife.ui.rsyntaxtextarea.SyntaxConstants;
import org.fife.ui.rtextarea.RTextScrollPane;

/**
 * A custom modal dialog for displaying exceptions with a properly formatted
 * and indented stack trace.
 * 
 * @author anahata
 */
public class ExceptionDialog extends JDialog {

    /**
     * Constructs a new ExceptionDialog.
     * 
     * @param owner The owner window.
     * @param taskName The name of the task that failed.
     * @param description a brief description of the error.
     * @param throwable The exception to display.
     */
    public ExceptionDialog(Window owner, String taskName, String description, Throwable throwable) {
        super(owner, "Error: " + taskName, ModalityType.APPLICATION_MODAL);
        initComponents(taskName, description, throwable);
    }

    private void initComponents(String taskName, String description, Throwable throwable) {
        setLayout(new BorderLayout(10, 10));
        
        // Header Panel
        JPanel headerPanel = new JPanel(new BorderLayout(10, 5));
        headerPanel.setBorder(BorderFactory.createEmptyBorder(15, 15, 5, 15));
        
        JLabel titleLabel = new JLabel(taskName);
        titleLabel.setFont(titleLabel.getFont().deriveFont(Font.BOLD, 14f));
        titleLabel.setForeground(Color.RED.darker());
        headerPanel.add(titleLabel, BorderLayout.NORTH);
        
        JLabel descLabel = new JLabel("<html><body style='width: 400px;'>" + description + "</body></html>");
        headerPanel.add(descLabel, BorderLayout.CENTER);
        
        add(headerPanel, BorderLayout.NORTH);
        
        // Stack Trace Panel
        RSyntaxTextArea textArea = new RSyntaxTextArea();
        textArea.setSyntaxEditingStyle(SyntaxConstants.SYNTAX_STYLE_NONE);
        textArea.setEditable(false);
        textArea.setTabSize(4);
        textArea.setFont(new Font(Font.MONOSPACED, Font.PLAIN, 12));
        
        String stackTrace = ExceptionUtils.getStackTrace(throwable);
        textArea.setText(stackTrace);
        textArea.setCaretPosition(0);
        
        RTextScrollPane scrollPane = new RTextScrollPane(textArea);
        scrollPane.setBorder(BorderFactory.createTitledBorder("Stack Trace"));
        
        JPanel centerPanel = new JPanel(new BorderLayout());
        centerPanel.setBorder(BorderFactory.createEmptyBorder(0, 15, 0, 15));
        centerPanel.add(scrollPane, BorderLayout.CENTER);
        
        add(centerPanel, BorderLayout.CENTER);
        
        // Button Panel
        JPanel buttonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));
        buttonPanel.setBorder(BorderFactory.createEmptyBorder(5, 15, 15, 15));
        
        JButton copyButton = new JButton("Copy to Clipboard");
        copyButton.addActionListener(e -> SwingUtils.copyToClipboard(stackTrace));
        buttonPanel.add(copyButton);
        
        JButton closeButton = new JButton("Close");
        closeButton.addActionListener(e -> dispose());
        buttonPanel.add(closeButton);
        
        add(buttonPanel, BorderLayout.SOUTH);
        
        setPreferredSize(new Dimension(800, 600));
        pack();
        setLocationRelativeTo(getOwner());
    }

    /**
     * Static utility method to show the exception dialog.
     * 
     * @param component The relative component.
     * @param taskName The task name.
     * @param description The description.
     * @param throwable The exception.
     */
    public static void show(Component component, String taskName, String description, Throwable throwable) {
        SwingUtilities.invokeLater(() -> {
            Window owner = component != null ? SwingUtilities.getWindowAncestor(component) : null;
            ExceptionDialog dialog = new ExceptionDialog(owner, taskName, description, throwable);
            dialog.setVisible(true);
        });
    }
}
