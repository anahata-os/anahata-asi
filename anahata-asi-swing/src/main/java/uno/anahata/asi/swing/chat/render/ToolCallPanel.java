/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.chat.render;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.event.ActionListener;
import java.util.Map;
import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import lombok.NonNull;
import net.miginfocom.swing.MigLayout;
import org.jdesktop.swingx.JXTitledPanel;
import org.jdesktop.swingx.prompt.PromptSupport;
import uno.anahata.asi.internal.JacksonUtils;
import uno.anahata.asi.model.tool.AbstractTool;
import uno.anahata.asi.model.tool.AbstractToolCall;
import uno.anahata.asi.model.tool.AbstractToolParameter;
import uno.anahata.asi.model.tool.AbstractToolResponse;
import uno.anahata.asi.model.tool.ToolExecutionStatus;
import uno.anahata.asi.model.tool.ToolPermission;
import uno.anahata.asi.swing.chat.ChatPanel;
import uno.anahata.asi.swing.chat.SwingChatConfig;
import uno.anahata.asi.swing.chat.SwingChatConfig.UITheme;
import uno.anahata.asi.swing.components.CodeHyperlink;
import uno.anahata.asi.swing.icons.DeleteIcon;
import uno.anahata.asi.swing.icons.IconUtils;
import uno.anahata.asi.swing.icons.RunIcon;
import uno.anahata.asi.swing.internal.AnyChangeDocumentListener;
import uno.anahata.asi.swing.internal.EdtPropertyChangeListener;
import uno.anahata.asi.swing.internal.SwingUtils;

/**
 * A specialized panel for rendering an {@link AbstractToolCall} and its associated
 * {@link AbstractToolResponse} within a model message. It provides a vertical
 * layout where arguments are followed by a titled response area.
 * 
 * @author anahata
 */
public class ToolCallPanel extends AbstractPartPanel<AbstractToolCall<?, ?>> {

    private JPanel argsPanel;
    
    private JXTitledPanel responseTitledPanel;
    private JTabbedPane resultsTabbedPane;
    private JScrollPane outputScrollPane;
    private JScrollPane errorScrollPane;
    private JScrollPane logsScrollPane;
    
    private JTextArea outputArea;
    private JTextArea errorArea;
    private JTextArea logsArea;
    private ToolResponseAttachmentsPanel attachmentsPanel;
    
    private JTextField feedbackField;
    private CodeHyperlink jsonLink;
    
    private JComboBox<ToolPermission> permissionCombo;
    private JComboBox<ToolExecutionStatus> statusCombo;
    private JButton runButton;
    private JButton revertButton;
    private JProgressBar toolProgressBar;

    public ToolCallPanel(@NonNull ChatPanel chatPanel, @NonNull AbstractToolCall<?, ?> part) {
        super(chatPanel, part);
        // Listen to both the call and its response for state changes
        new EdtPropertyChangeListener(this, part, null, evt -> render());
        new EdtPropertyChangeListener(this, part.getResponse(), null, evt -> render());
    }

    @Override
    protected void renderContent() {
        if (argsPanel == null) {
            initComponents();
        }
        
        AbstractToolCall<?, ?> call = getPart();
        AbstractToolResponse<?> response = call.getResponse();

        // 1. Update Arguments
        renderArguments(call);

        // 2. Update Results/Logs/Errors
        renderResults(response);

        // 3. Update Controls (Bottom Bar)
        updateControls(call, response);
        
        updateHeaderInfoText();
    }

    private void initComponents() {
        getCentralContainer().setLayout(new MigLayout("fillx, insets 0", "[grow]", "[]0[]0[]"));

        // --- Arguments Panel (Top) ---
        argsPanel = new JPanel(new MigLayout("fillx, insets 5", "[][grow]"));
        argsPanel.setOpaque(false);
        getCentralContainer().add(argsPanel, "growx, wrap");
        
        // --- Response Panel (Middle) ---
        resultsTabbedPane = new JTabbedPane();
        
        UITheme theme = chatConfig.getTheme();
        outputArea = createTextArea(theme.getToolOutputFg(), theme.getToolOutputBg());
        errorArea = createTextArea(theme.getToolErrorFg(), theme.getToolErrorBg());
        logsArea = createTextArea(theme.getToolLogsFg(), theme.getToolLogsBg());
        attachmentsPanel = new ToolResponseAttachmentsPanel(chatPanel);

        outputScrollPane = new JScrollPane(outputArea);
        outputScrollPane.addMouseWheelListener(e -> SwingUtils.redispatchMouseWheelEvent(outputScrollPane, e));

        errorScrollPane = new JScrollPane(errorArea);
        errorScrollPane.addMouseWheelListener(e -> SwingUtils.redispatchMouseWheelEvent(errorScrollPane, e));

        logsScrollPane = new JScrollPane(logsArea);
        logsScrollPane.addMouseWheelListener(e -> SwingUtils.redispatchMouseWheelEvent(logsScrollPane, e));

        responseTitledPanel = new JXTitledPanel("Response");
        responseTitledPanel.setTitleFont(new Font("SansSerif", Font.BOLD, 11));
        responseTitledPanel.setTitleForeground(new Color(100, 100, 100));
        responseTitledPanel.setContentContainer(resultsTabbedPane);
        responseTitledPanel.setOpaque(false);
        responseTitledPanel.setBorder(BorderFactory.createMatteBorder(1, 0, 0, 0, Color.LIGHT_GRAY));
        
        // Cap the response area at 500px height
        getCentralContainer().add(responseTitledPanel, "growx, hmax 500, wrap");

        // --- Bottom Control Bar ---
        JPanel controlBar = new JPanel(new MigLayout("fillx, insets 5", "[][grow][]", "[][]"));
        controlBar.setOpaque(false);
        controlBar.setBorder(BorderFactory.createMatteBorder(1, 0, 0, 0, Color.LIGHT_GRAY));

        // Row 1: Permission (Left) and Feedback (Right, Large)
        permissionCombo = new JComboBox<>(new ToolPermission[]{
            ToolPermission.PROMPT, ToolPermission.APPROVE_ALWAYS, ToolPermission.DENY_NEVER
        });
        permissionCombo.setRenderer(new ToolPermissionRenderer());
        permissionCombo.addActionListener(e -> {
            ToolPermission tp = (ToolPermission) permissionCombo.getSelectedItem();
            getPart().getTool().setPermission(tp);
            permissionCombo.setForeground(SwingChatConfig.getColor(tp));
        });

        feedbackField = new JTextField();
        PromptSupport.setPrompt("Your comments to the model regarding this tool call", feedbackField);
        PromptSupport.setFocusBehavior(PromptSupport.FocusBehavior.HIDE_PROMPT, feedbackField);
        feedbackField.getDocument().addDocumentListener(new AnyChangeDocumentListener(() -> {
            getPart().getResponse().setUserFeedback(feedbackField.getText());
        }));

        controlBar.add(new JLabel("Permission:"), "split 2");
        controlBar.add(permissionCombo);
        controlBar.add(feedbackField, "growx, pushx, span 2, wrap");

        // Row 2: Status and Run (Right)
        statusCombo = new JComboBox<>(ToolExecutionStatus.values());
        statusCombo.setRenderer(new ToolExecutionStatusRenderer());
        statusCombo.addActionListener(e -> {
            ToolExecutionStatus status = (ToolExecutionStatus) statusCombo.getSelectedItem();
            getPart().getResponse().setStatus(status);
            statusCombo.setForeground(SwingChatConfig.getColor(status));
        });

        revertButton = new JButton("Revert", new DeleteIcon(16));
        revertButton.setToolTipText("Clear execution results and restore status to NOT_EXECUTED");
        revertButton.addActionListener(e -> getPart().getResponse().reset());

        runButton = new JButton("Run", new RunIcon(16));
        
        toolProgressBar = new JProgressBar();
        toolProgressBar.setIndeterminate(true);
        toolProgressBar.setPreferredSize(new Dimension(100, 16));
        toolProgressBar.setVisible(false);

        jsonLink = new CodeHyperlink("json", 
                () -> "Tool Response: " + getPart().getToolName(), 
                () -> JacksonUtils.prettyPrint(getPart().getResponse()), 
                "json");

        controlBar.add(new JLabel("Status:"), "split 2");
        controlBar.add(statusCombo);
        controlBar.add(toolProgressBar, "gapleft 10");
        controlBar.add(revertButton, "right, skip 1, split 2");
        controlBar.add(runButton, "right, wrap");
        controlBar.add(jsonLink, "cell 2 1, right");

        getCentralContainer().add(controlBar, "growx");
    }

    private void renderArguments(AbstractToolCall<?, ?> call) {
        argsPanel.removeAll();
        AbstractTool<?, ?> tool = call.getTool();
        Map<String, Object> args = call.getArgs();
        
        int row = 0; 
        for (Map.Entry<String, Object> entry : args.entrySet()) {
            String paramName = entry.getKey();
            Object value = entry.getValue();
            
            String rendererId = tool.getParameters().stream()
                    .filter(p -> p.getName().equals(paramName))
                    .map(AbstractToolParameter::getRendererId)
                    .findFirst()
                    .orElse("");

            String valStr = (value instanceof String s) ? s : JacksonUtils.prettyPrint(value);

            boolean isLargeArgument = (rendererId != null && !rendererId.isEmpty()) || 
                                      (valStr.length() > 100 || valStr.contains("\n"));

            if (isLargeArgument) {
                argsPanel.add(new JLabel("<html><b>" + paramName + ":</b></html>"), "cell 0 " + row + ", span 2, wrap");
                row++; 
            } else {
                argsPanel.add(new JLabel("<html><b>" + paramName + ":</b></html>"), "cell 0 " + row); 
            }

            if (rendererId != null && !rendererId.isEmpty()) {
                AbstractCodeBlockSegmentRenderer renderer = AbstractCodeBlockSegmentRenderer.create(chatPanel, valStr, rendererId);
                renderer.render();
                argsPanel.add(renderer.getComponent(), "cell 1 " + row + ", growx, hmin 40, wrap"); 
            } else if (valStr.length() > 100 || valStr.contains("\n")) {
                JTextArea area = new JTextArea();
                area.setEditable(false);
                area.setLineWrap(true);
                area.setWrapStyleWord(true);
                area.setText(valStr);
                argsPanel.add(new JScrollPane(area), "cell 1 " + row + ", growx, hmin 40, wrap");
            } else {
                argsPanel.add(new JLabel(valStr), "cell 1 " + row + ", wrap"); 
            }
            row++; 
        }
        argsPanel.revalidate();
        argsPanel.repaint(); 
    }

    private void renderResults(AbstractToolResponse<?> response) {
        // 1. Output
        String output = response.getResult() != null ? response.getResult().toString() : "";
        updateTab(outputScrollPane, "Output", !output.isEmpty());
        outputArea.setText(output);

        // 2. Error
        String error = response.getError() != null ? response.getError() : "";
        updateTab(errorScrollPane, "Error", !error.isEmpty());
        errorArea.setText(error);
        
        // 3. Logs
        StringBuilder logsBuilder = new StringBuilder();
        for (String log : response.getLogs()) {
            logsBuilder.append("• ").append(log).append("\n");
        }
        String logs = logsBuilder.toString();
        updateTab(logsScrollPane, "Logs", !logs.isEmpty());
        logsArea.setText(logs);
        
        // 4. Attachments
        updateTab(attachmentsPanel, "Attachments", !response.getAttachments().isEmpty());
        if (!response.getAttachments().isEmpty()) {
            attachmentsPanel.render(response);
        }
        
        // Dynamic Visibility
        boolean hasResults = resultsTabbedPane.getTabCount() > 0;
        responseTitledPanel.setVisible(hasResults);
        
        if (hasResults) {
            // Tab Selection Logic: Prioritize Error on failure, then Attachments, then Output, then Logs.
            if (response.getStatus() == ToolExecutionStatus.FAILED && !error.isEmpty()) {
                resultsTabbedPane.setSelectedComponent(errorScrollPane);
            } else if (response.getStatus() == ToolExecutionStatus.EXECUTED) {
                if (!response.getAttachments().isEmpty()) {
                    resultsTabbedPane.setSelectedComponent(attachmentsPanel);
                } else if (!output.isEmpty()) {
                    resultsTabbedPane.setSelectedComponent(outputScrollPane);
                } else if (!logs.isEmpty()) {
                    resultsTabbedPane.setSelectedComponent(logsScrollPane);
                }
            }
            
            if (resultsTabbedPane.getSelectedIndex() == -1) {
                resultsTabbedPane.setSelectedIndex(0);
            }
        }
    }
    
    private void updateTab(java.awt.Component component, String title, boolean shouldBeVisible) {
        int index = resultsTabbedPane.indexOfComponent(component);
        if (shouldBeVisible && index == -1) {
            resultsTabbedPane.addTab(title, component);
        } else if (!shouldBeVisible && index != -1) {
            resultsTabbedPane.removeTabAt(index);
        }
    }

    private void updateControls(AbstractToolCall<?, ?> call, AbstractToolResponse<?> response) {
        permissionCombo.setSelectedItem(call.getTool().getPermission());
        statusCombo.setSelectedItem(response.getStatus());
        
        // Ensure initial colors are set
        permissionCombo.setForeground(SwingChatConfig.getColor(call.getTool().getPermission()));
        statusCombo.setForeground(SwingChatConfig.getColor(response.getStatus()));
        
        if (!feedbackField.getText().equals(response.getUserFeedback())) {
            feedbackField.setText(response.getUserFeedback());
        }
        
        // Remove all action listeners before adding new ones
        for (ActionListener al : runButton.getActionListeners()) {
            runButton.removeActionListener(al);
        }

        toolProgressBar.setVisible(response.getStatus() == ToolExecutionStatus.EXECUTING);

        if (response.getStatus() == ToolExecutionStatus.EXECUTING) {
            runButton.setText("Stop");
            runButton.setIcon(IconUtils.getIcon("delete.png", 16, 16));
            runButton.addActionListener(e -> response.stop());
            runButton.setEnabled(true);
            revertButton.setVisible(false);
        } else if (response.getStatus() == ToolExecutionStatus.EXECUTED) {
            runButton.setText("Run Again");
            runButton.setIcon(new RunIcon(16));
            runButton.addActionListener(e -> response.execute());
            runButton.setEnabled(true);
            revertButton.setVisible(true);
        } else if (response.getStatus() == ToolExecutionStatus.PENDING || response.getStatus() == ToolExecutionStatus.NOT_EXECUTED) {
            runButton.setText("Run");
            runButton.setIcon(new RunIcon(16));
            runButton.addActionListener(e -> response.execute());
            runButton.setEnabled(true);
            revertButton.setVisible(false);
        } else if (response.getStatus() == ToolExecutionStatus.FAILED) {
            runButton.setText("Retry");
            runButton.setIcon(new RunIcon(16));
            runButton.addActionListener(e -> response.execute());
            runButton.setEnabled(true);
            revertButton.setVisible(true);
        } else {
            runButton.setText("Executed");
            runButton.setIcon(new RunIcon(16));
            runButton.setEnabled(false);
            revertButton.setVisible(false);
        }
    }

    private JTextArea createTextArea(Color fg, Color bg) {
        JTextArea area = new JTextArea();
        area.setEditable(false);
        area.setLineWrap(true);
        area.setWrapStyleWord(true);
        area.setForeground(fg);
        if (bg != null) {
            area.setBackground(bg);
            area.setOpaque(true);
        } else {
            area.setOpaque(false);
        }
        area.setFont(chatConfig.getTheme().getMonoFont());
        return area;
    }

    @Override
    protected void updateHeaderInfoText() {
        AbstractToolCall<?, ?> call = getPart();
        AbstractToolResponse<?> response = call.getResponse();
        
        StringBuilder sb = new StringBuilder("<html>");
        sb.append("<b>Tool: </b>").append(call.getToolName());
        
        String statusText = response.getStatus() != null ? response.getStatus().name() : "";
        String color = SwingUtils.toHtmlColor(SwingChatConfig.getColor(response.getStatus()));
        
        sb.append(" <font color='").append(color).append("'>[").append(statusText).append("]</font>");

        Long executionTime = response.getExecutionTimeMillis();
        if (executionTime != null && executionTime > 0) {
            sb.append(" (").append(executionTime).append(" ms)");
        }
        
        sb.append("</html>");
        
        setTitle(sb.toString());
    }
}
