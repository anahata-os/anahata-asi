/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.chat.render;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.HashMap;
import java.util.List;

import java.util.Map;
import java.util.stream.Collectors;
import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.SwingUtilities;
import lombok.NonNull;
import net.miginfocom.swing.MigLayout;
import org.jdesktop.swingx.JXTitledPanel;
import org.jdesktop.swingx.prompt.PromptSupport;
import uno.anahata.asi.internal.JacksonUtils;
import uno.anahata.asi.internal.TextUtils;
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
import uno.anahata.asi.swing.icons.CancelIcon;
import uno.anahata.asi.swing.icons.DeleteIcon;
import uno.anahata.asi.swing.icons.IconUtils;
import uno.anahata.asi.swing.icons.RunIcon;
import uno.anahata.asi.swing.internal.AnyChangeDocumentListener;
import uno.anahata.asi.swing.internal.EdtPropertyChangeListener;
import uno.anahata.asi.swing.internal.SwingUtils;

/**
 * A specialized panel for rendering an {@link AbstractToolCall} and its associated
 * {@link AbstractToolResponse} within a model message. It provides a vertical
 * layout where arguments are presented in tabs (one for each parameter), 
 * followed by a titled, expandable response area.
 * 
 * @author anahata
 */
public class ToolCallPanel extends AbstractPartPanel<AbstractToolCall<?, ?>> {

    private JPanel argsContainer;
    private JTabbedPane argsTabbedPane;
    /** Cache of renderers for arguments to support incremental updates and editing. */
    private final Map<String, ParameterRenderer<?>> argRenderers = new HashMap<>();
    
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
    private JButton skipButton;
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
        if (argsContainer == null) {
            initComponents();
        }
        
        AbstractToolCall<?, ?> call = getPart();
        AbstractToolResponse<?> response = call.getResponse();

        // 1. Update Arguments
        renderArguments(call, response);

        // 2. Update Results/Logs/Errors
        renderResults(response);

        // 3. Update Controls (Bottom Bar)
        updateControls(call, response);
    }

    private void initComponents() {
        getCentralContainer().setLayout(new MigLayout("fillx, insets 0", "[grow]", "[]0[]0[]"));

        // --- Arguments Panel (Top) ---
        argsContainer = new JPanel(new BorderLayout());
        argsContainer.setOpaque(false);
        getCentralContainer().add(argsContainer, "push, grow, wrap");
        
        // --- Response Panel (Middle) ---
        resultsTabbedPane = new JTabbedPane();
        resultsTabbedPane.addChangeListener(e -> adjustTabbedPaneHeight(resultsTabbedPane));
        
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
        
        // Add expand/collapse logic to the response titled panel header
        if (responseTitledPanel.getComponentCount() > 0) {
            responseTitledPanel.getComponent(0).addMouseListener(new MouseAdapter() {
                @Override
                public void mouseClicked(MouseEvent e) {
                    resultsTabbedPane.setVisible(!resultsTabbedPane.isVisible());
                    responseTitledPanel.revalidate();
                    responseTitledPanel.repaint();
                }
            });
        }
        
        getCentralContainer().add(responseTitledPanel, "growx, wrap");

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

        skipButton = new JButton("Skip", new CancelIcon(16));
        skipButton.setToolTipText("Set status to NOT_EXECUTED");
        skipButton.addActionListener(e -> getPart().getResponse().setStatus(ToolExecutionStatus.NOT_EXECUTED));

        revertButton = new JButton("Clear response", new DeleteIcon(16));
        revertButton.setToolTipText("Clear execution results, erros and logs and sets the status to NOT_EXECUTED");
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
        controlBar.add(skipButton, "right, skip 1, split 3");
        controlBar.add(revertButton);
        controlBar.add(runButton, "right, wrap");
        
        JPanel jsonLinksPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT, 5, 0));
        jsonLinksPanel.setOpaque(false);
        jsonLinksPanel.add(jsonLink);
        controlBar.add(jsonLinksPanel, "cell 2 1, right");

        getCentralContainer().add(controlBar, "growx");
    }

    private void renderArguments(AbstractToolCall<?, ?> call, AbstractToolResponse<?> response) {
        Map<String, Object> effectiveArgs = call.getEffectiveArgs();
        AbstractTool<?, ?> tool = call.getTool();
        List<AbstractToolParameter<?>> parameters = (List) tool.getParameters();

        if (argsTabbedPane == null) {
            argsTabbedPane = new JTabbedPane();
            argsTabbedPane.addChangeListener(e -> adjustTabbedPaneHeight(argsTabbedPane));
            argsContainer.removeAll();
            argsContainer.add(argsTabbedPane, BorderLayout.CENTER);
        }

        // 1. Update/Create Renderers for all parameters
        for (AbstractToolParameter<?> param : parameters) {
            String paramName = param.getName();
            Object value = effectiveArgs.get(paramName);
            
            ParameterRenderer renderer = argRenderers.get(paramName);

            if (renderer == null) {
                String rendererId = param.getRendererId();
                renderer = ParameterRendererFactory.create(chatPanel, call, paramName, value, rendererId);
                renderer.render();
                
                argRenderers.put(paramName, renderer);
                // Trigger initial height adjustment
                SwingUtilities.invokeLater(() -> adjustTabbedPaneHeight(argsTabbedPane));
            } else {
                // Update existing
                renderer.updateContent(value);
                renderer.render();
            }

            // 2. Sync Tab
            JComponent comp = renderer.getComponent();
            int tabIndex = argsTabbedPane.indexOfComponent(comp);
            if (tabIndex == -1) {
                argsTabbedPane.addTab(paramName, comp);
                tabIndex = argsTabbedPane.indexOfComponent(comp);
            }
            
            // Visual feedback for modified arguments
            if (call.getModifiedArgs().containsKey(paramName)) {
                argsTabbedPane.setForegroundAt(tabIndex, Color.BLUE);
                argsTabbedPane.setTitleAt(tabIndex, paramName + "*");
            } else if (value == null) {
                argsTabbedPane.setForegroundAt(tabIndex, Color.LIGHT_GRAY);
                argsTabbedPane.setTitleAt(tabIndex, paramName);
            } else {
                argsTabbedPane.setForegroundAt(tabIndex, null);
                argsTabbedPane.setTitleAt(tabIndex, paramName);
            }
        }
        
        argsContainer.setVisible(!parameters.isEmpty());
        if (argsTabbedPane != null && argsTabbedPane.getTabCount() > 0) {
            adjustTabbedPaneHeight(argsTabbedPane);
        }
    }

    private void renderResults(AbstractToolResponse<?> response) {
        // 1. Prepare content
        String output = response.getResult() != null ? response.getResult().toString() : "";
        boolean hasOutput = !output.isEmpty();
        if (hasOutput) outputArea.setText(output);

        boolean hasAttachments = !response.getAttachments().isEmpty();
        if (hasAttachments) attachmentsPanel.render(response);
        
        StringBuilder logsBuilder = new StringBuilder();
        for (String log : response.getLogs()) {
            logsBuilder.append("• ").append(log).append("\n");
        }
        String logs = logsBuilder.toString();
        boolean hasLogs = !logs.isEmpty();
        if (hasLogs) logsArea.setText(logs);

        String error = response.getErrors() != null ? response.getErrors() : "";
        boolean hasError = !error.isEmpty();
        if (hasError) errorArea.setText(error);

        // 2. Sync Tabs (Order: Output, Attachments, Logs, Error)
        syncTab(outputScrollPane, "Output", hasOutput, 0);
        syncTab(attachmentsPanel, "Attachments", hasAttachments, 1);
        syncTab(logsScrollPane, "Logs", hasLogs, 2);
        syncTab(errorScrollPane, "Error", hasError, 3);
        
        // 3. Dynamic Visibility
        boolean hasResults = resultsTabbedPane.getTabCount() > 0;
        responseTitledPanel.setVisible(hasResults);
        
        if (hasResults) {
            if (response.getStatus() == ToolExecutionStatus.FAILED && hasError) {
                resultsTabbedPane.setSelectedComponent(errorScrollPane);
            } else if (response.getStatus() == ToolExecutionStatus.EXECUTED) {
                if (hasAttachments) {
                    resultsTabbedPane.setSelectedComponent(attachmentsPanel);
                } else if (hasOutput) {
                    resultsTabbedPane.setSelectedComponent(outputScrollPane);
                } else if (hasLogs) {
                    resultsTabbedPane.setSelectedComponent(logsScrollPane);
                }
            }
            
            if (resultsTabbedPane.getSelectedIndex() == -1) {
                resultsTabbedPane.setSelectedIndex(0);
            }
            adjustTabbedPaneHeight(resultsTabbedPane);
        }
    }
    
    /**
     * Synchronizes a tab's presence and position within the resultsTabbedPane.
     */
    private void syncTab(Component comp, String title, boolean visible, int rank) {
        int currentIndex = resultsTabbedPane.indexOfComponent(comp);
        if (visible) {
            if (currentIndex == -1) {
                // Calculate insertion index based on rank of existing tabs
                int insertAt = 0;
                for (int i = 0; i < resultsTabbedPane.getTabCount(); i++) {
                    Component existing = resultsTabbedPane.getComponentAt(i);
                    int existingRank = getRank(existing);
                    if (rank > existingRank) {
                        insertAt = i + 1;
                    } else {
                        break;
                    }
                }
                resultsTabbedPane.insertTab(title, null, comp, null, insertAt);
            }
        } else {
            if (currentIndex != -1) {
                resultsTabbedPane.removeTabAt(currentIndex);
            }
        }
    }

    private int getRank(Component comp) {
        if (comp == outputScrollPane) return 0;
        if (comp == attachmentsPanel) return 1;
        if (comp == logsScrollPane) return 2;
        if (comp == errorScrollPane) return 3;
        return 99;
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
        skipButton.setVisible(response.getStatus() == ToolExecutionStatus.PENDING);

        if (response.getStatus() == ToolExecutionStatus.EXECUTING) {
            runButton.setText("Stop");
            runButton.setIcon(IconUtils.getIcon("delete.png", 16, 16));
            runButton.addActionListener(e -> response.stop());
            runButton.setEnabled(true);
            revertButton.setVisible(false);
        } else if (response.getStatus() == ToolExecutionStatus.EXECUTED) {
            runButton.setText("Run Again");
            runButton.setIcon(new RunIcon(16));
            runButton.addActionListener(e -> executeTool());
            runButton.setEnabled(true);
            revertButton.setVisible(true);
        } else if (response.getStatus() == ToolExecutionStatus.PENDING || response.getStatus() == ToolExecutionStatus.NOT_EXECUTED) {
            runButton.setText("Run");
            runButton.setIcon(new RunIcon(16));
            runButton.addActionListener(e -> executeTool());
            runButton.setEnabled(true);
            revertButton.setVisible(false);
        } else if (response.getStatus() == ToolExecutionStatus.FAILED) {
            runButton.setText("Retry");
            runButton.setIcon(new RunIcon(16));
            runButton.addActionListener(e -> executeTool());
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

    /**
     * Executes the tool associated with this panel on a background thread using 
     * the Chat's dedicated executor.
     */
    private void executeTool() {
        chatPanel.getChat().getExecutor().execute(() -> getPart().getResponse().execute());
    }

    private void adjustTabbedPaneHeight(JTabbedPane tabbedPane) {
        Component selected = tabbedPane.getSelectedComponent();
        if (selected != null) {
            Component content = selected;
            if (selected instanceof JScrollPane sp) {
                content = sp.getViewport().getView();
            }
            
            Dimension prefSize = content.getPreferredSize();
            // 40px for tab headers, 40px extra buffer for padding/borders.
            int targetHeight = prefSize.height + 80; 
            
            Dimension currentPref = tabbedPane.getPreferredSize();
            if (currentPref.height != targetHeight) {
                int width = tabbedPane.getWidth() > 0 ? tabbedPane.getWidth() : currentPref.width;
                tabbedPane.setPreferredSize(new Dimension(width, targetHeight));
                
                // Force layout update
                tabbedPane.revalidate();
                tabbedPane.repaint();
                getCentralContainer().revalidate();
                
                // Also notify the conversation panel to update its scrolling if necessary
                SwingUtilities.invokeLater(() -> {
                    chatPanel.getConversationPanel().revalidate();
                    chatPanel.getConversationPanel().repaint();
                });
            }
        }
    }

    @Override
    protected void updateHeaderInfoText() {
        AbstractToolCall<?, ?> call = getPart();
        AbstractToolResponse<?> response = call.getResponse();
        Map<String, Object> effectiveArgs = call.getEffectiveArgs();
        
        StringBuilder sb = new StringBuilder("<html>");
        
        // Java-like signature: toolName(val1, val2, null, val4)
        sb.append("<font color='#444444'><b>").append(call.getToolName()).append("</b></font>(");
        
        String argsStr = call.getTool().getParameters().stream()
                .map(p -> {
                    Object val = effectiveArgs.get(p.getName());
                    String valStr = val == null ? "null" : TextUtils.formatValue(val.toString());
                    if (call.getModifiedArgs().containsKey(p.getName())) {
                        return "<font color='blue'>" + valStr + "</font>";
                    }
                    return valStr;
                })
                .collect(Collectors.joining(", "));
        
        sb.append(argsStr).append(")");
        
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
