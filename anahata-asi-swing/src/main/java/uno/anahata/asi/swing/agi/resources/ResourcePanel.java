/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.agi.resources;

import com.jgoodies.forms.layout.CellConstraints;
import com.jgoodies.forms.layout.FormLayout;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.SwingConstants;
import javax.swing.UIManager;
import javax.swing.border.TitledBorder;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.exception.ExceptionUtils;
import uno.anahata.asi.agi.context.ContextPosition;
import uno.anahata.asi.agi.message.RagMessage;
import uno.anahata.asi.agi.resource.RefreshPolicy;
import uno.anahata.asi.agi.resource.Resource;
import uno.anahata.asi.swing.agi.AgiPanel;
import uno.anahata.asi.swing.agi.message.RagMessagePanel;
import uno.anahata.asi.swing.agi.resources.handle.AbstractHandlePanel;
import uno.anahata.asi.swing.agi.resources.view.AbstractTextResourceViewer;
import uno.anahata.asi.swing.agi.resources.view.AbstractViewPanel;
import uno.anahata.asi.swing.components.AdjustingTabPane;
import uno.anahata.asi.swing.components.ScrollablePanel;
import uno.anahata.asi.swing.icons.RestartIcon;
import uno.anahata.asi.swing.internal.EdtPropertyChangeListener;
import uno.anahata.asi.swing.internal.SwingTask;

/**
 * A specialized panel for managing and viewing a V2 {@link Resource}.
 * <p>
 * This panel acts as a high-level orchestrator and command-and-control center.
 * It dynamically swaps specialized sub-panels for Handle and View metadata
 * based on the resource type.
 * </p>
 * <p>
 * <b>Fidelity Stack:</b> This panel uses a dual AdjustingTabPane architecture
 * within a vertical BoxLayout to ensure a perfectly compact UI.
 * </p>
 *
 * @author anahata
 */
@Slf4j
public class ResourcePanel extends ScrollablePanel {

    /**
     * The parent agi panel.
     */
    private final AgiPanel agiPanel;
    /**
     * The resource currently being managed.
     */
    private Resource currentResource;

    // Global Header
    private final JLabel nameLabel;
    private final JPanel actionPanel;

    // Metadata Components (Identity Tab)
    private final JTextField idField;
    private final JCheckBox providingBox;
    private final JComboBox<ContextPosition> positionCombo;
    private final JComboBox<RefreshPolicy> policyCombo;

    /**
     * Container for handle-specific metadata panel.
     */
    private final JPanel handleSectorContainer;
    /**
     * Container for view-specific metadata panel.
     */
    private final JPanel viewSectorContainer;

    /**
     * Container for the content component provided by the strategy.
     */
    private final JPanel viewerContainer;

    /**
     * The primary content component returned by the strategy.
     */
    private JComponent activeViewer;
    /**
     * The active strategy being used for the current resource.
     */
    private ResourceUI activeStrategy;

    /**
     * Tab pane for metadata.
     */
    private final AdjustingTabPane metadataTabs;
    
    /**
     * Tab pane for content.
     */
    private final AdjustingTabPane contentTabs;

    /**
     * Guard flag to prevent feedback loops during UI synchronization.
     */
    private boolean syncing = false;

    /**
     * Reactive listener for resource state changes.
     */
    private EdtPropertyChangeListener resourceListener;

    /**
     * Constructs a new ResourcePanel.
     *
     * @param agiPanel The parent AgiPanel.
     */
    public ResourcePanel(AgiPanel agiPanel) {
        this.agiPanel = agiPanel;

        setLayout(new BorderLayout());
        setOpaque(true);
        setBackground(javax.swing.UIManager.getColor("Panel.background"));
        setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));

        // 1. GLOBAL HEADER
        JPanel globalHeader = new JPanel(new BorderLayout());
        globalHeader.setOpaque(false);
        globalHeader.setMaximumSize(new Dimension(Integer.MAX_VALUE, 40));

        nameLabel = new JLabel("Resource");
        nameLabel.setFont(nameLabel.getFont().deriveFont(Font.BOLD, 18f));
        globalHeader.add(nameLabel, BorderLayout.WEST);

        actionPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT, 10, 0));
        actionPanel.setOpaque(false);
        globalHeader.add(actionPanel, BorderLayout.EAST);

        // 2. METADATA TABS (Identity, Handle, View)
        metadataTabs = new AdjustingTabPane(50);

        // Identity Tab - Pure JGoodies Implementation
        JPanel identityTab = new JPanel();
        identityTab.setOpaque(false);
        identityTab.setBorder(BorderFactory.createTitledBorder(null, "Identity (Core Metadata)", TitledBorder.LEFT, TitledBorder.TOP, identityTab.getFont().deriveFont(Font.BOLD)));

        FormLayout idLayout = new FormLayout(
                "pref, 6dlu, pref, 0:grow", // Cols: Label, Gap, Component, Pusher
                "p, 3dlu, p, 3dlu, p, 3dlu, p" // Rows: UUID, Position, Policy, Providing
        );
        identityTab.setLayout(idLayout);
        CellConstraints cc = new CellConstraints();

        // ROW 0: UUID
        identityTab.add(new JLabel("UUID:"), cc.xy(1, 1));
        idField = createReadOnlyField();
        identityTab.add(idField, cc.xy(3, 1));

        // ROW 1: Position
        identityTab.add(new JLabel("Position:"), cc.xy(1, 3));
        positionCombo = new JComboBox<>(ContextPosition.values());
        positionCombo.addActionListener(e -> {
            if (!syncing && currentResource != null) {
                currentResource.setContextPosition((ContextPosition) positionCombo.getSelectedItem());
            }
        });
        identityTab.add(positionCombo, cc.xy(3, 3));

        // ROW 2: Refresh Policy
        identityTab.add(new JLabel("Refresh Policy:"), cc.xy(1, 5));
        policyCombo = new JComboBox<>(RefreshPolicy.values());
        policyCombo.addActionListener(e -> {
            if (!syncing && currentResource != null) {
                currentResource.setRefreshPolicy((RefreshPolicy) policyCombo.getSelectedItem());
            }
        });
        identityTab.add(policyCombo, cc.xy(3, 5));

        // ROW 3: Providing Context
        providingBox = new JCheckBox("Providing Context");
        providingBox.setOpaque(false);
        providingBox.addActionListener(e -> {
            if (!syncing && currentResource != null) {
                currentResource.setProviding(providingBox.isSelected());
            }
        });
        identityTab.add(providingBox, cc.xyw(1, 7, 3));

        metadataTabs.addTab("Identity", identityTab);

        handleSectorContainer = new JPanel(new BorderLayout());
        handleSectorContainer.setOpaque(false);
        metadataTabs.addTab("Handle", handleSectorContainer);

        viewSectorContainer = new JPanel(new BorderLayout());
        viewSectorContainer.setOpaque(false);
        metadataTabs.addTab("View", viewSectorContainer);

        // 3. CONTENT TABS (Capabilities, Model perspective (RAG))
        contentTabs = new AdjustingTabPane(350);

        viewerContainer = new JPanel(new BorderLayout());
        viewerContainer.setOpaque(false);

        // 4. VERTICAL STACK ASSEMBLY
        JPanel contentStack = new JPanel();
        contentStack.setLayout(new BoxLayout(contentStack, BoxLayout.Y_AXIS));
        contentStack.setOpaque(false);

        contentStack.add(globalHeader);
        contentStack.add(Box.createVerticalStrut(10));
        contentStack.add(metadataTabs);
        contentStack.add(Box.createVerticalStrut(15));
        contentStack.add(contentTabs);

        // Final glue to push everything up
        contentStack.add(Box.createVerticalGlue());

        add(contentStack, BorderLayout.CENTER);
    }

    private JTextField createReadOnlyField() {
        JTextField f = new JTextField(35); // Long enough for UUID
        f.setEditable(false);
        f.setBorder(BorderFactory.createCompoundBorder(
                BorderFactory.createLineBorder(Color.LIGHT_GRAY),
                BorderFactory.createEmptyBorder(2, 5, 2, 5)
        ));
        f.setOpaque(true);
        f.setBackground(UIManager.getColor("TextField.inactiveBackground"));
        return f;
    }

    /**
     * Atomically saves the content back to the resource.
     */
    private void saveContent(String content) {
        new SwingTask<>(this, "Saving Resource", () -> {
            log.info("Saving content to {}", currentResource);
            currentResource.write(content);
            currentResource.reloadIfNeeded();
            return null;
        }, done -> {
            syncUiWithResource();
        }).execute();
    }

    /**
     * Sets the resource to manage and initializes the dynamic viewer.
     * @param res The one and only Anahata Resource
     */
    public void setResource(Resource res) {
        if (resourceListener != null) {
            resourceListener.unbind();
        }

        this.currentResource = res;

        // 1. Initial Cleanup
        viewerContainer.removeAll();
        actionPanel.removeAll();
        handleSectorContainer.removeAll();
        viewSectorContainer.removeAll();
        contentTabs.removeAll();

        if (res == null) {
            nameLabel.setText("No Resource Selected");
            revalidate();
            repaint();
            return;
        }

        // 2. Background Reload & UI Assembly
        new SwingTask<>(this, "Loading Resource", () -> {
            res.reloadIfNeeded();
            return null;
        }, done -> {
            this.resourceListener = new EdtPropertyChangeListener(this, res, null, evt -> syncUiWithResource());
            assembleResourceUI();
        }, error -> {
            viewerContainer.add(new JLabel("<html><font color='red'><b>Failed to load resource:</b><br>" + error.getMessage() + "</font></html>"), BorderLayout.CENTER);
            updatePerspectives();
        }).execute();

        // 3. Show Placeholder
        JLabel loadingLabel = new JLabel("Sensing Resource...", SwingConstants.CENTER);
        loadingLabel.setFont(loadingLabel.getFont().deriveFont(Font.ITALIC, 16f));
        viewerContainer.add(loadingLabel, BorderLayout.CENTER);
        contentTabs.addTab("Capabilities", viewerContainer);
    }

    /**
     * Assemblies the specialized UI components using the host-aware strategy.
     */
    private void assembleResourceUI() {
        if (currentResource == null) {
            return;
        }

        viewerContainer.removeAll();
        this.activeStrategy = ResourceUiRegistry.getInstance().getResourceUI();

        if (activeStrategy != null) {
            handleSectorContainer.removeAll();
            handleSectorContainer.add(activeStrategy.createHandlePanel(currentResource, agiPanel), BorderLayout.CENTER);

            viewSectorContainer.removeAll();
            viewSectorContainer.add(activeStrategy.createViewPanel(currentResource, agiPanel), BorderLayout.CENTER);

            this.activeViewer = activeStrategy.createContent(currentResource, agiPanel);

            if (activeViewer instanceof AbstractTextResourceViewer atv) {
                atv.setToolbarVisible(activeStrategy.canEdit(currentResource) && currentResource.isWritable());
                atv.setVerticalScrollEnabled(false);
                atv.setSaveAction(this::saveContent);
            }

            viewerContainer.add(activeViewer, BorderLayout.CENTER);
            actionPanel.removeAll();
            activeStrategy.populateActions(actionPanel, currentResource, agiPanel);
        }

        syncUiWithResource();
    }

    /**
     * Synchronizes the common metadata controls with the resource state.
     */
    private void syncUiWithResource() {
        if (currentResource == null || syncing) {
            return;
        }

        this.syncing = true;
        try {
            nameLabel.setText(currentResource.getName());
            idField.setText(currentResource.getId());
            providingBox.setSelected(currentResource.isProviding());
            positionCombo.setSelectedItem(currentResource.getContextPosition());
            policyCombo.setSelectedItem(currentResource.getRefreshPolicy());

            if (handleSectorContainer.getComponentCount() > 0 && handleSectorContainer.getComponent(0) instanceof AbstractHandlePanel php) {
                php.refresh();
            }
            if (viewSectorContainer.getComponentCount() > 0 && viewSectorContainer.getComponent(0) instanceof AbstractViewPanel avp) {
                avp.refresh();
            }

            updatePerspectives();
        } finally {
            this.syncing = false;
        }

        revalidate();
        repaint();
    }

    /**
     * Rebuilds the content tabs based on the resource capabilities.
     */
    private void updatePerspectives() {
        int selected = contentTabs.getSelectedIndex();
        boolean hasCapabilities = currentResource.getHandle().isTextual() || currentResource.getView() != null;

        // ARCHITECTURAL STABILITY: We only rebuild if the tab count or composition changed 
        // to prevent losing reference to actively rendering components.
        int expectedCount = hasCapabilities ? 2 : 1;
        if (contentTabs.getTabCount() != expectedCount) {
            contentTabs.removeAll();
            if (hasCapabilities) {
                contentTabs.addTab("Capabilities", viewerContainer);
            }
            contentTabs.addTab("Model perspective (RAG)", createModelPerspectiveComponent());
        } else {
            // Just refresh RAG content if it's the only one that changed dynamically
            int ragIndex = hasCapabilities ? 1 : 0;
            contentTabs.setComponentAt(ragIndex, createModelPerspectiveComponent());
        }

        if (selected >= 0 && selected < contentTabs.getTabCount()) {
            contentTabs.setSelectedIndex(selected);
        }

        contentTabs.refresh();
    }

    /**
     * Generates the "Model Perspective" component.
     */
    private JComponent createModelPerspectiveComponent() {
        RagMessage rawMsg = new RagMessage(agiPanel.getAgi());
        try {
            rawMsg.addTextPart(currentResource.getHeader());
            if (currentResource.getContextPosition() == ContextPosition.SYSTEM_INSTRUCTIONS) {
                currentResource.getSystemInstructions().forEach(rawMsg::addTextPart);
            } else {
                currentResource.populateMessage(rawMsg);
            }
        } catch (Exception e) {
            rawMsg.addTextPart("**Error generating perspective:**\n" + ExceptionUtils.getStackTrace(e));
        }

        RagMessagePanel panel = new RagMessagePanel(agiPanel, rawMsg, false, false);
        panel.render();
        return panel;
    }
}
