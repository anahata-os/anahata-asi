/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.agi.resources;

import uno.anahata.asi.swing.agi.resources.view.AbstractViewPanel;
import uno.anahata.asi.swing.agi.resources.handle.AbstractHandlePanel;
import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;
import javax.swing.JTextField;
import javax.swing.SwingConstants;
import javax.swing.border.TitledBorder;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.exception.ExceptionUtils;
import uno.anahata.asi.agi.context.ContextPosition;
import uno.anahata.asi.agi.message.RagMessage;
import uno.anahata.asi.agi.resource.RefreshPolicy;
import uno.anahata.asi.agi.resource.Resource;
import uno.anahata.asi.agi.resource.view.TextView;
import uno.anahata.asi.swing.agi.AgiPanel;
import uno.anahata.asi.swing.agi.message.RagMessageViewer;
import uno.anahata.asi.swing.agi.resources.view.AbstractTextResourceViewer;
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
 * <b>Capability-Aware UI:</b> For non-textual resources, the high-fidelity 
 * 'Capabilities' tab is automatically hidden, leaving only 
 * the 'Model Perspective'.
 * </p>
 * 
 * @author anahata
 */
@Slf4j
public class ResourcePanel extends ScrollablePanel {

    /** The parent agi panel. */
    private final AgiPanel agiPanel;
    /** The resource currently being managed. */
    private Resource currentResource;

    // Resource Sector
    private final JLabel nameLabel;
    private final JTextField idField;
    private final JCheckBox providingBox;
    private final JComboBox<ContextPosition> positionCombo;
    private final JComboBox<RefreshPolicy> policyCombo;

    /** Container for handle-specific metadata panel. */
    private final JPanel handleSectorContainer;
    /** Container for view-specific metadata panel. */
    private final JPanel viewSectorContainer;

    private final JTabbedPane mainTabs;
    
    /** Container for the content component provided by the strategy. */
    private final JPanel viewerContainer;
    /** Container for the host-specific action buttons. */
    private final JPanel actionPanel;
    
    /** The primary content component returned by the strategy. */
    private JComponent activeViewer;
    /** The active strategy being used for the current resource. */
    private ResourceUI activeStrategy;

    /** Button to toggle between View and Edit modes. */
    private final JButton editBtn;
    /** Tracks the current editing state. */
    private boolean editing = false;

    /** Guard flag to prevent feedback loops during UI synchronization. */
    private boolean syncing = false;

    /** Reactive listener for resource state changes. */
    private EdtPropertyChangeListener resourceListener;

    /**
     * Constructs a new Resource2Panel.
     * @param agiPanel The parent AgiPanel.
     */
    public ResourcePanel(AgiPanel agiPanel) {
        this.agiPanel = agiPanel;
        setLayout(new BorderLayout());
        setBorder(BorderFactory.createEmptyBorder(8, 8, 8, 8));

        // 1. Triple-Sectored Header
        JPanel sectorsPanel = new JPanel(new GridBagLayout());
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.weightx = 1.0;
        gbc.insets = new Insets(0, 0, 4, 0);

        // --- Sector A: Resource ---
        JPanel resourceSector = new JPanel(new GridBagLayout());
        resourceSector.setBorder(BorderFactory.createTitledBorder(null, "Resource", TitledBorder.LEFT, TitledBorder.TOP, getFont().deriveFont(Font.BOLD)));
        GridBagConstraints rgbc = new GridBagConstraints();
        rgbc.gridx = 0; rgbc.gridy = 0; rgbc.anchor = GridBagConstraints.WEST; rgbc.insets = new Insets(2, 5, 2, 5);

        nameLabel = new JLabel("Resource");
        nameLabel.setFont(nameLabel.getFont().deriveFont(Font.BOLD, 14f));
        resourceSector.add(nameLabel, rgbc);

        rgbc.gridx = 1; rgbc.weightx = 1.0; rgbc.fill = GridBagConstraints.HORIZONTAL;
        actionPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT, 10, 0));
        actionPanel.setOpaque(false);
        editBtn = new JButton("📝 EDIT");
        editBtn.addActionListener(e -> toggleEditMode());
        actionPanel.add(editBtn);
        resourceSector.add(actionPanel, rgbc);

        rgbc.gridx = 0; rgbc.gridy++; rgbc.weightx = 0; rgbc.fill = GridBagConstraints.NONE;
        resourceSector.add(new JLabel("UUID:"), rgbc);
        rgbc.gridx = 1; rgbc.weightx = 1.0; rgbc.fill = GridBagConstraints.HORIZONTAL;
        idField = createReadOnlyField();
        resourceSector.add(idField, rgbc);

        rgbc.gridx = 0; rgbc.gridy++; rgbc.weightx = 0; rgbc.fill = GridBagConstraints.NONE;
        providingBox = new JCheckBox("Providing Context");
        providingBox.addActionListener(e -> { 
            if (!syncing && currentResource != null) {
                currentResource.setProviding(providingBox.isSelected()); 
            }
        });
        resourceSector.add(providingBox, rgbc);

        rgbc.gridx = 1; rgbc.fill = GridBagConstraints.HORIZONTAL;
        JPanel rComboPanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 15, 0));
        rComboPanel.setOpaque(false);
        rComboPanel.add(new JLabel("Position:"));
        positionCombo = new JComboBox<>(ContextPosition.values());
        positionCombo.addActionListener(e -> { 
            if (!syncing && currentResource != null) {
                currentResource.setContextPosition((ContextPosition) positionCombo.getSelectedItem()); 
            }
        });
        rComboPanel.add(positionCombo);
        rComboPanel.add(new JLabel("Refresh:"));
        policyCombo = new JComboBox<>(RefreshPolicy.values());
        policyCombo.addActionListener(e -> { 
            if (!syncing && currentResource != null) {
                currentResource.setRefreshPolicy((RefreshPolicy) policyCombo.getSelectedItem()); 
            }
        });
        rComboPanel.add(policyCombo);
        resourceSector.add(rComboPanel, rgbc);

        sectorsPanel.add(resourceSector, gbc);

        // --- Sector B & C: Dynamic Containers ---
        gbc.gridy++;
        handleSectorContainer = new JPanel(new BorderLayout());
        sectorsPanel.add(handleSectorContainer, gbc);
        
        gbc.gridy++;
        viewSectorContainer = new JPanel(new BorderLayout());
        sectorsPanel.add(viewSectorContainer, gbc);

        add(sectorsPanel, BorderLayout.NORTH);

        // 2. Main Viewport Tabs
        mainTabs = new JTabbedPane();
        viewerContainer = new JPanel(new BorderLayout());
        add(mainTabs, BorderLayout.CENTER);
    }

    private JTextField createReadOnlyField() {
        JTextField f = new JTextField();
        f.setEditable(false);
        f.setBorder(null);
        f.setOpaque(false);
        return f;
    }

    /**
     * Toggles between read-only viewport view and high-fidelity editor.
     */
    private void toggleEditMode() {
        if (currentResource == null || activeStrategy == null || activeViewer == null) {
            return;
        }

        if (editing) {
            String newContent = activeStrategy.getEditorContent(activeViewer);
            if (newContent != null) {
                saveContent(newContent);
            } else {
                setEditing(false);
            }
        } else {
            setEditing(true);
        }
    }

    private void setEditing(boolean editing) {
        this.editing = editing;
        activeStrategy.setEditing(activeViewer, editing);
        
        if (editing) {
            editBtn.setText("💾 SAVE");
            editBtn.setIcon(new RestartIcon(16));
        } else {
            editBtn.setText("📝 EDIT");
            editBtn.setIcon(null);
        }
    }

    /**
     * Atomically saves the content back to the resource via the agnostic write API.
     */
    private void saveContent(String content) {
        new SwingTask<>(this, "Saving Resource", () -> {
            log.info("Saving content to {}", currentResource );
            currentResource.write(content);
            currentResource.reloadIfNeeded();
            return null;
        }, done -> {
            setEditing(false);
            syncUiWithResource();
        }).execute();
    }

    /**
     * Sets the resource to manage and initializes the dynamic viewer.
     * <p>
     * <b>Background SENSING:</b> This method performs a background reload of 
     * the resource to trigger view binding and content freshness before 
     * assembling the Strategy-based UI.
     * </p>
     * @param res The V2 resource instance.
     */
    public void setResource(Resource res) {
        if (resourceListener != null) {
            resourceListener.unbind();
        }
        
        this.currentResource = res;
        this.editing = false;
        editBtn.setText("📝 EDIT");
        editBtn.setIcon(null);
        
        // 1. Initial Cleanup
        viewerContainer.removeAll();
        actionPanel.removeAll();
        actionPanel.add(editBtn);
        handleSectorContainer.removeAll();
        viewSectorContainer.removeAll();
        mainTabs.removeAll();

        if (res == null) {
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
            updateTabs();
        }).execute();

        // 3. Show Placeholder
        JLabel loadingLabel = new JLabel("Sensing Blaugrana Resource...", SwingConstants.CENTER);
        loadingLabel.setFont(loadingLabel.getFont().deriveFont(Font.ITALIC, 16f));
        viewerContainer.add(loadingLabel, BorderLayout.CENTER);
        mainTabs.addTab("Capabilities", viewerContainer);
    }

    /**
     * Assemblies the specialized UI components using the host-aware strategy.
     */
    private void assembleResourceUI() {
        if (currentResource == null) return;
        
        viewerContainer.removeAll();
        this.activeStrategy = ResourceUiRegistry.getInstance().getResourceUI();
        
        if (activeStrategy != null) {
            // Swap specialized metadata panels
            handleSectorContainer.add(activeStrategy.createHandlePanel(currentResource, agiPanel), BorderLayout.CENTER);
            viewSectorContainer.add(activeStrategy.createViewPanel(currentResource, agiPanel), BorderLayout.CENTER);
            
            this.activeViewer = activeStrategy.createContent(currentResource, agiPanel);
            
            // ARCHITECTURAL FIDELITY: Hide the viewer's internal toolbar because ResourcePanel 
            // provides its own integrated control header and sectoral view panels.
            if (activeViewer instanceof AbstractTextResourceViewer atrv) {
                atrv.setToolbarVisible(false);
            }
            
            // HEIGHT FIDELITY: Cap the Capability view to 800px to ensure the internal scroller 
            // is triggered and navigation is possible for long resources.
            JPanel cappedWrapper = new JPanel(new BorderLayout()) {
                @Override
                public Dimension getPreferredSize() {
                    Dimension d = super.getPreferredSize();
                    return new Dimension(d.width, Math.min(800, d.height));
                }
            };
            cappedWrapper.add(activeViewer, BorderLayout.CENTER);
            
            viewerContainer.add(cappedWrapper, BorderLayout.CENTER);
            activeStrategy.populateActions(actionPanel, currentResource, agiPanel);
            editBtn.setVisible(activeStrategy.canEdit(currentResource) && currentResource.isWritable());
        } else {
            viewerContainer.add(new JLabel("No ResourceUI strategy registered for this host environment."), BorderLayout.CENTER);
            editBtn.setVisible(false);
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

            // Refresh sub-panels
            if (handleSectorContainer.getComponentCount() > 0 && handleSectorContainer.getComponent(0) instanceof AbstractHandlePanel php) {
                php.refresh();
            }
            if (viewSectorContainer.getComponentCount() > 0 && viewSectorContainer.getComponent(0) instanceof AbstractViewPanel avp) {
                avp.refresh();
            }

            updateTabs();
        } finally {
            this.syncing = false;
        }
        
        revalidate();
        repaint();
    }

    /**
     * Rebuilds the tabs based on the resource capabilities.
     */
    private void updateTabs() {
        mainTabs.removeAll();
        
        // Tab 1: Capabilities (Only for textual resources or valid views)
        if (currentResource.getHandle().isTextual() || currentResource.getView() != null) {
            mainTabs.addTab("Capabilities", viewerContainer);
        }
        
        // Tab 2: Model Perspective (RAG)
        mainTabs.addTab("Model Perspective (RAG)", createModelPerspectiveComponent());
    }

    /**
     * Generates the "Model Perspective" component, showing the definitive
     * RAG part that will be injected into the prompt.
     * @return The RAG viewer component.
     */
    private JComponent createModelPerspectiveComponent() {
        RagMessage rawMsg = new RagMessage(agiPanel.getAgi());
        try {
            // SENSING PURITY: Freshness is guaranteed by the Turn orchestration or 
            // the background load task. Reloading here causes recursion.
            rawMsg.addTextPart(currentResource.getHeader());
            if (currentResource.getContextPosition() == ContextPosition.SYSTEM_INSTRUCTIONS) {
                currentResource.getSystemInstructions().forEach(rawMsg::addTextPart);
            } else {
                currentResource.populateMessage(rawMsg);
            }
        } catch (Exception e) {
            rawMsg.addTextPart("**Error generating perspective:**\n" + ExceptionUtils.getStackTrace(e));
        }

        RagMessageViewer viewer = new RagMessageViewer(agiPanel, rawMsg, false, false);
        viewer.render();
        return viewer;
    }
}
