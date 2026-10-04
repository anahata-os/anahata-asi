/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.ide.ui.project;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.FlowLayout;
import java.awt.Font;
import java.beans.PropertyChangeEvent;
import javax.swing.BorderFactory;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.context.ContextProvider;
import uno.anahata.asi.swing.agi.context.AbstractContextProviderRenderer;
import uno.anahata.asi.ide.tools.project.context.AbstractProjectContextProvider;
import uno.anahata.asi.ide.tools.project.ProjectStructureScope;

/**
 * Specialized context provider renderer for {@link AbstractProjectContextProvider}.
 * <p>
 * Displays a scope mode dropdown ("Inherit from Default Scope" vs "Custom Scope Override")
 * and embeds the {@link ProjectStructureScopePanel}. When in inherited mode, checkboxes
 * display the effective inherited settings from the parent/toolkit and are disabled.
 * When switched to custom mode, checkboxes become editable for project-specific overrides.
 * </p>
 *
 * @author anahata
 */
@Slf4j
public class ProjectContextProviderPanel extends AbstractContextProviderRenderer<AbstractProjectContextProvider> {

    /**
     * Display label for the inherited scope mode.
     */
    private static final String MODE_INHERITED = "Inherit";

    /**
     * Display label for the custom scope override mode.
     */
    private static final String MODE_CUSTOM = "Custom";

    /**
     * Combo box for switching between inherited and custom structure scope modes.
     */
    private final JComboBox<String> modeCombo;

    /**
     * Label displaying the inheritance source when in inherited mode.
     */
    private final JLabel inheritanceSourceLabel;

    /**
     * Embedded panel rendering the checkboxes for configuring individual scope switches.
     */
    private final ProjectStructureScopePanel scopePanel;

    /**
     * Reentrancy guard preventing circular event loops during programmatic UI updates.
     */
    private boolean adjusting = false;

    /**
     * Constructs a new ProjectContextProviderPanel.
     */
    public ProjectContextProviderPanel() {
        super();
        setBorder(BorderFactory.createTitledBorder("Project Structure Scope"));

        JPanel topBar = new JPanel(new FlowLayout(FlowLayout.LEFT, 6, 2));
        topBar.setOpaque(false);
        topBar.add(new JLabel("Scope:"));

        modeCombo = new JComboBox<>(new String[]{MODE_INHERITED, MODE_CUSTOM});
        topBar.add(modeCombo);

        inheritanceSourceLabel = new JLabel();
        inheritanceSourceLabel.setFont(inheritanceSourceLabel.getFont().deriveFont(Font.ITALIC));
        inheritanceSourceLabel.setForeground(new Color(0, 128, 0));
        topBar.add(inheritanceSourceLabel);

        scopePanel = new ProjectStructureScopePanel();

        add(topBar, BorderLayout.NORTH);
        add(scopePanel, BorderLayout.CENTER);

        modeCombo.addActionListener(e -> {
            if (adjusting || contextProvider == null) {
                return;
            }
            String selected = (String) modeCombo.getSelectedItem();
            if (MODE_INHERITED.equals(selected)) {
                log.info("Switching project {} to inherited scope mode", contextProvider.getName());
                contextProvider.setScope(null);
                updateUiFromProvider();
            } else if (MODE_CUSTOM.equals(selected)) {
                log.info("Switching project {} to custom scope override mode", contextProvider.getName());
                ProjectStructureScope initialCustom = contextProvider.getEffectiveScope().toBuilder().build();
                contextProvider.setScope(initialCustom);
                updateUiFromProvider();
            }
        });

        scopePanel.setOnScopeChanged(newScope -> {
            if (!adjusting && contextProvider != null && contextProvider.getScope() != null) {
                log.info("Updating custom ProjectStructureScope for {}: {}", contextProvider.getName(), newScope);
                contextProvider.setScope(newScope);
            }
        });
    }

    /**
     * {@inheritDoc}
     * <p>
     * Re-synchronizes the scope combo and checkboxes when a new context provider is bound.
     * </p>
     */
    @Override
    protected void onBind() {
        updateUiFromProvider();
    }

    /**
     * Synchronizes the mode combo and scope checkboxes with the bound provider's state.
     */
    private void updateUiFromProvider() {
        if (contextProvider == null) {
            return;
        }
        adjusting = true;
        try {
            boolean isCustom = (contextProvider.getScope() != null);
            modeCombo.setSelectedItem(isCustom ? MODE_CUSTOM : MODE_INHERITED);
            scopePanel.setScope(contextProvider.getEffectiveScope());
            scopePanel.setEditable(isCustom);

            if (!isCustom) {
                inheritanceSourceLabel.setVisible(true);
                inheritanceSourceLabel.setText("Inheriting from: " + resolveInheritanceSource(contextProvider));
            } else {
                inheritanceSourceLabel.setVisible(false);
            }
        } finally {
            adjusting = false;
        }
    }

    /**
     * Resolves the descriptive name of the ancestor providing the effective scope.
     *
     * @param pcp The project context provider to inspect.
     * @return The simple name of the parent project with custom scope, or "Projects toolkit default".
     */
    private String resolveInheritanceSource(AbstractProjectContextProvider pcp) {
        ContextProvider curr = pcp.getParentProvider();
        while (curr instanceof AbstractProjectContextProvider ancestor) {
            if (ancestor.getScope() != null) {
                return ancestor.getName();
            }
            curr = ancestor.getParentProvider();
        }
        return "Projects toolkit default";
    }

    /**
     * {@inheritDoc}
     * <p>
     * Updates the UI when the underlying project provider's scope configuration changes.
     * </p>
     */
    @Override
    public void propertyChange(PropertyChangeEvent evt) {
        if ("projectStructureScope".equals(evt.getPropertyName())) {
            updateUiFromProvider();
        }
    }
}
