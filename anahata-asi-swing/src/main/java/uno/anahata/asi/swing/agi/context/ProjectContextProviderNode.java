/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.agi.context;

import java.util.List;
import javax.swing.tree.TreePath;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.swing.agi.AgiPanel;
import uno.anahata.asi.swing.internal.EdtPropertyChangeListener;
import uno.anahata.asi.swing.internal.SwingTask;
import uno.anahata.asi.toolkit.project.AbstractProjectContextProvider;

/**
 * A specialized context tree node for {@link AbstractProjectContextProvider} instances.
 * <p>
 * Manages reactive updates when a project or module's granularity scope changes.
 * Dispatches heavy token recalculations (which may invoke VCS/Git operations)
 * to a background {@link SwingTask} to prevent blocking the Swing Event Dispatch Thread (EDT).
 * </p>
 *
 * @author anahata
 */
@Slf4j
public class ProjectContextProviderNode extends ContextProviderNode {

    /**
     * Lifecycle-aware listener bound to the parent AgiPanel component.
     */
    private final EdtPropertyChangeListener scopeListener;

    /**
     * Constructs a new ProjectContextProviderNode.
     *
     * @param agiPanel The parent AgiPanel component governing listener lifecycle.
     * @param userObject The project context provider to wrap.
     */
    public ProjectContextProviderNode(AgiPanel agiPanel, AbstractProjectContextProvider userObject) {
        super(agiPanel, userObject);
        this.scopeListener = new EdtPropertyChangeListener(agiPanel, userObject, "projectStructureScope", evt -> onProjectScopeChanged());
    }

    /**
     * Reacts to project structure scope changes on this project or module:
     * dispatches token recalculation to a background SwingTask (preventing Git/VCS EDT lockups),
     * cascades to child modules that inherit scope, bubbles totals up to ancestors,
     * and refreshes the tree table model.
     */
    private void onProjectScopeChanged() {
        new SwingTask<Void>(agiPanel, "Recalculating Project Tokens", () -> {
            refreshInheritedSubtree();
            bubbleUpTotals();
            return null;
        }, v -> {
            if (agiPanel != null && agiPanel.getContextPanel() != null) {
                ContextTreeTableModel model = (ContextTreeTableModel) agiPanel.getContextPanel().getTreeTable().getTreeTableModel();
                model.refreshNodeData(this);
                agiPanel.getContextPanel().getTreeTable().repaint();
            }
        }, null, false).start();
    }

    /**
     * Recalculates local tokens for this project and cascades down to all child
     * module nodes that inherit their scope from this project.
     */
    public void refreshInheritedSubtree() {
        this.instructionsTokens = 0;
        this.declarationsTokens = 0;
        this.historyTokens = 0;
        this.ragTokens = 0;
        calculateLocalTokens();

        if (children != null) {
            for (AbstractContextNode<?> child : children) {
                if (child instanceof ProjectContextProviderNode pcpn) {
                    if (((AbstractProjectContextProvider) pcpn.getUserObject()).getScope() == null) {
                        pcpn.refreshInheritedSubtree();
                    }
                }
            }
        }
        updateStatus();
    }

    /**
     * {@inheritDoc}
     * <p>
     * Ensures child project or module providers are instantiated as ProjectContextProviderNode.
     * </p>
     */
    @Override
    protected AbstractContextNode<?> createChildNode(Object obj) {
        if (obj instanceof AbstractProjectContextProvider apcp) {
            return new ProjectContextProviderNode(agiPanel, apcp);
        }
        return super.createChildNode(obj);
    }
}
