/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.agi.project;

import java.awt.GridLayout;
import java.awt.event.ActionListener;
import java.util.function.Consumer;
import javax.swing.BorderFactory;
import javax.swing.JCheckBox;
import javax.swing.JPanel;
import lombok.Getter;
import lombok.Setter;
import uno.anahata.asi.toolkit.project.ProjectStructureScope;

/**
 * A dedicated Swing panel for configuring a {@link ProjectStructureScope}.
 * <p>
 * Displays the 9 granularity switches governing how project and module structures,
 * compiler alerts, physical file sizes, VCS badges, and AST metadata are rendered
 * into the AI prompt. Supports programmatic binding and editable state toggling.
 * </p>
 *
 * @author anahata
 */
public class ProjectStructureScopePanel extends JPanel {

    private final JCheckBox showAlertsBox;
    private final JCheckBox showRootFilesBox;
    private final JCheckBox showResourcesBox;
    private final JCheckBox showVcsStatusBox;
    private final JCheckBox showFileSizesBox;
    private final JCheckBox showElementKindBox;
    private final JCheckBox showInnerClassesBox;
    private final JCheckBox showSupertypesBox;
    private final JCheckBox showJavadocBox;

    @Getter
    private ProjectStructureScope scope;

    @Getter
    @Setter
    private Consumer<ProjectStructureScope> onScopeChanged;

    private boolean adjusting = false;

    /**
     * Constructs a new ProjectStructureScopePanel with default scope settings.
     */
    public ProjectStructureScopePanel() {
        this(new ProjectStructureScope());
    }

    /**
     * Constructs a new ProjectStructureScopePanel bound to a specific scope.
     *
     * @param initialScope The initial project structure granularity scope.
     */
    public ProjectStructureScopePanel(ProjectStructureScope initialScope) {
        setLayout(new GridLayout(0, 3, 8, 4));
        setBorder(BorderFactory.createEmptyBorder(4, 4, 4, 4));

        showAlertsBox = new JCheckBox("Compiler Alerts");
        showAlertsBox.setToolTipText("Include compiler errors, broken references, and diagnostic problem alerts");

        showRootFilesBox = new JCheckBox("Root Files");
        showRootFilesBox.setToolTipText("Include root directory files like pom.xml, README.md, anahata.md");

        showResourcesBox = new JCheckBox("Resources");
        showResourcesBox.setToolTipText("Include non-Java resource directories like src/main/resources, .github");

        showVcsStatusBox = new JCheckBox("VCS Badges");
        showVcsStatusBox.setToolTipText("Append version control status markers (e.g. [-/M], [-/A])");

        showFileSizesBox = new JCheckBox("File Sizes");
        showFileSizesBox.setToolTipText("Append physical file sizes in human-readable format (e.g. [12.5 KB])");

        showElementKindBox = new JCheckBox("Element Kinds");
        showElementKindBox.setToolTipText("Render Java element kinds like (CLASS), (INTERFACE), (ENUM), (RECORD)");

        showInnerClassesBox = new JCheckBox("Inner Classes");
        showInnerClassesBox.setToolTipText("Recursively render nested and inner classes beneath their enclosing type");

        showSupertypesBox = new JCheckBox("Supertypes");
        showSupertypesBox.setToolTipText("List extended superclasses and implemented interfaces (e.g. extends Foo implements Bar)");

        showJavadocBox = new JCheckBox("Javadoc Summaries");
        showJavadocBox.setToolTipText("Include the first sentence of the class or package Javadoc summary");

        add(showAlertsBox);
        add(showRootFilesBox);
        add(showResourcesBox);
        add(showVcsStatusBox);
        add(showFileSizesBox);
        add(showElementKindBox);
        add(showInnerClassesBox);
        add(showSupertypesBox);
        add(showJavadocBox);

        ActionListener listener = e -> {
            if (!adjusting) {
                ProjectStructureScope newScope = buildScopeFromUi();
                this.scope = newScope;
                if (onScopeChanged != null) {
                    onScopeChanged.accept(newScope);
                }
            }
        };

        showAlertsBox.addActionListener(listener);
        showRootFilesBox.addActionListener(listener);
        showResourcesBox.addActionListener(listener);
        showVcsStatusBox.addActionListener(listener);
        showFileSizesBox.addActionListener(listener);
        showElementKindBox.addActionListener(listener);
        showInnerClassesBox.addActionListener(listener);
        showSupertypesBox.addActionListener(listener);
        showJavadocBox.addActionListener(listener);

        setScope(initialScope);
    }

    /**
     * Updates the UI checkboxes to match the specified scope settings without firing change events.
     *
     * @param scope The scope to apply, or null for default settings.
     */
    public void setScope(ProjectStructureScope scope) {
        this.scope = scope != null ? scope : new ProjectStructureScope();
        adjusting = true;
        try {
            showAlertsBox.setSelected(this.scope.isShowAlerts());
            showRootFilesBox.setSelected(this.scope.isShowRootFiles());
            showResourcesBox.setSelected(this.scope.isShowResources());
            showVcsStatusBox.setSelected(this.scope.isShowVcsStatus());
            showFileSizesBox.setSelected(this.scope.isShowFileSizes());
            showElementKindBox.setSelected(this.scope.isShowElementKind());
            showInnerClassesBox.setSelected(this.scope.isShowInnerClasses());
            showSupertypesBox.setSelected(this.scope.isShowSupertypes());
            showJavadocBox.setSelected(this.scope.isShowJavadoc());
        } finally {
            adjusting = false;
        }
    }

    /**
     * Enables or disables all granularity checkboxes.
     * <p>
     * Used when toggling between custom local overrides and inherited default scopes.
     * </p>
     *
     * @param editable true to allow user editing; false to gray out checkboxes.
     */
    public void setEditable(boolean editable) {
        showAlertsBox.setEnabled(editable);
        showRootFilesBox.setEnabled(editable);
        showResourcesBox.setEnabled(editable);
        showVcsStatusBox.setEnabled(editable);
        showFileSizesBox.setEnabled(editable);
        showElementKindBox.setEnabled(editable);
        showInnerClassesBox.setEnabled(editable);
        showSupertypesBox.setEnabled(editable);
        showJavadocBox.setEnabled(editable);
    }

    /**
     * Constructs a new immutable {@link ProjectStructureScope} from the current checkbox states.
     *
     * @return A newly constructed ProjectStructureScope instance.
     */
    private ProjectStructureScope buildScopeFromUi() {
        return ProjectStructureScope.builder()
                .showAlerts(showAlertsBox.isSelected())
                .showRootFiles(showRootFilesBox.isSelected())
                .showResources(showResourcesBox.isSelected())
                .showVcsStatus(showVcsStatusBox.isSelected())
                .showFileSizes(showFileSizesBox.isSelected())
                .showElementKind(showElementKindBox.isSelected())
                .showInnerClasses(showInnerClassesBox.isSelected())
                .showSupertypes(showSupertypesBox.isSelected())
                .showJavadoc(showJavadocBox.isSelected())
                .build();
    }
}
