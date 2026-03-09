/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.agi.resources;

import uno.anahata.asi.swing.agi.resources.view.AbstractTextResourceViewer;
import javax.swing.JComponent;
import javax.swing.JPanel;
import uno.anahata.asi.agi.resource.Resource;
import uno.anahata.asi.swing.agi.AgiPanel;

/**
 * The definitive strategy interface for providing a host-aware user interface 
 * for V2 {@link Resource}s.
 * <p>
 * Implementations are responsible for creating the content visualization, 
 * injecting environment-specific actions, and providing modular metadata 
 * panels for handles and views.
 * </p>
 * <p>
 * This interface also defines the Edit/Save lifecycle, allowing the UI to 
 * transition between read-only previews and full-file editing.
 * </p>
 * 
 * @author anahata
 */
public interface ResourceUI {

    /**
     * Creates the primary content component for the given resource.
     * 
     * @param resource The resource to render.
     * @param agiPanel The parent AgiPanel.
     * @return The JComponent representing the content view.
     */
    JComponent createContent(Resource resource, AgiPanel agiPanel);

    /**
     * Injects host-specific actions into the provided container.
     * 
     * @param actionContainer The container (usually a toolbar or header panel).
     * @param resource The resource instance.
     * @param agiPanel The parent AgiPanel.
     */
    void populateActions(JPanel actionContainer, Resource resource, AgiPanel agiPanel);

    /**
     * Creates a specialized metadata panel for the resource's handle.
     * 
     * @param resource The resource instance.
     * @param agiPanel The parent AgiPanel.
     * @return A JPanel configured to show handle-specific attributes.
     */
    JPanel createHandlePanel(Resource resource, AgiPanel agiPanel);

    /**
     * Creates a specialized metadata panel for the resource's view.
     * 
     * @param resource The resource instance.
     * @param agiPanel The parent AgiPanel.
     * @return A JPanel configured to show view-specific attributes.
     */
    JPanel createViewPanel(Resource resource, AgiPanel agiPanel);

    /**
     * Opens the resource in the host environment's preferred viewer or editor.
     * 
     * @param resource The resource to open.
     * @param agiPanel The parent AgiPanel.
     */
    void open(Resource resource, AgiPanel agiPanel);

    /**
     * Selects and highlights the resource in the host environment's navigation 
     * component (e.g., Projects tree).
     * 
     * @param resource The resource to select.
     * @param agiPanel The parent AgiPanel.
     */
    void select(Resource resource, AgiPanel agiPanel);

    /**
     * Determines if the given resource can be edited in this environment.
     * 
     * @param resource The resource instance.
     * @return true if editing is supported.
     */
    default boolean canEdit(Resource resource) {
        return resource.getHandle().isTextual();
    }

    /**
     * Signals the active viewer component for this resource to enter or exit edit mode.
     * 
     * @param viewer The component returned by {@link #createContent}.
     * @param editing true to enable editing.
     */
    default void setEditing(JComponent viewer, boolean editing) {
        if (viewer instanceof AbstractTextResourceViewer atv) {
            atv.setEditing(editing);
        }
    }

    /**
     * Retrieves the current content from the editor component for saving.
     * 
     * @param viewer The component returned by {@link #createContent}.
     * @return The current content string, or null if not available.
     */
    default String getEditorContent(JComponent viewer) {
        if (viewer instanceof AbstractTextResourceViewer atv) {
            return atv.getEditorContent();
        }
        return null;
    }
}
