/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.nb.ui.render;

import java.awt.Color;
import java.awt.event.HierarchyEvent;
import java.awt.event.HierarchyListener;
import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.JComponent;
import javax.swing.JEditorPane;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;
import javax.swing.text.EditorKit;
import lombok.extern.slf4j.Slf4j;
import org.netbeans.api.editor.mimelookup.MimeLookup;
import org.netbeans.editor.EditorUI;
import org.netbeans.editor.GlyphGutter;
import org.netbeans.editor.Utilities;
import org.netbeans.spi.editor.SideBarFactory;
import uno.anahata.asi.swing.agi.AgiPanel;
import uno.anahata.asi.swing.agi.render.JEditorPaneCodeBlockSegmentRenderer;

/**
 * A NetBeans-specific code block renderer that provides high-fidelity IDE features
 * like line numbers, code folding, and semantic annotations.
 * 
 * <p>It uses a {@link HierarchyListener} to detect when the component is added to 
 * the UI, triggering the 'onboarding' process that connects the editor to the 
 * NetBeans {@link EditorUI} and injects the native sidebars.</p>
 * 
 * @author anahata
 */
@Slf4j
public class NbCodeBlockSegmentRenderer extends JEditorPaneCodeBlockSegmentRenderer {

    /**
     * Constructs a new NbCodeBlockSegmentRenderer.
     * 
     * @param agiPanel The active agi panel.
     * @param initialContent The initial code content.
     * @param language The language identifier.
     * @param kit The NetBeans EditorKit.
     */
    public NbCodeBlockSegmentRenderer(AgiPanel agiPanel, String initialContent, String language, EditorKit kit) {
        super(agiPanel, initialContent, language, kit);
    }

    /**
     * {@inheritDoc}
     * <p>Implementation details: Installs a HierarchyListener to trigger the 
     * NetBeans onboarding process once the editor is visible.</p>
     */
    @Override
    protected JComponent createInnerComponent() {
        JEditorPane editor = (JEditorPane) super.createInnerComponent();
        
        editor.addHierarchyListener(new HierarchyListener() {
            @Override
            public void hierarchyChanged(HierarchyEvent e) {
                if ((e.getChangeFlags() & HierarchyEvent.SHOWING_CHANGED) != 0 && editor.isShowing()) {
                    // Give NetBeans a moment to install the UI and create the EditorUI
                    SwingUtilities.invokeLater(() -> onboardNetBeansEditor(editor));
                    editor.removeHierarchyListener(this);
                }
            }
        });
        
        return editor;
    }

    /**
     * Performs the deep integration with the NetBeans editor system.
     * 
     * @param editor The realized JEditorPane.
     */
    private void onboardNetBeansEditor(JEditorPane editor) {
        EditorUI eui = Utilities.getEditorUI(editor);
        if (eui == null || scrollPane == null) {
            log.warn("EditorUI or ScrollPane null during onboarding. High-fidelity rendering skipped.");
            return;
        }

        log.info("Onboarding NetBeans editor for high-fidelity rendering...");
        
        // 1. Enable Line Numbers in the internal UI model
        eui.setLineNumberEnabled(true);
        // Trigger a width calculation to sync internal state
        eui.updateLineNumberWidth(0);

        JPanel rowHeader = new JPanel();
        rowHeader.setLayout(new BoxLayout(rowHeader, BoxLayout.X_AXIS));
        rowHeader.setBackground(Color.WHITE);

        // 2. Inject the Real Line Numbers (GlyphGutter)
        GlyphGutter gutter = new GlyphGutter(eui);
        gutter.update();
        rowHeader.add(gutter);

        // 3. Inject other sidebars (Folding, Errors, etc.) from MimeLookup
        String mimeType = (String) editor.getDocument().getProperty("mimeType");
        if (mimeType != null) {
            for (SideBarFactory factory : MimeLookup.getLookup(mimeType).lookupAll(SideBarFactory.class)) {
                // Skip GlyphGutter if it's returned as a factory to avoid duplicates
                if (factory.getClass().getName().contains("GlyphGutter")) {
                    continue;
                }
                
                JComponent sidebar = factory.createSideBar(editor);
                if (sidebar != null) {
                    sidebar.setBorder(BorderFactory.createMatteBorder(0, 0, 0, 1, Color.LIGHT_GRAY));
                    rowHeader.add(sidebar);
                    log.debug("Injected sidebar: {}", sidebar.getClass().getSimpleName());
                }
            }
        }

        // 4. Update the scroll pane row header
        scrollPane.setRowHeaderView(rowHeader);
        rowHeader.revalidate();
        rowHeader.repaint();
    }
}
