/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.ui.resources;

import java.io.File;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JPanel;
import lombok.extern.slf4j.Slf4j;
import org.openide.cookies.OpenCookie;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import org.openide.loaders.DataObject;
import org.openide.util.ImageUtilities;
import uno.anahata.asi.nb.resources.handle.NbHandle;
import uno.anahata.asi.nb.tools.ide.IDE;
import uno.anahata.asi.agi.resource.handle.PathHandle;
import uno.anahata.asi.agi.resource.Resource;
import uno.anahata.asi.swing.agi.AgiPanel;
import uno.anahata.asi.swing.agi.resources.DefaultResourceUI;
import uno.anahata.asi.swing.icons.NextIcon;

/**
 * NetBeans-specific implementation of {@link uno.anahata.asi.swing.agi.resources.ResourceUI}.
 * <p>
 * This strategy leverages NetBeans native APIs for high-fidelity navigation 
 * and visualization within the IDE. It provides the {@link NetBeansTextResourceViewer} 
 * for 100% IDE fidelity (line numbers, folds, errors).
 * </p>
 * 
 * @author anahata
 */
@Slf4j
public class NbResourceUI extends DefaultResourceUI {

    /** 
     * {@inheritDoc} 
     * <p>Implementation details: Returns a native {@link NetBeansTextResourceViewer} 
     * for textual resources, providing real IDE editor frames.</p>
     */
    @Override
    public JComponent createContent(Resource resource, AgiPanel agiPanel) {
        if (resource.getHandle().isTextual()) {
            // Bypass IDE fidelity for log files to prevent 'Modified Externally' popups
            if (resource.getName().toLowerCase().endsWith(".log")) {
                return super.createContent(resource, agiPanel);
            }
            return new NetBeansTextResourceViewer(agiPanel, resource);
        }
        return super.createContent(resource, agiPanel);
    }
    
        /** 
     * {@inheritDoc} 
     * <p>Implementation details: Provides specialized metadata panels based 
     * on the handle's connectivity type.</p>
     */
    @Override
    public JPanel createHandlePanel(Resource resource, AgiPanel agiPanel) {
        if (resource.getHandle() instanceof NbHandle nbh) {
            NbHandlePanel nhp = new NbHandlePanel();
            nhp.setHandle(nbh);
            return nhp;
        } else {
            return super.createHandlePanel(resource, agiPanel);
        }
    }

    /** 
     * {@inheritDoc} 
     * <p>Implementation details: Injects IDE-specific 'Open' and 'Select' 
     * actions for physical resources.</p>
     */
    @Override
    public void populateActions(JPanel actionContainer, Resource resource, AgiPanel agiPanel) {
        if (!resource.getHandle().isVirtual()) {
            // 1. Open in Editor - Standard IDE navigation
            JButton openBtn = createLinkButton("Open in Editor", 
                "Open the file in the NetBeans code editor.", 
                new NextIcon(16));
            openBtn.addActionListener(e -> open(resource, agiPanel));
            actionContainer.add(openBtn);

            // 2. Select in Projects - Context synchronization
            JButton selectBtn = createLinkButton("Select in Projects", 
                "Locate and highlight the file in the IDE Projects tree.", 
                ImageUtilities.loadImageIcon("org/netbeans/modules/project/ui/resources/projectTab.png", true));
            selectBtn.addActionListener(e -> select(resource, agiPanel));
            actionContainer.add(selectBtn);
        } else {
            super.populateActions(actionContainer, resource, agiPanel);
        }
    }

    /** 
     * {@inheritDoc} 
     * <p>Implementation details: Uses the NetBeans {@link OpenCookie} 
     * to open physical files in the native code editor.</p>
     */
    @Override
    public void open(Resource resource, AgiPanel agiPanel) {
        String path = getPath(resource);
        if (path != null) {
            FileObject fo = FileUtil.toFileObject(new File(path));
            if (fo != null) {
                try {
                    DataObject dao = DataObject.find(fo);
                    OpenCookie oc = dao.getLookup().lookup(OpenCookie.class);
                    if (oc != null) {
                        log.info("Opening resource in NetBeans editor: {}", path);
                        oc.open();
                        return;
                    }
                } catch (Exception e) {
                    log.error("Failed to open file in IDE: " + path, e);
                }
            }
        }
        super.open(resource, agiPanel);
    }

    /** 
     * {@inheritDoc} 
     */
    @Override
    public void openUri(String uriString) {
        try {
            java.net.URI uri = java.net.URI.create(uriString);
            if ("file".equalsIgnoreCase(uri.getScheme())) {
                File file = new File(uri);
                FileObject fo = FileUtil.toFileObject(file);
                if (fo != null) {
                    try {
                        DataObject dao = DataObject.find(fo);
                        OpenCookie oc = dao.getLookup().lookup(OpenCookie.class);
                        if (oc != null) {
                            log.info("Opening URI in NetBeans editor: {}", uriString);
                            oc.open();
                            return;
                        }
                    } catch (Exception e) {
                        log.warn("Failed to open URI in IDE, falling back to desktop: " + uriString, e);
                    }
                }
            }
            java.awt.Desktop.getDesktop().browse(uri);
        } catch (Exception ex) {
            log.error("Failed to open URI: " + uriString, ex);
        }
    }

    /** 
     * {@inheritDoc} 
     * <p>Implementation details: Uses the ASI {@link IDE} tool to 
     * focus the resource in the Projects window.</p>
     */
    @Override
    public void select(Resource resource, AgiPanel agiPanel) {
        String path = getPath(resource);
        if (path != null) {
            try {
                log.info("Selecting resource in NetBeans projects: {}", path);
                IDE.selectInStatic(path, IDE.SelectInTarget.PROJECTS);
            } catch (Exception e) {
                log.error("Failed to select resource in IDE: " + path, e);
            }
        }
    }

    /**
     * Resolves the physical path from the handle for IDE navigation.
     * @param resource The resource.
     * @return The absolute path, or null if virtual.
     */
    private String getPath(Resource resource) {
        if (resource.getHandle() instanceof NbHandle nh) {
            return nh.getPath();
        } else if (resource.getHandle() instanceof PathHandle ph) {
            return ph.getPath();
        }
        return null;
    }
}
