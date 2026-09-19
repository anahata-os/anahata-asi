/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.agi.resources;

import uno.anahata.asi.swing.agi.resources.view.MediaViewPanel;
import uno.anahata.asi.swing.agi.resources.view.TextViewPanel;
import uno.anahata.asi.swing.agi.resources.view.RSyntaxTextAreaTextResourceViewer;
import uno.anahata.asi.swing.agi.resources.handle.UrlHandlePanel;
import uno.anahata.asi.swing.agi.resources.handle.PathHandlePanel;
import uno.anahata.asi.swing.agi.resources.handle.StringHandlePanel;
import java.awt.BorderLayout;
import java.awt.Desktop;
import java.io.File;
import java.io.IOException;
import java.net.URI;
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.AbstractAsiContainer;
import uno.anahata.asi.swing.AbstractSwingAsiContainer;
import uno.anahata.asi.swing.agi.render.MediaViewerComponent;
import uno.anahata.asi.agi.resource.view.MediaView;
import uno.anahata.asi.agi.resource.handle.PathHandle;
import uno.anahata.asi.agi.resource.Resource;
import uno.anahata.asi.agi.resource.handle.StringHandle;
import uno.anahata.asi.agi.resource.view.TextView;
import uno.anahata.asi.agi.resource.handle.UrlHandle;
import uno.anahata.asi.swing.agi.AgiPanel;
import uno.anahata.asi.swing.agi.render.MediaRenderer;
import uno.anahata.asi.swing.icons.IconUtils;
import uno.anahata.asi.swing.icons.NextIcon;

/**
 * The standard, universal implementation of {@link ResourceUI} for Standalone 
 * and generic Swing environments.
 * <p>
 * It provides capability-based visualization (Text, Image, Audio) and 
 * basic OS-level actions with descriptive icons.
 * </p>
 * 
 * @author anahata
 */
@Slf4j
public class DefaultResourceUI implements ResourceUI {

    /** 
     * {@inheritDoc} 
     * <p>Implementation details: Dispatches to {@link RSyntaxTextAreaTextResourceViewer} 
     * for text resources or {@link MediaRenderer#createViewer} for binary/media resources.</p>
     */
    @Override
    public JComponent createContent(Resource resource, AgiPanel agiPanel) {
        if (resource.getHandle().isTextual()) {
            return new RSyntaxTextAreaTextResourceViewer(agiPanel, resource);
        } else if (resource.getView() instanceof MediaView mv) {
            return createMediaComponent(resource, mv, null, agiPanel);
        }
        
        return new JLabel("No viewer available for: " + resource.getMimeType());
    }

    /** 
     * {@inheritDoc} 
     * <p>Implementation details: Dispatches to {@link RSyntaxTextAreaTextResourceViewer} 
     * bound to a container context for text resources or {@link MediaRenderer#createViewer} for binary/media resources.</p>
     */
    @Override
    public JComponent createContent(Resource resource, AbstractAsiContainer container) {
        if (resource.getHandle().isTextual()) {
            return new RSyntaxTextAreaTextResourceViewer(container, resource);
        } else if (resource.getView() instanceof MediaView mv) {
            return createMediaComponent(resource, mv, container, null);
        }
        
        return new JLabel("No viewer available for: " + resource.getMimeType());
    }

    /**
     * Creates a Swing component for media-based resources using {@link MediaRenderer#createViewer}.
     * 
     * @param resource The resource instance.
     * @param mv The associated media view.
     * @param container The optional container instance.
     * @param agiPanel The optional AgiPanel instance.
     * @return The configured media JComponent.
     */
    private JComponent createMediaComponent(Resource resource, MediaView mv, AbstractAsiContainer container, AgiPanel agiPanel) {
        AbstractSwingAsiContainer swingContainer = (container instanceof AbstractSwingAsiContainer sac) ? sac
                : (agiPanel != null && agiPanel.getAgi().getConfig().getAsiContainer() instanceof AbstractSwingAsiContainer sac2 ? sac2 : null);

        byte[] data = null;
        if (mv != null && mv.getCachedData() != null) {
            data = mv.getCachedData();
        } else {
            try {
                data = resource.asBytes();
            } catch (Exception ex) {
                log.debug("Could not read binary data directly from resource handle: {}", ex.getMessage());
            }
        }

        URI uri = (resource.getHandle() != null) ? resource.getHandle().getUri() : null;

        if ((data == null || data.length == 0) && uri == null) {
            return new JLabel("Media data not available.");
        }

        MediaViewerComponent viewer = MediaRenderer.createViewer(
                data,
                resource.getMimeType(),
                resource.getName(),
                uri,
                swingContainer,
                agiPanel);
        return viewer.getComponent();
    }

    /** 
     * {@inheritDoc} 
     * <p>Implementation details: Injects OS-level actions for physical resources.</p>
     */
    @Override
    public void populateActions(JPanel actionContainer, Resource resource, AgiPanel agiPanel) {
        if (!resource.getHandle().isVirtual()) {
            JButton openBtn = createLinkButton("Open in System", 
                "Open file using the OS default application.", 
                new NextIcon(16));
            openBtn.addActionListener(e -> open(resource, agiPanel));
            actionContainer.add(openBtn);
        } else if (resource.getHandle() instanceof UrlHandle) {
             JButton openBtn = createLinkButton("Browse URL", 
                 "Open the URL in the system browser.", 
                 IconUtils.getIcon("discord.png", 16, 16));
            openBtn.addActionListener(e -> open(resource, agiPanel));
            actionContainer.add(openBtn);
        }
    }

    /** 
     * {@inheritDoc} 
     * <p>Implementation details: Provides specialized metadata panels based 
     * on the handle's connectivity type.</p>
     */
    @Override
    public JPanel createHandlePanel(Resource resource, AgiPanel agiPanel) {
        if (resource.getHandle() instanceof PathHandle ph) {
            PathHandlePanel php = new PathHandlePanel();
            php.setHandle(ph);
            return php;
        } else if (resource.getHandle() instanceof UrlHandle uh) {
            UrlHandlePanel uhp = new UrlHandlePanel();
            uhp.setHandle(uh);
            return uhp;
        } else if (resource.getHandle() instanceof StringHandle sh) {
            StringHandlePanel shp = new StringHandlePanel();
            shp.setHandle(sh);
            return shp;
        }
        return new JPanel();
    }

    /** 
     * {@inheritDoc} 
     * <p>Implementation details: Provides specialized metadata panels based 
     * on the view's semantic interpreter type.</p>
     */
    @Override
    public JPanel createViewPanel(Resource resource, AgiPanel agiPanel) {
        if (resource.getView() instanceof TextView tv) {
            TextViewPanel tvp = new TextViewPanel(agiPanel);
            tvp.setView(tv);
            return tvp;
        } else if (resource.getView() instanceof MediaView mv) {
            MediaViewPanel mvp = new MediaViewPanel(agiPanel);
            mvp.setView(mv);
            return mvp;
        }
        throw new IllegalStateException("Cannot create view panel for resource " + resource);
    }

    /** 
     * {@inheritDoc} 
     * <p>Implementation details: Uses {@link Desktop} API to open physical or URL resources.</p>
     */
    @Override
    public void open(Resource resource, AgiPanel agiPanel) {
        try {
            if (resource.getHandle() instanceof PathHandle ph) {
                Desktop.getDesktop().open(new File(ph.getPath()));
            } else if (resource.getHandle() instanceof UrlHandle uh) {
                Desktop.getDesktop().browse(uh.getUri());
            }
        } catch (IOException ex) {
            log.error("Failed to open resource: " + resource.getName(), ex);
        }
    }

    /** 
     * {@inheritDoc} 
     */
    @Override
    public void openUri(String uriString) {
        try {
            Desktop.getDesktop().browse(URI.create(uriString));
        } catch (Exception ex) {
            log.error("Failed to open URI: " + uriString, ex);
        }
    }

    /** 
     * {@inheritDoc} 
     * <p>Implementation details: Standalone mode does not support project-level selection.</p>
     */
    @Override
    public void select(Resource resource, AgiPanel agiPanel) {
        log.debug("Select not supported in DefaultResourceUI for: {}", resource.getName());
    }

    /**
     * Helper for creating hyperlink-styled buttons with icons.
     * @param text The button text.
     * @param tooltip The tooltip.
     * @param icon The icon.
     * @return The configured JButton.
     */
    protected JButton createLinkButton(String text, String tooltip, Icon icon) {
        JButton btn = new JButton(text, icon);
        btn.setToolTipText(tooltip);
        //btn.setBorderPainted(false);
        //btn.setOpaque(false);
        //btn.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
        return btn;
    }
}
