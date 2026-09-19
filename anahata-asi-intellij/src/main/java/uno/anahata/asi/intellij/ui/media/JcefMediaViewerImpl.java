/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.intellij.ui.media;

import com.intellij.openapi.util.Disposer;
import com.intellij.ui.jcef.JBCefApp;
import com.intellij.ui.jcef.JBCefBrowser;
import com.intellij.ui.jcef.JBCefJSQuery;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.event.MouseEvent;
import java.awt.event.MouseWheelEvent;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;
import lombok.Getter;
import lombok.NonNull;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.internal.TikaUtils;
import uno.anahata.asi.swing.agi.AgiPanel;
import uno.anahata.asi.swing.agi.render.MediaToolbar;
import uno.anahata.asi.swing.agi.render.MediaViewerComponent;

/**
 * Native IntelliJ video and media viewer powered by the Chromium Embedded Framework (JCEF).
 * <p>
 * Implements {@link MediaViewerComponent} to embed hardware-accelerated HTML5 video playback
 * within the IntelliJ tool window without requiring OpenJFX.
 * </p>
 *
 * @author anahata
 */
@Slf4j
public class JcefMediaViewerImpl extends JPanel implements MediaViewerComponent {

    /**
     * Standardized action and metadata toolbar.
     */
    @Getter
    private final MediaToolbar toolbar;

    /**
     * The embedded Chromium browser component.
     */
    private JBCefBrowser browser;

    /**
     * Temporary disk file created when media bytes are loaded from memory.
     */
    private File tempMediaFile;

    /**
     * Temporary HTML page file used to grant same-origin permissions for local media files.
     */
    private File tempHtmlFile;

    /**
     * JS-to-Java bridge query for forwarding scroll wheel events to the enclosing Swing JScrollPane.
     */
    private JBCefJSQuery scrollQuery;

    /**
     * The resolved media playback URI.
     */
    private URI currentPlayUri;

    /**
     * Cached MIME type string.
     */
    private String mimeType;

    /**
     * Constructs a new JcefMediaViewerImpl.
     *
     * @param agiPanel The parent AgiPanel providing session and configuration context.
     */
    public JcefMediaViewerImpl(@NonNull AgiPanel agiPanel) {
        super(new BorderLayout());
        this.toolbar = new MediaToolbar(agiPanel);
        setOpaque(true);
        setBackground(new Color(11, 15, 25));
        setMinimumSize(new Dimension(320, 240));
        setPreferredSize(new Dimension(640, 420));

        if (JBCefApp.isSupported()) {
            this.browser = new JBCefBrowser();
            this.scrollQuery = JBCefJSQuery.create(browser);
            this.scrollQuery.addHandler(deltaYStr -> {
                forwardScrollToSwing(deltaYStr);
                return null;
            });
            add(browser.getComponent(), BorderLayout.CENTER);
        } else {
            JLabel unsupported = new JLabel("JCEF is not supported on this platform/JDK", SwingConstants.CENTER);
            unsupported.setForeground(new Color(148, 163, 184));
            add(unsupported, BorderLayout.CENTER);
        }

        add(toolbar, BorderLayout.SOUTH);
    }

    /**
     * {@inheritDoc}
     * <p>
     * Returns this JPanel as the primary visual component.
     * </p>
     */
    @Override
    public JComponent getComponent() {
        return this;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Resolves the media URI, updates the toolbar metadata, and initializes the HTML5 player.
     * </p>
     */
    @Override
    public void load(byte[] data, String mimeType, String displayName, URI sourceUri) {
        toolbar.setData(data);
        toolbar.setMimeType(mimeType);
        toolbar.setDisplayName(displayName);
        toolbar.setSourceUri(sourceUri);
        toolbar.updateMetadata(null);

        this.mimeType = mimeType;
        this.currentPlayUri = resolveMediaUri(data, mimeType, displayName, sourceUri);
        if (currentPlayUri == null) {
            log.warn("JcefMediaViewer: Unable to resolve a playable media URI.");
            return;
        }

        if (browser != null) {
            String html = generateHtml5VideoPlayer(currentPlayUri.toString(), mimeType);
            try {
                if (tempHtmlFile != null && tempHtmlFile.exists()) {
                    tempHtmlFile.delete();
                }
                this.tempHtmlFile = File.createTempFile("jcef_player_", ".html");
                this.tempHtmlFile.deleteOnExit();
                Files.writeString(tempHtmlFile.toPath(), html, StandardCharsets.UTF_8);
                browser.loadURL(tempHtmlFile.toURI().toString());
            } catch (IOException ex) {
                log.error("JcefMediaViewer: Failed to write temp player HTML, falling back to loadHTML", ex);
                browser.loadHTML(html);
            }
        }
    }

    /**
     * Generates a modern, dark-themed HTML5 video player page with scroll forwarding.
     *
     * @param mediaUrl The URL or file URI of the media source.
     * @param mimeType The detected MIME type.
     * @return The complete HTML5 document.
     */
    private String generateHtml5VideoPlayer(String mediaUrl, String mimeType) {
        String scrollJs = "";
        if (scrollQuery != null) {
            scrollJs = "  window.addEventListener('wheel', function(e) {\n"
                    + "    " + scrollQuery.inject("e.deltaY") + "\n"
                    + "  }, { passive: true });\n";
        }

        return "<!DOCTYPE html>\n"
                + "<html>\n"
                + "<head>\n"
                + "  <meta charset='utf-8'/>\n"
                + "  <style>\n"
                + "    html, body {\n"
                + "      margin: 0; padding: 0;\n"
                + "      width: 100%; height: 100%;\n"
                + "      background-color: #0b0f19;\n"
                + "      display: flex; justify-content: center; align-items: center;\n"
                + "      overflow: hidden;\n"
                + "    }\n"
                + "    video {\n"
                + "      max-width: 100%; max-height: 100%;\n"
                + "      outline: none;\n"
                + "      object-fit: contain;\n"
                + "    }\n"
                + "  </style>\n"
                + "</head>\n"
                + "<body>\n"
                + "  <video id='player' controls autoplay loop>\n"
                + "    <source src='" + mediaUrl + "' type='" + mimeType + "'>\n"
                + "    Your browser does not support HTML5 video playback.\n"
                + "  </video>\n"
                + "  <script>\n"
                + scrollJs
                + "  </script>\n"
                + "</body>\n"
                + "</html>";
    }

    /**
     * Forwards a scroll wheel delta from Chromium up to the enclosing Swing JScrollPane.
     *
     * @param deltaYStr The vertical scroll delta string from JavaScript.
     */
    private void forwardScrollToSwing(String deltaYStr) {
        if (deltaYStr == null || deltaYStr.isBlank()) {
            return;
        }
        try {
            double deltaY = Double.parseDouble(deltaYStr.trim());
            if (deltaY == 0) {
                return;
            }
            int wheelRotation = deltaY > 0 ? 1 : -1;
            SwingUtilities.invokeLater(() -> {
                JScrollPane scrollPane = (JScrollPane) SwingUtilities.getAncestorOfClass(JScrollPane.class, this);
                if (scrollPane != null) {
                    MouseWheelEvent mwe = new MouseWheelEvent(
                            scrollPane,
                            MouseEvent.MOUSE_WHEEL,
                            System.currentTimeMillis(),
                            0,
                            0, 0,
                            1,
                            false,
                            MouseWheelEvent.WHEEL_UNIT_SCROLL,
                            3,
                            wheelRotation
                    );
                    scrollPane.dispatchEvent(mwe);
                }
            });
        } catch (NumberFormatException ignored) {
        }
    }

    /**
     * Resolves the media URI to play, writing in-memory bytes to a temporary file if needed.
     *
     * @param data The raw binary data.
     * @param mimeType The MIME type.
     * @param displayName The display or file name.
     * @param sourceUri The optional original source URI.
     * @return A playable URI, or {@code null} on failure.
     */
    private URI resolveMediaUri(byte[] data, String mimeType, String displayName, URI sourceUri) {
        if (sourceUri != null && "file".equalsIgnoreCase(sourceUri.getScheme())) {
            File f = new File(sourceUri);
            if (f.exists()) {
                return f.toURI();
            }
        }

        if (data != null && data.length > 0) {
            try {
                String ext = TikaUtils.getExtension(mimeType);
                String prefix = (displayName != null) ? displayName.replaceAll("[^a-zA-Z0-9.-]", "_") + "_" : "media_";
                if (prefix.length() < 3) {
                    prefix = "media_";
                }
                this.tempMediaFile = File.createTempFile(prefix, ext);
                this.tempMediaFile.deleteOnExit();
                try (FileOutputStream fos = new FileOutputStream(tempMediaFile)) {
                    fos.write(data);
                }
                return tempMediaFile.toURI();
            } catch (IOException ex) {
                log.error("JcefMediaViewer: Failed to write temp file for video playback", ex);
            }
        }
        return sourceUri;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Pauses the active HTML5 video playback via JavaScript execution in the browser.
     * </p>
     */
    @Override
    public void stop() {
        if (browser != null && browser.getCefBrowser() != null) {
            try {
                browser.getCefBrowser().executeJavaScript(
                        "var v = document.getElementById('player'); if (v) { v.pause(); }",
                        browser.getCefBrowser().getURL(), 0
                );
            } catch (Throwable t) {
                log.debug("Could not pause JCEF player: {}", t.getMessage());
            }
        }
    }

    /**
     * {@inheritDoc}
     * <p>
     * Automatically pauses playback when detached from the Swing container hierarchy.
     * </p>
     */
    @Override
    public void removeNotify() {
        stop();
        super.removeNotify();
    }

    /**
     * {@inheritDoc}
     * <p>
     * Permanently disposes of the JCEF browser instance and cleans up temporary media files.
     * </p>
     */
    @Override
    public void dispose() {
        stop();
        if (scrollQuery != null) {
            Disposer.dispose(scrollQuery);
            scrollQuery = null;
        }
        if (browser != null) {
            Disposer.dispose(browser);
            browser = null;
        }
        if (tempMediaFile != null && tempMediaFile.exists()) {
            tempMediaFile.delete();
            tempMediaFile = null;
        }
        if (tempHtmlFile != null && tempHtmlFile.exists()) {
            tempHtmlFile.delete();
            tempHtmlFile = null;
        }
        this.currentPlayUri = null;
    }
}
