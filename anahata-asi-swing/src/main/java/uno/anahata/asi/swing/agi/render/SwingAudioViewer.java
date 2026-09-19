/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.agi.render;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.event.ItemEvent;
import java.io.ByteArrayInputStream;
import java.net.URI;
import javax.sound.sampled.AudioInputStream;
import javax.sound.sampled.AudioSystem;
import javax.sound.sampled.SourceDataLine;
import javax.swing.BorderFactory;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JToggleButton;
import javax.swing.SwingUtilities;
import lombok.Getter;
import lombok.NonNull;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.swing.agi.AgiPanel;

/**
 * Pure Swing audio player implementing {@link MediaViewerComponent}.
 * <p>
 * Provides lightweight audio playback using the JavaSound engine. Functions in all
 * host environments regardless of JavaFX runtime availability.
 * </p>
 * 
 * @author anahata
 */
@Slf4j
public class SwingAudioViewer extends JPanel implements MediaViewerComponent {

    /** The action and metadata toolbar. */
    @Getter
    private final MediaToolbar toolbar;

    /** Toggle button for initiating and stopping playback. */
    private final JToggleButton playButton = new JToggleButton("▶ Play Audio");

    /** Status label indicating playback state. */
    private final JLabel statusLabel = new JLabel("Audio Ready");

    /** Active playback thread handle for cancellation. */
    private volatile Thread playbackThread;

    /** Active hardware line handle. */
    private volatile SourceDataLine activeLine;

    /** Raw audio data loaded into this viewer. */
    private byte[] audioData;

    /**
     * Constructs a new SwingAudioViewer.
     *
     * @param agiPanel The parent AgiPanel providing session and configuration context.
     */
    public SwingAudioViewer(@NonNull AgiPanel agiPanel) {
        super(new BorderLayout());

        this.toolbar = new MediaToolbar(agiPanel);
        setOpaque(true);
        setBackground(new Color(24, 28, 36));
        setBorder(BorderFactory.createEmptyBorder(10, 14, 10, 14));

        JPanel centerPanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 12, 10));
        centerPanel.setOpaque(false);

        playButton.setFont(playButton.getFont().deriveFont(Font.BOLD, 12f));
        playButton.setFocusable(false);
        playButton.addItemListener(e -> {
            if (e.getStateChange() == ItemEvent.SELECTED) {
                startPlayback();
            } else {
                stopPlayback();
            }
        });

        statusLabel.setFont(statusLabel.getFont().deriveFont(Font.PLAIN, 12f));
        statusLabel.setForeground(new Color(148, 163, 184));

        centerPanel.add(playButton);
        centerPanel.add(statusLabel);

        add(centerPanel, BorderLayout.CENTER);
        add(toolbar, BorderLayout.SOUTH);
    }

    /**
     * Starts audio playback in a dedicated background worker thread.
     */
    private void startPlayback() {
        if (audioData == null || audioData.length == 0) {
            statusLabel.setText("No audio data");
            playButton.setSelected(false);
            return;
        }

        stopPlayback();

        statusLabel.setText("Playing...");
        playButton.setText("■ Stop Audio");

        playbackThread = new Thread(() -> {
            try (AudioInputStream ais = AudioSystem.getAudioInputStream(new ByteArrayInputStream(audioData));
                 SourceDataLine line = AudioSystem.getSourceDataLine(ais.getFormat())) {
                
                this.activeLine = line;
                line.open(ais.getFormat());
                line.start();

                byte[] buffer = new byte[4096];
                int read;
                while (!Thread.currentThread().isInterrupted() && (read = ais.read(buffer, 0, buffer.length)) != -1) {
                    line.write(buffer, 0, read);
                }
                line.drain();
            } catch (Exception e) {
                log.warn("Error during audio playback in SwingAudioViewer: {}", e.getMessage());
            } finally {
                this.activeLine = null;
                this.playbackThread = null;
                SwingUtilities.invokeLater(() -> {
                    playButton.setSelected(false);
                    playButton.setText("▶ Play Audio");
                    statusLabel.setText("Playback Finished");
                });
            }
        }, "SwingAudioViewer-Playback");

        playbackThread.setDaemon(true);
        playbackThread.start();
    }

    /**
     * Stops active audio playback.
     */
    private void stopPlayback() {
        if (playbackThread != null) {
            playbackThread.interrupt();
            playbackThread = null;
        }
        if (activeLine != null) {
            try {
                activeLine.stop();
                activeLine.close();
            } catch (Throwable ignored) {
            }
            activeLine = null;
        }
        playButton.setText("▶ Play Audio");
        statusLabel.setText("Stopped");
    }

    /**
     * {@inheritDoc}
     * <p>Returns this JPanel as the primary visual component.</p>
     */
    @Override
    public JComponent getComponent() {
        return this;
    }

    /**
     * {@inheritDoc}
     * <p>Loads audio bytes and updates the action toolbar.</p>
     */
    @Override
    public void load(byte[] data, String mimeType, String displayName, URI sourceUri) {
        this.audioData = data;
        toolbar.setData(data);
        toolbar.setMimeType(mimeType);
        toolbar.setDisplayName(displayName);
        toolbar.setSourceUri(sourceUri);
        toolbar.updateMetadata("JavaSound");
    }

    /**
     * {@inheritDoc}
     * <p>Stops active audio playback.</p>
     */
    @Override
    public void stop() {
        stopPlayback();
    }

    /**
     * {@inheritDoc}
     * <p>Disposes of active playback and audio data.</p>
     */
    @Override
    public void dispose() {
        stopPlayback();
        this.audioData = null;
    }
}
