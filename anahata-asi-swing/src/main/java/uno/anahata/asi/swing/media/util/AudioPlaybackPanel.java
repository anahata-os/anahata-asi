package uno.anahata.asi.swing.media.util;

import java.awt.FlowLayout;
import java.io.BufferedInputStream;
import java.io.ByteArrayInputStream;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Consumer;
import javax.sound.sampled.AudioInputStream;
import javax.sound.sampled.AudioSystem;
import javax.sound.sampled.Clip;
import javax.sound.sampled.LineEvent;
import javax.sound.sampled.LineListener;
import javax.sound.sampled.SourceDataLine;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.SwingUtilities;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import org.jdesktop.swingx.JXComboBox;
import uno.anahata.asi.AiExecutors;
import uno.anahata.asi.swing.agi.AgiPanel; // Added import
import uno.anahata.asi.swing.internal.SwingTask;
import uno.anahata.asi.swing.internal.SwingUtils;

/**
 * A panel that encapsulates all audio playback functionality, including selecting
 * output lines, displaying a progress bar, and playing various audio types.
 */
@Slf4j
@Getter
public final class AudioPlaybackPanel extends JPanel {

    private final AgiPanel agiPanel; // New: Reference to the parent AgiPanel
    private final JXComboBox playbackLineComboBox;
    private final JProgressBar playbackLevelBar;

    private final ExecutorService audioExecutor;
    private SourceDataLine currentPlaybackLine;
    private final AtomicBoolean playing = new AtomicBoolean(false);
    private volatile Clip currentClip; // For resource sounds. Made volatile for thread-safety.

    public AudioPlaybackPanel(AgiPanel agiPanel) { // Modified constructor
        super(new FlowLayout(FlowLayout.RIGHT, 5, 0)); // Align to the right
        this.agiPanel = agiPanel; // Initialize agiPanel
        this.audioExecutor = AiExecutors.newCachedThreadPoolExecutor("audio-player");

        playbackLineComboBox = new JXComboBox();
        playbackLineComboBox.setToolTipText("Select audio playback line");
        playbackLineComboBox.setEnabled(false); // Disable until lines are loaded
        playbackLineComboBox.setRenderer(new LineInfoRenderer());
        playbackLineComboBox.addActionListener(e -> {
            LineInfo selectedLineInfo = (LineInfo) playbackLineComboBox.getSelectedItem();
            if (selectedLineInfo != null) {
                setPlaybackLine(selectedLineInfo.getSourceDataLine());
            }
        });

        playbackLevelBar = new JProgressBar(0, 100);
        playbackLevelBar.setStringPainted(false); // Don't show percentage text
        playbackLevelBar.setPreferredSize(new java.awt.Dimension(100, 20)); // Set a preferred size
        playbackLevelBar.setVisible(false); // Initially hidden

        add(playbackLineComboBox);
        add(playbackLevelBar);

        initPlaybackLineComboBox();
    }

    /**
     * Sets the playback line for this AudioPlaybackPanel instance.
     * If audio is currently playing, it will be stopped before changing the line.
     * @param newLine The new SourceDataLine to use for playback.
     */
    public void setPlaybackLine(SourceDataLine newLine) {
        stop(); // Stop any current playback before changing the line
        if (this.currentPlaybackLine != null && this.currentPlaybackLine.isOpen()) {
            this.currentPlaybackLine.close();
        }
        this.currentPlaybackLine = newLine;
    }

    /**
     * Plays a short, non-blocking notification sound from the application's
     * resources.
     * <p>
     * The sound file is expected to be located in the {@code /sounds/}
     * directory within the classpath. Playback is handled on a dedicated
     * background thread.
     *
     * @param resourceName The simple name of the sound file to play (e.g.,
     * "idle.wav").
     */
    public void playSound(final String resourceName) {
        audioExecutor.submit(() -> {
            var resource = AudioPlaybackPanel.class.getResourceAsStream("/sounds/" + resourceName);
            if (resource == null) {
                log.warn("Sound resource not found: {}", resourceName);
                return;
            }
            
            // CRITICAL FIX: Wrap the resource stream in a BufferedInputStream to support mark/reset,
            // which is required by AudioSystem.getAudioInputStream when running from a JAR (standalone).
            try (var bis = new BufferedInputStream(resource);
                 AudioInputStream inputStream = AudioSystem.getAudioInputStream(bis)) {
                // Use a new Clip for each sound resource to avoid conflicts
                Clip clip = AudioSystem.getClip();
                clip.addLineListener(event -> {
                    if (event.getType() == LineEvent.Type.STOP) {
                        clip.close();
                    }
                });
                clip.open(inputStream);
                clip.start();
                this.currentClip = clip; // Keep track of the last played clip
            } catch (Exception e) {
                log.warn("Could not play sound resource: {}", resourceName, e);
            }
        });
    }

    /**
     * Prepares and starts a toggleable audio playback from a byte array.
     * The onPlaybackStatusChange consumer will be called with true when playback starts
     * and false when it stops (either by explicit stop or natural end).
     * @param data The audio data as a byte array.
     * @param onPlaybackStatusChange A consumer to be notified of playback status changes (true for playing, false for stopped).
     * @return A Runnable that can be called to stop the playback.
     */
    public Runnable playToggleable(byte[] data, Consumer<Boolean> onPlaybackStatusChange) {
        if (currentPlaybackLine == null) {
            log.warn("No playback line selected. Cannot play audio.");
            if (onPlaybackStatusChange != null) {
                onPlaybackStatusChange.accept(false);
            }
            return () -> {}; // Return a no-op runnable
        }

        AtomicBoolean taskPlaying = new AtomicBoolean(true); // Specific to this playback task
        Future<?> future = audioExecutor.submit(() -> {
            playing.set(true); // Global playing state
            SwingUtilities.invokeLater(() -> playbackLevelBar.setVisible(true));
            if (onPlaybackStatusChange != null) {
                SwingUtilities.invokeLater(() -> onPlaybackStatusChange.accept(true));
            }
            
            try (AudioInputStream ais = AudioSystem.getAudioInputStream(new ByteArrayInputStream(data))) {
                // Use the AudioInputStream's format for playback
                currentPlaybackLine.open(ais.getFormat());
                currentPlaybackLine.start();

                byte[] buffer = new byte[4096];
                int bytesRead;
                while (taskPlaying.get() && (bytesRead = ais.read(buffer, 0, buffer.length)) != -1) {
                    currentPlaybackLine.write(buffer, 0, bytesRead);
                    double rms = SoundUtils.calculateRMS(buffer, bytesRead);
                    SwingUtilities.invokeLater(() -> playbackLevelBar.setValue((int) (rms * 100)));
                }
                currentPlaybackLine.drain();
            } catch (Exception e) { // Catch generic Exception to handle LineUnavailableException, IOException, etc.
                log.error("Error during audio playback", e);
            } finally {
                currentPlaybackLine.stop();
                currentPlaybackLine.close();
                playing.set(false); // Global playing state
                SwingUtilities.invokeLater(() -> {
                    playbackLevelBar.setValue(0);
                    playbackLevelBar.setVisible(false);
                }); // Reset and hide progress bar
                if (onPlaybackStatusChange != null) {
                    SwingUtilities.invokeLater(() -> onPlaybackStatusChange.accept(false));
                }
            }
        });
        
        return () -> {
            taskPlaying.set(false); // Stop this specific task
            future.cancel(true); // Attempt to interrupt the task
        };
    }

    /**
     * Stops any currently playing audio (both resource Clips and streamed playback).
     */
    public void stop() {
        Clip clip = currentClip;
        if (clip != null && clip.isRunning()) {
            clip.stop();
            clip.close();
            currentClip = null;
        }
        if (playing.get()) {
            playing.set(false);
            // The playback thread will handle closing the SourceDataLine
        }
    }

    private void initPlaybackLineComboBox() {
        new SwingTask<>(
            this, // Pass 'this' as the owner
            "Load Playback Lines",
            () -> SoundUtils.getAvailablePlaybackLines(),
            (lines) -> {
                if (lines != null && !lines.isEmpty()) {
                    for (LineInfo lineInfo : lines) {
                        playbackLineComboBox.addItem(lineInfo);
                    }
                    playbackLineComboBox.setEnabled(true);

                    // Select the first item in the list and initialize the playback line
                    playbackLineComboBox.setSelectedIndex(0);
                    setPlaybackLine(lines.get(0).getSourceDataLine());
                } else {
                    playbackLineComboBox.setEnabled(false);
                    playbackLineComboBox.setToolTipText("No playback lines found.");
                }
            },
            (error) -> {
                playbackLineComboBox.setEnabled(false);
                playbackLineComboBox.setToolTipText("Error loading playback lines: " + ((Exception) error).getMessage());
            }
        ).execute();
    }
}