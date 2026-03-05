/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.media.util;

import java.awt.FlowLayout;
import java.io.BufferedInputStream;
import java.io.ByteArrayInputStream;
import java.util.List;
import java.util.concurrent.Future;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Consumer;
import javax.sound.sampled.AudioInputStream;
import javax.sound.sampled.AudioSystem;
import javax.sound.sampled.Clip;
import javax.sound.sampled.DataLine;
import javax.sound.sampled.LineEvent;
import javax.sound.sampled.Mixer;
import javax.sound.sampled.SourceDataLine;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.SwingUtilities;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import org.jdesktop.swingx.JXComboBox;
import uno.anahata.asi.model.audio.AudioDevice;
import uno.anahata.asi.model.audio.AudioUtils;
import uno.anahata.asi.swing.agi.AgiPanel;
import uno.anahata.asi.swing.internal.EdtPropertyChangeListener;
import uno.anahata.asi.swing.internal.SwingTask;
import uno.anahata.asi.toolkit.Audio;

/**
 * A reactive panel that encapsulates audio playback functionality.
 * <p>
 * This panel is a high-salience observer of the autonomous {@link Audio} toolkit. 
 * It synchronizes the UI dropdown with the session's selected output device 
 * and provides the engine for notification sounds and tool-invoked playback.
 * </p>
 */
@Slf4j
@Getter
public final class AudioPlaybackPanel extends JPanel {

    /** The parent agi panel providing session context. */
    private final AgiPanel agiPanel;
    /** The dropdown for selecting the hardware playback line. */
    private final JXComboBox playbackLineComboBox;
    /** Real-time volume meter for playback feedback. */
    private final JProgressBar playbackLevelBar;

    /** Flag indicating if audio is currently being played. */
    private final AtomicBoolean playing = new AtomicBoolean(false);
    /** Reference to a short notification clip currently in flight. */
    private volatile Clip currentClip;
    
    /** Listener for toolkit-driven device selection changes. */
    private EdtPropertyChangeListener toolkitListener;

    /**
     * Constructs a new AudioPlaybackPanel and binds it to the Audio toolkit.
     * 
     * @param agiPanel The parent AgiPanel.
     */
    public AudioPlaybackPanel(AgiPanel agiPanel) {
        super(new FlowLayout(FlowLayout.RIGHT, 5, 0));
        this.agiPanel = agiPanel;

        playbackLineComboBox = new JXComboBox();
        playbackLineComboBox.setToolTipText("Select audio playback line");
        playbackLineComboBox.setEnabled(false);
        playbackLineComboBox.addActionListener(e -> {
            Audio toolkit = getAudioToolkit();
            AudioDevice selected = (AudioDevice) playbackLineComboBox.getSelectedItem();
            if (toolkit != null && selected != null && !selected.equals(toolkit.getSelectedOutputDevice())) {
                toolkit.setSelectedOutputDevice(selected);
            }
        });

        playbackLevelBar = new JProgressBar(0, 100);
        playbackLevelBar.setStringPainted(false);
        playbackLevelBar.setPreferredSize(new java.awt.Dimension(100, 20));
        playbackLevelBar.setVisible(false);

        add(playbackLineComboBox);
        add(playbackLevelBar);

        bindToToolkit();
        initPlaybackLineComboBox();
    }

    /**
     * Resolves the autonomous Audio toolkit from the session.
     * @return The toolkit instance or null.
     */
    private Audio getAudioToolkit() {
        return agiPanel.getAgi().getToolkit(Audio.class).orElse(null);
    }

    /**
     * Binds the UI listener to the toolkit's property change support.
     */
    private void bindToToolkit() {
        Audio toolkit = getAudioToolkit();
        if (toolkit != null) {
            this.toolkitListener = new EdtPropertyChangeListener(this, toolkit, "selectedOutputDevice", evt -> {
                AudioDevice newDevice = (AudioDevice) evt.getNewValue();
                if (newDevice != null && !newDevice.equals(playbackLineComboBox.getSelectedItem())) {
                    playbackLineComboBox.setSelectedItem(newDevice);
                }
            });
        }
    }

    /**
     * Plays a short, non-blocking notification sound from the application's resources.
     * 
     * @param resourceName The name of the sound file (e.g., "idle.wav").
     */
    public void playSound(final String resourceName) {
        agiPanel.getAgi().getExecutor().submit(() -> {
            var stream = AudioPlaybackPanel.class.getResourceAsStream("/sounds/" + resourceName);
            if (stream == null) {
                log.warn("Sound resource not found: {}", resourceName);
                return;
            }
            
            Audio toolkit = getAudioToolkit();
            AudioDevice configDevice = (toolkit != null) ? toolkit.getSelectedOutputDevice() : null;
            Mixer.Info mixerInfo = (configDevice != null) ? configDevice.getMixerInfo() : null;
            
            try (var bis = new BufferedInputStream(stream);
                 AudioInputStream inputStream = AudioSystem.getAudioInputStream(bis)) {
                
                Clip clip;
                if (mixerInfo != null) {
                    Mixer mixer = AudioSystem.getMixer(mixerInfo);
                    clip = (Clip) mixer.getLine(new DataLine.Info(Clip.class, inputStream.getFormat()));
                } else {
                    clip = AudioSystem.getClip();
                }
                
                clip.addLineListener(event -> {
                    if (event.getType() == LineEvent.Type.STOP) {
                        clip.close();
                    }
                });
                clip.open(inputStream);
                clip.start();
                this.currentClip = clip;
            } catch (Exception e) {
                log.warn("Could not play sound resource: {} on device: {}", resourceName, configDevice, e);
            }
        });
    }

    /**
     * Starts a toggleable audio playback from a byte array.
     * 
     * @param data The PCM audio data.
     * @param onStatusChange Callback for play/stop events.
     * @return A stopper Runnable that cancels the playback task.
     */
    public Runnable playToggleable(byte[] data, Consumer<Boolean> onStatusChange) {
        Audio toolkit = getAudioToolkit();
        AudioDevice device = (toolkit != null) ? toolkit.getSelectedOutputDevice() : null;

        AtomicBoolean taskPlaying = new AtomicBoolean(true);
        Future<?> future = agiPanel.getAgi().getExecutor().submit(() -> {
            playing.set(true);
            SwingUtilities.invokeLater(() -> {
                playbackLevelBar.setVisible(true);
                if (onStatusChange != null) {
                    onStatusChange.accept(true);
                }
            });
            
            try (AudioInputStream ais = AudioSystem.getAudioInputStream(new ByteArrayInputStream(data));
                 SourceDataLine line = (device != null) ? device.getOutputLine(ais.getFormat()) : AudioSystem.getSourceDataLine(ais.getFormat())) {
                
                line.open(ais.getFormat());
                line.start();

                byte[] buffer = new byte[4096];
                int read;
                while (taskPlaying.get() && (read = ais.read(buffer, 0, buffer.length)) != -1) {
                    line.write(buffer, 0, read);
                    double rms = AudioUtils.calculateRMS(buffer, read);
                    SwingUtilities.invokeLater(() -> playbackLevelBar.setValue((int) (rms * 100)));
                }
                line.drain();
            } catch (Exception e) {
                log.error("Error during reactive audio playback on device: {}", device, e);
            } finally {
                playing.set(false);
                SwingUtilities.invokeLater(() -> {
                    playbackLevelBar.setValue(0);
                    playbackLevelBar.setVisible(false);
                    if (onStatusChange != null) {
                        onStatusChange.accept(false);
                    }
                });
            }
        });
        
        return () -> {
            taskPlaying.set(false);
            future.cancel(true);
        };
    }

    /** Stops all currently playing notification sounds. */
    public void stop() {
        Clip clip = currentClip;
        if (clip != null && clip.isRunning()) {
            clip.stop();
            clip.close();
            currentClip = null;
        }
        playing.set(false);
    }

    /**
     * Discovers available playback hardware and populates the UI dropdown.
     */
    private void initPlaybackLineComboBox() {
        new SwingTask<List<AudioDevice>>(
            this,
            "Load Playback Devices",
            () -> AudioDevice.listAvailableDevices(AudioDevice.Type.OUTPUT),
            (devices) -> {
                playbackLineComboBox.removeAllItems();
                for (AudioDevice d : devices) {
                    playbackLineComboBox.addItem(d);
                }
                playbackLineComboBox.setEnabled(true);

                // Initial Selection from Toolkit
                Audio toolkit = getAudioToolkit();
                if (toolkit != null && toolkit.getSelectedOutputDevice() != null) {
                    playbackLineComboBox.setSelectedItem(toolkit.getSelectedOutputDevice());
                }
            },
            (error) -> {
                playbackLineComboBox.setEnabled(false);
                playbackLineComboBox.setToolTipText("Error: " + ((Exception) error).getMessage());
            }
        ).execute();
    }
}
