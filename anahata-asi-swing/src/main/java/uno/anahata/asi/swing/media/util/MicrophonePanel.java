/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.media.util;

import java.awt.FlowLayout;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.concurrent.atomic.AtomicBoolean;
import javax.sound.sampled.AudioFileFormat;
import javax.sound.sampled.AudioInputStream;
import javax.sound.sampled.AudioSystem;
import javax.sound.sampled.LineUnavailableException;
import javax.sound.sampled.TargetDataLine;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.JToggleButton;
import javax.swing.SwingUtilities;
import lombok.extern.slf4j.Slf4j;
import org.jdesktop.swingx.JXComboBox;
import uno.anahata.asi.model.audio.AudioDevice;
import uno.anahata.asi.model.audio.AudioUtils;
import uno.anahata.asi.swing.agi.InputPanel;
import uno.anahata.asi.swing.icons.MicrophoneIcon;
import uno.anahata.asi.swing.icons.RecordingIcon;
import uno.anahata.asi.swing.internal.EdtPropertyChangeListener;
import uno.anahata.asi.swing.internal.SwingTask;
import uno.anahata.asi.toolkit.Audio;

/**
 * A reactive panel that encapsulates microphone selection and recording logic.
 * <p>
 * This panel is a high-salience observer of the autonomous {@link Audio} toolkit. 
 * It synchronizes its state with the session's selected input device and provides 
 * real-time visual feedback of recording levels.
 * </p>
 *
 * @author anahata
 */
@Slf4j
public final class MicrophonePanel extends JPanel {

    /** The parent input panel for attaching recordings to messages. */
    private final InputPanel parentPanel;
    /** Toggle button to start/stop recording. */
    private final JToggleButton micButton;
    /** Dropdown for selecting the hardware input channel. */
    private final JXComboBox microphoneLineComboBox;
    /** Real-time volume meter for recording feedback. */
    private final JProgressBar levelBar;
    
    /** Flag indicating if recording is currently active. */
    private final AtomicBoolean recording = new AtomicBoolean(false);
    /** Buffer for accumulating raw PCM data during recording. */
    private ByteArrayOutputStream recordingBuffer;
    /** Listener for toolkit-driven device selection changes. */
    private EdtPropertyChangeListener toolkitListener;

    /**
     * Constructs a new MicrophonePanel and binds it to the session config.
     * 
     * @param parentPanel The parent InputPanel.
     */
    public MicrophonePanel(InputPanel parentPanel) {
        super(new FlowLayout(FlowLayout.LEFT, 5, 0));
        this.parentPanel = parentPanel;
        
        micButton = new JToggleButton(new MicrophoneIcon(24));
        micButton.setSelectedIcon(new RecordingIcon(24));
        micButton.setToolTipText("Click to start/stop recording");
        micButton.addActionListener(e -> toggleRecording());
        micButton.setEnabled(false);

        microphoneLineComboBox = new JXComboBox();
        microphoneLineComboBox.setToolTipText("Select microphone input line");
        microphoneLineComboBox.setEnabled(false);
        microphoneLineComboBox.addActionListener(e -> {
            if (!micButton.isSelected()) {
                Audio toolkit = getAudioToolkit();
                AudioDevice selected = (AudioDevice) microphoneLineComboBox.getSelectedItem();
                if (toolkit != null && selected != null && !selected.equals(toolkit.getSelectedInputDevice())) {
                    toolkit.setSelectedInputDevice(selected);
                }
            }
        });
        
        levelBar = new JProgressBar(0, 100);
        levelBar.setStringPainted(false);
        levelBar.setPreferredSize(new java.awt.Dimension(100, 20));
        levelBar.setVisible(false);

        add(micButton);
        add(microphoneLineComboBox);
        add(levelBar);

        bindToToolkit();
        initMicrophoneLineComboBox();
    }

    /**
     * Resolves the autonomous Audio toolkit from the session.
     * @return The toolkit instance or null.
     */
    private Audio getAudioToolkit() {
        return parentPanel.getAgi().getToolManager().getToolkitInstance(Audio.class).orElse(null);
    }

    /**
     * Binds the UI listener to the toolkit's property change support.
     */
    private void bindToToolkit() {
        Audio toolkit = getAudioToolkit();
        if (toolkit != null) {
            this.toolkitListener = new EdtPropertyChangeListener(this, toolkit, "selectedInputDevice", evt -> {
                AudioDevice newDevice = (AudioDevice) evt.getNewValue();
                if (newDevice != null && !newDevice.equals(microphoneLineComboBox.getSelectedItem())) {
                    microphoneLineComboBox.setSelectedItem(newDevice);
                }
            });
        }
    }

    /** @return true if the microphone is currently recording. */
    public boolean isRecording() {
        return recording.get();
    }
    
    /**
     * Enables or disables the UI components.
     * @param enabled true to enable.
     */
    public void setMicrophoneComponentsEnabled(boolean enabled) {
        micButton.setEnabled(enabled);
        microphoneLineComboBox.setEnabled(enabled && !micButton.isSelected());
    }

    /**
     * Toggles recording on or off based on the button state.
     */
    public void toggleRecording() {
        if (micButton.isSelected()) {
            startRecordingWorkflow();
        } else {
            stopRecordingWorkflow();
        }
    }

    /**
     * Initializes the hardware line and starts the recording loop on the Agi executor.
     */
    private void startRecordingWorkflow() {
        AudioDevice device = (AudioDevice) microphoneLineComboBox.getSelectedItem();
        if (device == null) {
            micButton.setSelected(false);
            return;
        }

        microphoneLineComboBox.setEnabled(false);
        microphoneLineComboBox.setVisible(false);
        levelBar.setVisible(true);

        parentPanel.getAgi().getExecutor().submit(() -> {
            try (TargetDataLine line = device.getInputLine(AudioDevice.DEFAULT_FORMAT)) {
                line.open(AudioDevice.DEFAULT_FORMAT);
                line.start();
                recording.set(true);
                recordingBuffer = new ByteArrayOutputStream();
                
                log.info("Recording started on device: {}", device.getName());
                
                byte[] buffer = new byte[4096];
                int read;
                while (recording.get()) {
                    read = line.read(buffer, 0, buffer.length);
                    if (read > 0) {
                        recordingBuffer.write(buffer, 0, read);
                        double rms = AudioUtils.calculateRMS(buffer, read);
                        SwingUtilities.invokeLater(() -> levelBar.setValue((int) (rms * 100)));
                    }
                }
                
                line.stop();
            } catch (LineUnavailableException e) {
                log.error("Microphone hardware unavailable: {}", device.getName(), e);
                SwingUtilities.invokeLater(() -> {
                    micButton.setSelected(false);
                    stopRecordingWorkflow();
                });
            }
        });
    }

    /**
     * Signals the recording loop to stop and packages the result into a WAVE file.
     */
    private void stopRecordingWorkflow() {
        recording.set(false);
        microphoneLineComboBox.setEnabled(true);
        microphoneLineComboBox.setVisible(true);
        levelBar.setVisible(false);
        levelBar.setValue(0);

        new SwingTask<File>(
            this,
            "Finalize Recording",
            () -> {
                if (recordingBuffer == null) {
                    return null;
                }
                byte[] data = recordingBuffer.toByteArray();
                File tempFile = File.createTempFile("recording-", ".wav");
                try (AudioInputStream ais = new AudioInputStream(new java.io.ByteArrayInputStream(data), AudioDevice.DEFAULT_FORMAT, data.length / AudioDevice.DEFAULT_FORMAT.getFrameSize())) {
                    AudioSystem.write(ais, AudioFileFormat.Type.WAVE, tempFile);
                }
                return tempFile;
            },
            (audioFile) -> {
                if (audioFile != null) {
                    try {
                        parentPanel.getCurrentMessage().addAttachment(audioFile);
                        parentPanel.getInputMessagePreview().render();
                    } catch (Exception e) {
                        log.error("Failed to attach recording to message", e);
                    }
                }
            }
        ).execute();
    }
    
    /**
     * Discovers available input hardware and populates the UI dropdown.
     */
    private void initMicrophoneLineComboBox() {
        new SwingTask<List<AudioDevice>>(
            this,
            "Load Microphone Devices",
            () -> AudioDevice.listAvailableDevices(AudioDevice.Type.INPUT),
            (devices) -> {
                microphoneLineComboBox.removeAllItems();
                for (AudioDevice d : devices) {
                    microphoneLineComboBox.addItem(d);
                }
                microphoneLineComboBox.setEnabled(true);
                micButton.setEnabled(true);

                // Initial Selection from Toolkit
                Audio toolkit = getAudioToolkit();
                if (toolkit != null && toolkit.getSelectedInputDevice() != null) {
                    microphoneLineComboBox.setSelectedItem(toolkit.getSelectedInputDevice());
                }
            },
            (error) -> {
                micButton.setEnabled(false);
                microphoneLineComboBox.setEnabled(false);
                microphoneLineComboBox.setToolTipText("Hardware Error: " + ((Exception) error).getMessage());
            }
        ).execute();
    }
}
