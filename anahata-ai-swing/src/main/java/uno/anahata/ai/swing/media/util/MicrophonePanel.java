package uno.anahata.ai.swing.media.util;

import java.awt.Component;
import java.awt.FlowLayout;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Consumer;
import java.util.stream.Collectors;
import javax.sound.sampled.AudioFileFormat;
import javax.sound.sampled.AudioFormat;
import javax.sound.sampled.AudioInputStream;
import javax.sound.sampled.AudioSystem;
import javax.sound.sampled.DataLine;
import javax.sound.sampled.Line;
import javax.sound.sampled.LineUnavailableException;
import javax.sound.sampled.Mixer;
import javax.sound.sampled.TargetDataLine;
import javax.swing.BoxLayout;
import javax.swing.DefaultListCellRenderer;
import javax.swing.JButton;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.JToggleButton;
import javax.swing.SwingUtilities;
import lombok.extern.slf4j.Slf4j;
import org.jdesktop.swingx.JXComboBox;
import uno.anahata.ai.chat.Chat;
import uno.anahata.ai.model.core.InputUserMessage;
import uno.anahata.ai.swing.chat.InputPanel;
import uno.anahata.ai.swing.icons.IconUtils;
import uno.anahata.ai.swing.icons.MicrophoneIcon;
import uno.anahata.ai.swing.icons.RecordingIcon;
import uno.anahata.ai.swing.internal.SwingTask;
import uno.anahata.ai.swing.internal.SwingUtils;

/**
 * A panel that encapsulates all microphone-related UI components and logic.
 * This includes a microphone button, a dropdown for selecting input lines,
 * and a progress bar to show audio levels.
 *
 * @author pablo
 */
@Slf4j
public final class MicrophonePanel extends JPanel {

    private final InputPanel parentPanel;
    private final JToggleButton micButton;
    private final JXComboBox microphoneLineComboBox;
    private final JProgressBar levelBar;

    
    private TargetDataLine targetDataLine;
    private final AtomicBoolean recording = new AtomicBoolean(false);
    private ByteArrayOutputStream byteArrayOutputStream;

    public MicrophonePanel(InputPanel parentPanel) {
        super(new FlowLayout(FlowLayout.LEFT, 5, 0)); // Use FlowLayout for horizontal arrangement
        this.parentPanel = parentPanel;
        
        micButton = new JToggleButton(new MicrophoneIcon(24));
        micButton.setSelectedIcon(new RecordingIcon(24));
        micButton.setToolTipText("Click to start/stop recording");
        micButton.addActionListener(e -> toggleRecording());
        micButton.setEnabled(false); // Disable until lines are loaded

        microphoneLineComboBox = new JXComboBox(); // Removed generic type parameters
        microphoneLineComboBox.setToolTipText("Select microphone input line");
        microphoneLineComboBox.setEnabled(false); // Disable until lines are loaded
        microphoneLineComboBox.setRenderer(new LineInfoRenderer());
        microphoneLineComboBox.addActionListener(e -> {
            if (!micButton.isSelected()) { // Only allow changing if not recording
                LineInfo selectedLineInfo = (LineInfo) microphoneLineComboBox.getSelectedItem();
                if (selectedLineInfo != null) {
                    setTargetDataLine(selectedLineInfo.getTargetDataLine());
                }
            }
        });
        
        levelBar = new JProgressBar(0, 100);
        levelBar.setStringPainted(false); // Don't show percentage text
        levelBar.setPreferredSize(new java.awt.Dimension(100, 20)); // Set a preferred size
        levelBar.setVisible(false); // Initially hidden

        add(micButton);
        add(microphoneLineComboBox);
        add(levelBar);

        initMicrophoneLineComboBox();
    }

    public boolean isRecording() {
        return recording.get();
    }
    
    public void setMicrophoneComponentsEnabled(boolean enabled) {
        micButton.setEnabled(enabled);
        microphoneLineComboBox.setEnabled(enabled && !micButton.isSelected());
    }

    private void setTargetDataLine(TargetDataLine newTargetDataLine) {
        if (recording.get()) {
            throw new IllegalStateException("Cannot change microphone line while recording is in progress.");
        }
        if (this.targetDataLine != null && this.targetDataLine.isOpen()) {
            this.targetDataLine.close();
        }
        this.targetDataLine = newTargetDataLine;
    }

    public void startRecording() throws LineUnavailableException {
        if (recording.get()) {
            throw new IllegalStateException("Recording is already in progress");
        }
        targetDataLine.open(SoundUtils.RECORDING_FORMAT);
        targetDataLine.start();
        recording.set(true);
        byteArrayOutputStream = new ByteArrayOutputStream();
        
        // Recording now happens on the calling thread (SwingWorker's doInBackground)
        byte[] buffer = new byte[4096];
        int bytesRead;
        try {
            while (recording.get()) {
                bytesRead = targetDataLine.read(buffer, 0, buffer.length);
                if (bytesRead > 0) {
                    byteArrayOutputStream.write(buffer, 0, bytesRead);
                    double rms = SoundUtils.calculateRMS(buffer, bytesRead);
                    SwingUtilities.invokeLater(() -> levelBar.setValue((int)(rms * 100)));
                }
            }
        } catch (Exception e) {
            log.error("Error during audio recording buffer read", e);
        }
    }

    public File stopRecording() throws IOException {
        if (!recording.get()) {
            log.warn("Stop recording called, but recording was not in progress.");
            return null;
        }
        recording.set(false); 
        
        targetDataLine.stop();
        targetDataLine.close();
        
        File tempFile = File.createTempFile("recording", ".wav");
        
        byte[] audioData = byteArrayOutputStream.toByteArray();
        try (ByteArrayInputStream bais = new ByteArrayInputStream(audioData);
             AudioInputStream ais = new AudioInputStream(bais, SoundUtils.RECORDING_FORMAT, audioData.length / SoundUtils.RECORDING_FORMAT.getFrameSize())) {
            AudioSystem.write(ais, AudioFileFormat.Type.WAVE, tempFile);
        }
        return tempFile;
    }
    
    
    
    private void initMicrophoneLineComboBox() {
        new SwingTask<List<LineInfo>>("Load Microphone Lines",
            () -> SoundUtils.getAvailableRecordingLines(),
            (lines) -> {
                if (lines != null && !lines.isEmpty()) {
                    for (LineInfo lineInfo : lines) {
                        microphoneLineComboBox.addItem(lineInfo);
                    }
                    microphoneLineComboBox.setEnabled(true);
                    micButton.setEnabled(true);
                    
                    // Select the first item in the list and initialize the microphone
                    microphoneLineComboBox.setSelectedIndex(0);
                    setTargetDataLine(lines.get(0).getTargetDataLine());
                } else {
                    micButton.setEnabled(false);
                    microphoneLineComboBox.setEnabled(false);
                    micButton.setToolTipText("No microphone lines found.");
                }
            },
            (error) -> {
                log.error("Failed to load microphone lines", error);
                micButton.setEnabled(false);
                microphoneLineComboBox.setEnabled(false);
                micButton.setToolTipText("Error loading microphone lines: " + error.getMessage());
                SwingUtils.showException("Load Microphone Lines", "Failed to load microphone lines", error);
            }
        ).execute();
    }
    
    /**
     * Toggles microphone recording on or off.
     */
    public void toggleRecording() {
        if (micButton.isSelected()) {
            // Start recording
            LineInfo selectedLineInfo = (LineInfo) microphoneLineComboBox.getSelectedItem();
            if (selectedLineInfo == null) {
                log.warn("No microphone line selected. Cannot start recording.");
                micButton.setSelected(false);
                return;
            }
            setTargetDataLine(selectedLineInfo.getTargetDataLine());
            microphoneLineComboBox.setEnabled(false);
            microphoneLineComboBox.setVisible(false); // Hide combo box while recording
            levelBar.setVisible(true); // Show progress bar
            new SwingTask<Void> (
                "Start Recording",
                () -> {
                    startRecording();
                    return null;
                }
            ).execute();
        } else {
            // Stop recording
            if (targetDataLine == null) {
                log.warn("Microphone not initialized. Cannot stop recording.");
                return;
            }
            microphoneLineComboBox.setEnabled(true);
            microphoneLineComboBox.setVisible(true); // Show combo box after recording stops
            levelBar.setVisible(false); // Hide progress bar
            new SwingTask<Void> (
                "Stop Recording",
                () -> {
                    File audioFile = stopRecording();
                    if (audioFile != null) {
                        parentPanel.getCurrentMessage().addAttachment(audioFile.toPath());
                    }
                    return null; // Return Void
                },
                (v) -> {
                    // On Success (UI Thread)
                    parentPanel.getInputMessageRenderer().render(); // Refresh preview
                    log.info("Recording stopped. Audio attached.");
                }
            ).execute();
        }
    }
}
