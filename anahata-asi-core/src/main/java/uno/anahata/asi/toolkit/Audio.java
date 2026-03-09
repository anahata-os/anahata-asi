/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.toolkit;

import java.io.File;
import java.net.URI;
import java.net.URL;
import java.util.List;
import java.util.Objects;
import javax.sound.sampled.*;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.toolkit.audio.AudioDevice;
import uno.anahata.asi.agi.message.RagMessage;
import uno.anahata.asi.agi.tool.AiTool;
import uno.anahata.asi.agi.tool.AiToolException;
import uno.anahata.asi.agi.tool.AiToolParam;
import uno.anahata.asi.agi.tool.AiToolkit;
import uno.anahata.asi.agi.tool.AnahataToolkit;

/**
 * The definitive core toolkit for audio operations.
 * <p>
 * This toolkit is autonomous: it manages its own state for selected devices 
 * and implements self-healing defaults on initialization and session restoration.
 * </p>
 */
@Slf4j
@AiToolkit("Tools for managing audio recording and playback across local devices.")
public class Audio extends AnahataToolkit {

    /** The selected audio input device for this session. */
    @Getter
    private AudioDevice selectedInputDevice;

    /** The selected audio output device for this session. */
    @Getter
    private AudioDevice selectedOutputDevice;

    /**
     * {@inheritDoc}
     * <p>Auto-discovers and selects system hardware defaults on startup.</p>
     */
    @Override
    public void initialize() {
        super.initialize();
        log.info("Initializing Audio hardware defaults...");
        initInputLineFromDefault();
        initOutputLineFromDefault();
    }

    /**
     * {@inheritDoc}
     * <p>Verifies that restored hardware devices still exist on the host.</p>
     */
    @Override
    public void rebind() {
        super.rebind();
        
        if (selectedInputDevice != null) {
            AudioDevice live = AudioDevice.findDevice(AudioDevice.Type.INPUT, selectedInputDevice.getId());
            if (live == null) {
                log.warn("Restored input device '{}' not found. Re-detecting default.", selectedInputDevice.getName());
                initInputLineFromDefault();
            }
        } else {
            initInputLineFromDefault();
        }
        
        if (selectedOutputDevice != null) {
            AudioDevice live = AudioDevice.findDevice(AudioDevice.Type.OUTPUT, selectedOutputDevice.getId());
            if (live == null) {
                log.warn("Restored output device '{}' not found. Re-detecting default.", selectedOutputDevice.getName());
                initOutputLineFromDefault();
            }
        } else {
            initOutputLineFromDefault();
        }
    }

    /**
     * Discovers and selects the system's default input hardware.
     */
    private void initInputLineFromDefault() {
        AudioDevice.listAvailableDevices(AudioDevice.Type.INPUT).stream()
                .filter(AudioDevice::isDefaultLine)
                .findFirst()
                .ifPresent(this::setSelectedInputDevice);
    }

    /**
     * Discovers and selects the system's default output hardware.
     */
    private void initOutputLineFromDefault() {
        AudioDevice.listAvailableDevices(AudioDevice.Type.OUTPUT).stream()
                .filter(AudioDevice::isDefaultLine)
                .findFirst()
                .ifPresent(this::setSelectedOutputDevice);
    }

    /**
     * Sets the selected input device and fires a property change event.
     * @param device The device to select.
     */
    public void setSelectedInputDevice(AudioDevice device) {
        AudioDevice old = this.selectedInputDevice;
        if (Objects.equals(old, device)) {
            return;
        }
        this.selectedInputDevice = device;
        propertyChangeSupport.firePropertyChange("selectedInputDevice", old, device);
    }

    /**
     * Sets the selected output device and fires a property change event.
     * @param device The device to select.
     */
    public void setSelectedOutputDevice(AudioDevice device) {
        AudioDevice old = this.selectedOutputDevice;
        if (Objects.equals(old, device)) {
            return;
        }
        this.selectedOutputDevice = device;
        propertyChangeSupport.firePropertyChange("selectedOutputDevice", old, device);
    }

    /** {@inheritDoc} */
    @Override
    public void populateMessage(RagMessage ragMessage) throws Exception {
        StringBuilder sb = new StringBuilder("\n## Audio Hardware Configuration\n");
        
        sb.append("\n### Available Input Lines\n");
        for (AudioDevice device : AudioDevice.listAvailableDevices(AudioDevice.Type.INPUT)) {
            sb.append("- ").append(device.toMarkdown());
            if (device.equals(selectedInputDevice)) {
                sb.append(" <-- **[CURRENTLY SELECTED]**");
            }
            sb.append("\n");
        }

        sb.append("\n### Available Output Lines\n");
        for (AudioDevice device : AudioDevice.listAvailableDevices(AudioDevice.Type.OUTPUT)) {
            sb.append("- ").append(device.toMarkdown());
            if (device.equals(selectedOutputDevice)) {
                sb.append(" <-- **[CURRENTLY SELECTED]**");
            }
            sb.append("\n");
        }
        
        ragMessage.addTextPart(sb.toString());
    }

    /**
     * Selects a specific device for recording for the current session.
     * @param deviceId The unique ID of the input device to select.
     * @return a status message.
     */
    @AiTool("Selects a specific device for recording for the current session.")
    public String selectRecordingDevice(@AiToolParam("The unique ID of the input device to select.") String deviceId) {
        AudioDevice device = AudioDevice.findDevice(AudioDevice.Type.INPUT, deviceId);
        if (device == null) {
            throw new AiToolException("Recording device not found: " + deviceId);
        }
        setSelectedInputDevice(device);
        return "Input device selected: " + device.toMarkdown();
    }

    /**
     * Selects a specific device for playback for the current session.
     * @param deviceId The unique ID of the output device to select.
     * @return a status message.
     */
    @AiTool("Selects a specific device for playback for the current session.")
    public String selectOutputDevice(@AiToolParam("The unique ID of the output device to select.") String deviceId) {
        AudioDevice device = AudioDevice.findDevice(AudioDevice.Type.OUTPUT, deviceId);
        if (device == null) {
            throw new AiToolException("Playback device not found: " + deviceId);
        }
        setSelectedOutputDevice(device);
        return "Output device selected: " + device.toMarkdown();
    }

    /**
     * Records audio from the selected input device for a specified duration.
     * 
     * @param durationSeconds Duration of the recording in seconds.
     * @param deviceId Optional specific device ID (overrides session selection).
     * @return A status message.
     * @throws Exception if hardware access fails.
     */
    @AiTool("Records audio from the selected input device for a specified duration.")
    public String record(@AiToolParam("Duration in seconds.") int durationSeconds,
                         @AiToolParam(value = "Optional specific device ID to use.", required = false) String deviceId) throws Exception {
        
        AudioDevice targetDevice = (deviceId != null) 
                ? AudioDevice.findDevice(AudioDevice.Type.INPUT, deviceId)
                : selectedInputDevice;
        
        try (TargetDataLine line = (targetDevice != null) ? targetDevice.getInputLine(AudioDevice.DEFAULT_FORMAT) : AudioSystem.getTargetDataLine(AudioDevice.DEFAULT_FORMAT)) {
            line.open(AudioDevice.DEFAULT_FORMAT);
            line.start();

            log("Recording started on device: " + (targetDevice != null ? targetDevice.toMarkdown() : "Default"));
            
            byte[] audioData = new byte[AudioDevice.DEFAULT_FORMAT.getFrameSize() * (int) AudioDevice.DEFAULT_FORMAT.getFrameRate() * durationSeconds];
            int totalRead = 0;
            while (totalRead < audioData.length) {
                int read = line.read(audioData, totalRead, audioData.length - totalRead);
                if (read > 0) {
                    totalRead += read;
                }
            }
            
            line.stop();

            File tempFile = File.createTempFile("recording-", ".wav");
            try (AudioInputStream ais = new AudioInputStream(new java.io.ByteArrayInputStream(audioData), AudioDevice.DEFAULT_FORMAT, audioData.length / AudioDevice.DEFAULT_FORMAT.getFrameSize())) {
                AudioSystem.write(ais, AudioFileFormat.Type.WAVE, tempFile);
            }

            addAttachment(tempFile);
            return "Recording complete. Attached to message.";
        }
    }

    /**
     * Plays an audio file from a URI on the selected output device.
     * @param uri The URI string.
     * @param deviceId Optional specific device ID (overrides session selection).
     * @return a status message.
     * @throws Exception if playback fails.
     */
    @AiTool("Plays an audio file on the selected output device. Supports both local paths and remote URLs.")
    public String play(@AiToolParam("The URI to the audio file.") String uri,
                       @AiToolParam(value = "Optional specific device ID to use.", required = false) String deviceId) throws Exception {
        
        URL url = URI.create(uri).toURL();
        AudioDevice targetDevice = (deviceId != null)
                ? AudioDevice.findDevice(AudioDevice.Type.OUTPUT, deviceId)
                : selectedOutputDevice;

        try (AudioInputStream ais = AudioSystem.getAudioInputStream(url);
             SourceDataLine line = (targetDevice != null) ? targetDevice.getOutputLine(ais.getFormat()) : AudioSystem.getSourceDataLine(ais.getFormat())) {
            
            line.open(ais.getFormat());
            line.start();
            
            log("Playing: " + url + " on device: " + (targetDevice != null ? targetDevice.getName() : "Default"));
            byte[] buffer = new byte[4096];
            int read;
            while ((read = ais.read(buffer)) != -1) {
                line.write(buffer, 0, read);
            }
            line.drain();
            return "Playback finished.";
        }
    }
}
