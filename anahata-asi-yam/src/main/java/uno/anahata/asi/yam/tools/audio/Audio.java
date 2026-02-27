/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.yam.tools.audio;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import javax.sound.sampled.*;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.AllArgsConstructor;
import uno.anahata.asi.tool.AiTool;
import uno.anahata.asi.tool.AiToolParam;
import uno.anahata.asi.tool.AiToolkit;
import uno.anahata.asi.tool.AnahataToolkit;

/**
 * A toolkit for audio recording and playback, supporting multiple devices.
 */
@AiToolkit("Tools for managing audio recording and playback across local devices.")
public class Audio extends AnahataToolkit {

    private static final AudioFormat FORMAT = new AudioFormat(16000, 16, 1, true, true);

    @Override
    public void populateMessage(uno.anahata.asi.model.core.RagMessage ragMessage) throws Exception {
        List<AudioDevice> devices = listDevices();
        StringBuilder sb = new StringBuilder("\n## Available Audio Devices\n");
        sb.append("Current selected input: `").append(getAgi().getConfig().getSelectedInputDeviceId()).append("`\n");
        sb.append("Current selected output: `").append(getAgi().getConfig().getSelectedOutputDeviceId()).append("`\n\n");
        
        for (AudioDevice device : devices) {
            sb.append("- ").append(device.toString()).append("\n");
        }
        ragMessage.addTextPart(sb.toString());
    }

    /**
     * Lists all available audio input and output devices.
     * @return a list of AudioDevice objects.
     */
    @AiTool("Lists all available audio input and output devices.")
    public List<AudioDevice> listDevices() {
        List<AudioDevice> devices = new ArrayList<>();
        Mixer.Info[] mixerInfos = AudioSystem.getMixerInfo();
        
        String selectedInput = getAgi().getConfig().getSelectedInputDeviceId();
        String selectedOutput = getAgi().getConfig().getSelectedOutputDeviceId();

        for (Mixer.Info info : mixerInfos) {
            Mixer mixer = AudioSystem.getMixer(info);
            
            if (mixer.getTargetLineInfo().length > 0) {
                AudioDevice device = AudioDevice.fromMixerInfo(info, AudioDevice.Type.INPUT);
                device.setSelected(device.getId().equals(selectedInput));
                devices.add(device);
            }
            
            if (mixer.getSourceLineInfo().length > 0) {
                AudioDevice device = AudioDevice.fromMixerInfo(info, AudioDevice.Type.OUTPUT);
                device.setSelected(device.getId().equals(selectedOutput));
                devices.add(device);
            }
        }
        return devices;
    }

    /**
     * Selects a specific device for recording for the current session.
     * @param deviceId The unique ID of the device to select.
     * @return a status message.
     */
    @AiTool("Selects a specific device for recording for the current session.")
    public String selectRecordingDevice(@AiToolParam("The unique ID of the input device to select.") String deviceId) {
        getAgi().getConfig().setSelectedInputDeviceId(deviceId);
        return "Input device selected: " + deviceId;
    }

    /**
     * Selects a specific device for playback for the current session.
     * @param deviceId The unique ID of the device to select.
     * @return a status message.
     */
    @AiTool("Selects a specific device for playback for the current session.")
    public String selectOutputDevice(@AiToolParam("The unique ID of the output device to select.") String deviceId) {
        getAgi().getConfig().setSelectedOutputDeviceId(deviceId);
        return "Output device selected: " + deviceId;
    }

    /**
     * Records audio from the selected input device for a specified duration.
     * @param durationSeconds Duration in seconds.
     * @param deviceId Optional specific device ID to use.
     * @return a status message.
     * @throws Exception if recording fails.
     */
    @AiTool("Records audio from the selected input device for a specified duration.")
    public String record(@AiToolParam("Duration in seconds.") int durationSeconds,
                         @AiToolParam(value = "Optional specific device ID to use.", required = false) String deviceId) throws Exception {
        
        String actualDeviceId = deviceId != null ? deviceId : getAgi().getConfig().getSelectedInputDeviceId();
        Mixer.Info mixerInfo = AudioDevice.toMixerInfo(actualDeviceId);
        
        TargetDataLine line;
        if (mixerInfo != null) {
            Mixer mixer = AudioSystem.getMixer(mixerInfo);
            line = (TargetDataLine) mixer.getLine(new DataLine.Info(TargetDataLine.class, FORMAT));
        } else {
            line = AudioSystem.getTargetDataLine(FORMAT);
        }

        line.open(FORMAT);
        line.start();

        ByteArrayOutputStream out = new ByteArrayOutputStream();
        byte[] buffer = new byte[4096];
        long endTime = System.currentTimeMillis() + (durationSeconds * 1000);
        
        log("Recording started on device: " + (mixerInfo != null ? mixerInfo.getName() : "Default"));
        
        while (System.currentTimeMillis() < endTime) {
            int count = line.read(buffer, 0, buffer.length);
            if (count > 0) {
                out.write(buffer, 0, count);
            }
        }

        line.stop();
        line.close();

        byte[] audioData = out.toByteArray();
        File tempFile = File.createTempFile("recording", ".wav");
        
        try (AudioInputStream ais = new AudioInputStream(new ByteArrayInputStream(audioData), FORMAT, audioData.length / FORMAT.getFrameSize())) {
            AudioSystem.write(ais, AudioFileFormat.Type.WAVE, tempFile);
        }

        addAttachment(tempFile);
        return "Recording complete. File size: " + tempFile.length() + " bytes. Attached to message.";
    }

    /**
     * Plays an audio file on the selected output device.
     * @param filePath The absolute path to the audio file.
     * @param deviceId Optional specific device ID to use.
     * @return a status message.
     * @throws Exception if playback fails.
     */
    @AiTool("Plays an audio file on the selected output device.")
    public String play(@AiToolParam("The absolute path to the audio file.") String filePath,
                       @AiToolParam(value = "Optional specific device ID to use.", required = false) String deviceId) throws Exception {
        File file = new File(filePath);
        if (!file.exists()) return "Error: File not found: " + filePath;

        String actualDeviceId = deviceId != null ? deviceId : getAgi().getConfig().getSelectedOutputDeviceId();
        Mixer.Info mixerInfo = AudioDevice.toMixerInfo(actualDeviceId);

        try (AudioInputStream ais = AudioSystem.getAudioInputStream(file)) {
            Clip clip;
            if (mixerInfo != null) {
                Mixer mixer = AudioSystem.getMixer(mixerInfo);
                clip = (Clip) mixer.getLine(new DataLine.Info(Clip.class, ais.getFormat()));
            } else {
                clip = AudioSystem.getClip();
            }
            
            clip.open(ais);
            clip.start();
            log("Playing: " + file.getName() + " on " + (mixerInfo != null ? mixerInfo.getName() : "Default"));
            Thread.sleep(clip.getMicrosecondLength() / 1000);
            return "Playback finished.";
        }
    }
}
