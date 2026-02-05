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
    private static final String SELECTED_INPUT_DEVICE = "audio.input.device.id";
    private static final String SELECTED_OUTPUT_DEVICE = "audio.output.device.id";

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    public static class AudioDevice {
        private String id;
        private String name;
        private String vendor;
        private String description;
        private String type; // INPUT or OUTPUT
        private boolean isSelected;
    }

    @AiTool("Lists all available audio input and output devices.")
    public List<AudioDevice> listDevices() {
        List<AudioDevice> devices = new ArrayList<>();
        Mixer.Info[] mixerInfos = AudioSystem.getMixerInfo();
        
        String selectedInput = (String) getSessionMap().get(SELECTED_INPUT_DEVICE);
        String selectedOutput = (String) getSessionMap().get(SELECTED_OUTPUT_DEVICE);

        for (Mixer.Info info : mixerInfos) {
            Mixer mixer = AudioSystem.getMixer(info);
            
            // Check for input lines
            if (mixer.getTargetLineInfo().length > 0) {
                AudioDevice device = createDevice(info, "INPUT");
                device.setSelected(device.getId().equals(selectedInput));
                devices.add(device);
            }
            
            // Check for output lines
            if (mixer.getSourceLineInfo().length > 0) {
                AudioDevice device = createDevice(info, "OUTPUT");
                device.setSelected(device.getId().equals(selectedOutput));
                devices.add(device);
            }
        }
        return devices;
    }

    private AudioDevice createDevice(Mixer.Info info, String type) {
        AudioDevice device = new AudioDevice();
        device.setId(info.getName() + ":" + type); // Crude unique ID
        device.setName(info.getName());
        device.setVendor(info.getVendor());
        device.setDescription(info.getDescription());
        device.setType(type);
        return device;
    }

    @AiTool("Selects a specific device for recording or playback for the current session.")
    public String selectDevice(@AiToolParam("The unique ID of the device to select.") String deviceId) {
        if (deviceId.endsWith(":INPUT")) {
            getSessionMap().put(SELECTED_INPUT_DEVICE, deviceId);
            return "Input device selected: " + deviceId;
        } else if (deviceId.endsWith(":OUTPUT")) {
            getSessionMap().put(SELECTED_OUTPUT_DEVICE, deviceId);
            return "Output device selected: " + deviceId;
        }
        return "Error: Invalid device ID format. Must end with :INPUT or :OUTPUT";
    }

    @AiTool("Records audio from the selected input device for a specified duration.")
    public String record(@AiToolParam("Duration in seconds.") int durationSeconds) throws Exception {
        String deviceId = (String) getSessionMap().get(SELECTED_INPUT_DEVICE);
        Mixer.Info mixerInfo = findMixerInfo(deviceId);
        
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

    @AiTool("Plays an audio file on the selected output device.")
    public String play(@AiToolParam("The absolute path to the audio file.") String filePath) throws Exception {
        File file = new File(filePath);
        if (!file.exists()) return "Error: File not found: " + filePath;

        String deviceId = (String) getSessionMap().get(SELECTED_OUTPUT_DEVICE);
        Mixer.Info mixerInfo = findMixerInfo(deviceId);

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

    private Mixer.Info findMixerInfo(String deviceId) {
        if (deviceId == null) return null;
        int lastColon = deviceId.lastIndexOf(':');
        if (lastColon == -1) return null;
        String name = deviceId.substring(0, lastColon);
        for (Mixer.Info info : AudioSystem.getMixerInfo()) {
            if (info.getName().equals(name)) return info;
        }
        return null;
    }
}
