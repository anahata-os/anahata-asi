/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.toolkit.audio;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import javax.sound.sampled.*;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Represents a stable, model-agnostic descriptor for an audio device.
 * <p>
 * This class is the authoritative way to identify and interact with hardware 
 * mixers. It provides static discovery and instance-level hardware binding 
 * (opening lines) to ensure consistency across the model and UI.
 * </p>
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
public class AudioDevice {

    /** Default format for recording (16kHz, 16-bit, Mono) - Optimized for speech. */
    public static final AudioFormat DEFAULT_FORMAT = new AudioFormat(16000, 16, 1, true, true);
    
    /** Default format for playback (44.1kHz, 16-bit, Stereo) - CD Quality. */
    public static final AudioFormat PLAYBACK_FORMAT = new AudioFormat(44100, 16, 2, true, false);

    /** The unique identifier for the device, derived from hardware metadata. */
    private String id;
    /** The human-readable name of the device. */
    private String name;
    /** The vendor of the device. */
    private String vendor;
    /** A detailed description of the device. */
    private String description;
    /** The type of device (INPUT or OUTPUT). */
    private Type type;
    /** Whether this device is the system's default line. */
    private boolean defaultLine;

    /**
     * Enum representing the direction of the audio data.
     */
    public enum Type {
        /** An audio input device (e.g., microphone). */
        INPUT,
        /** An audio output device (e.g., speakers, headphones). */
        OUTPUT
    }

    /**
     * Factory method to create an AudioDevice from a Mixer.Info and a type.
     * 
     * @param info The mixer info.
     * @param type The device type.
     * @return A new AudioDevice instance.
     */
    public static AudioDevice fromMixerInfo(Mixer.Info info, Type type) {
        AudioDevice device = new AudioDevice();
        device.setId(generateStableId(info));
        device.setName(info.getName());
        device.setVendor(info.getVendor());
        device.setDescription(info.getDescription());
        device.setType(type);
        return device;
    }

    /**
     * Generates a stable, hash-based ID for a mixer.
     * @param info The mixer info.
     * @return A stable ID string.
     */
    public static String generateStableId(Mixer.Info info) {
        return "audio:" + Math.abs(Objects.hash(info.getName(), info.getVendor(), info.getDescription()));
    }

    /**
     * Lists all available audio devices that support data lines (Source/Target).
     * 
     * @return A list of {@link AudioDevice} descriptors.
     */
    public static List<AudioDevice> listAvailableDevices() {
        List<AudioDevice> devices = new ArrayList<>();
        Mixer.Info[] mixerInfos = AudioSystem.getMixerInfo();
        
        Mixer defaultMixer = AudioSystem.getMixer(null);
        Mixer.Info defaultMixerInfo = (defaultMixer != null) ? defaultMixer.getMixerInfo() : null;

        for (Mixer.Info info : mixerInfos) {
            Mixer mixer = AudioSystem.getMixer(info);
            boolean isDefault = info.equals(defaultMixerInfo);
            
            if (mixer.isLineSupported(new DataLine.Info(TargetDataLine.class, DEFAULT_FORMAT))) {
                AudioDevice d = fromMixerInfo(info, Type.INPUT);
                d.setDefaultLine(isDefault);
                devices.add(d);
            }
            
            if (mixer.isLineSupported(new DataLine.Info(SourceDataLine.class, PLAYBACK_FORMAT))) {
                AudioDevice d = fromMixerInfo(info, Type.OUTPUT);
                d.setDefaultLine(isDefault);
                devices.add(d);
            }
        }
        return devices;
    }

    /**
     * Lists available audio devices of a specific type.
     * 
     * @param type The desired device type.
     * @return A filtered list of {@link AudioDevice} descriptors.
     */
    public static List<AudioDevice> listAvailableDevices(Type type) {
        return listAvailableDevices().stream()
                .filter(d -> d.getType() == type)
                .toList();
    }

    /**
     * Finds a specific audio device by its unique ID and type.
     * 
     * @param type The device type.
     * @param id The device identifier.
     * @return The AudioDevice, or null if not found.
     */
    public static AudioDevice findDevice(Type type, String id) {
        return listAvailableDevices().stream()
                .filter(d -> d.getType() == type && d.getId().equals(id))
                .findFirst()
                .orElse(null);
    }

    /**
     * Resolves an output line (SourceDataLine) for this device and a format.
     * 
     * @param format The desired audio format.
     * @return A SourceDataLine instance.
     * @throws LineUnavailableException if the line is already in use.
     */
    public SourceDataLine getOutputLine(AudioFormat format) throws LineUnavailableException {
        Mixer.Info info = getMixerInfo();
        DataLine.Info lineInfo = new DataLine.Info(SourceDataLine.class, format);
        return (info != null) ? (SourceDataLine) AudioSystem.getMixer(info).getLine(lineInfo) 
                              : (SourceDataLine) AudioSystem.getSourceDataLine(format);
    }

    /**
     * Resolves an input line (TargetDataLine) for this device and a format.
     * 
     * @param format The desired audio format.
     * @return A TargetDataLine instance.
     * @throws LineUnavailableException if the line is already in use.
     */
    public TargetDataLine getInputLine(AudioFormat format) throws LineUnavailableException {
        Mixer.Info info = getMixerInfo();
        DataLine.Info lineInfo = new DataLine.Info(TargetDataLine.class, format);
        return (info != null) ? (TargetDataLine) AudioSystem.getMixer(info).getLine(lineInfo) 
                              : (TargetDataLine) AudioSystem.getTargetDataLine(format);
    }

    /**
     * Resolves this descriptor back to a live JRE Mixer.Info.
     * @return The Mixer.Info, or null if the device is no longer present.
     */
    public Mixer.Info getMixerInfo() {
        for (Mixer.Info info : AudioSystem.getMixerInfo()) {
            if (generateStableId(info).equals(id)) {
                return info;
            }
        }
        return null;
    }

    /**
     * Returns a rich Markdown representation of the device for the AI model.
     * @return The markdown string.
     */
    public String toMarkdown() {
        return String.format("**%s** (%s) %s [ID: `%s`]", 
                name, type, (defaultLine ? "<-- **[SYSTEM DEFAULT]**" : ""), id);
    }

    /**
     * Returns the human-friendly name for UI components.
     */
    @Override
    public String toString() {
        if (defaultLine && name != null && !name.toLowerCase().contains("[default]")) {
            return name + " [default]";
        }
        return name;
    }
}
