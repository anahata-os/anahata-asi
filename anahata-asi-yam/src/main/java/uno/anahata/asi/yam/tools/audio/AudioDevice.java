/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.yam.tools.audio;

import javax.sound.sampled.AudioSystem;
import javax.sound.sampled.Mixer;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Represents an audio device (input or output) in a model-agnostic way.
 * This class acts as a keychain for identifying and selecting hardware mixers.
 * 
 * @author anahata
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
public class AudioDevice {

    /** The unique identifier for the device. */
    private String id;
    /** The human-readable name of the device. */
    private String name;
    /** The vendor of the device. */
    private String vendor;
    /** A detailed description of the device. */
    private String description;
    /** The type of device (INPUT or OUTPUT). */
    private Type type;
    /** Whether this device is currently selected in the chat session. */
    private boolean isSelected;

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
        device.setId(info.getName());
        device.setName(info.getName());
        device.setVendor(info.getVendor());
        device.setDescription(info.getDescription());
        device.setType(type);
        return device;
    }

    /**
     * Resolves a device ID back to a Mixer.Info.
     * 
     * @param id The device ID (mixer name).
     * @return The Mixer.Info, or null if not found.
     */
    public static Mixer.Info toMixerInfo(String id) {
        if (id == null) {
            return null;
        }
        for (Mixer.Info info : AudioSystem.getMixerInfo()) {
            if (info.getName().equals(id)) {
                return info;
            }
        }
        return null;
    }

    /**
     * {@inheritDoc}
     * Returns a formatted Markdown string representing the device and its status.
     */
    @Override
    public String toString() {
        return String.format("**%s** (%s)\n" +
                             "  - ID: `%s` %s\n" +
                             "  - Vendor: %s\n" +
                             "  - Description: %s",
                name, type, id, isSelected ? "<-- [SELECTED]" : "", vendor, description);
    }
}
