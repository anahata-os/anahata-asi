/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package uno.anahata.ai.swing.media.util;

import javax.sound.sampled.Mixer;
import javax.sound.sampled.SourceDataLine;
import javax.sound.sampled.TargetDataLine;
import lombok.Getter;
import lombok.RequiredArgsConstructor;

/**
 * Represents an available microphone line with its associated mixer information.
 * This class is used to populate the JComboBox for microphone selection.
 */
@RequiredArgsConstructor
@Getter
public class LineInfo {
    private final TargetDataLine targetDataLine;
    private final SourceDataLine sourceDataLine;
    private final Mixer.Info mixerInfo;
    private final boolean systemDefault;

    /**
     * Returns a human-readable description of the mixer, including its name, description, and vendor.
     * @return The detailed description of the mixer.
     */
    public String getDescription() {
        return mixerInfo.getDescription() + " (Vendor: " + mixerInfo.getVendor() + ")";
    }

    @Override
    public String toString() {
        return mixerInfo.getName();
    }
}
