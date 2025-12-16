/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package uno.anahata.ai.swing.media.util;

import java.util.ArrayList;
import java.util.List;
import javax.sound.sampled.AudioFormat;
import javax.sound.sampled.AudioSystem;
import javax.sound.sampled.DataLine;
import javax.sound.sampled.LineUnavailableException;
import javax.sound.sampled.Mixer;
import javax.sound.sampled.SourceDataLine; // Added import
import javax.sound.sampled.TargetDataLine;
import lombok.extern.slf4j.Slf4j;

/**
 *
 * @author pablo
 */
@Slf4j
public class SoundUtils {
    public static final AudioFormat RECORDING_FORMAT = new AudioFormat(16000, 16, 1, true, true);
    public static final AudioFormat PLAYBACK_FORMAT = new AudioFormat(44100, 16, 2, true, false); // CD quality
    
    /**
     * Calculates the Root Mean Square.
     * 
     * @param audioData
     * @param length
     * @return 
     */
    public static final double calculateRMS(byte[] audioData, int length) {
        long sum = 0;
        for (int i = 0; i < length; i += 2) {  // 16-bit, little-endian
            int sample = (audioData[i + 1] << 8) | (audioData[i] & 0xFF);
            if (sample >= 32768) sample -= 65536;  // Convert to signed
            sum += sample * sample;
        }
        double rms = Math.sqrt(sum / (length / 2.0));
        return rms / 32768.0;  // Normalize to 0.0–1.0
    }
    
    /**
     * Returns a list of all available TargetDataLines that support the default audio format,
     * with the system default line as the first item, wrapped in LineInfo objects for display purposes.
     * This method now explicitly filters for actual microphone input devices.
     * @return A list of available MicrophonePanel.LineInfo objects.
     */
    public static List<LineInfo> getAvailableRecordingLines() {
        List<LineInfo> lines = new ArrayList<>();
        LineInfo defaultLineInfo = null;

        // Try to get the system's default TargetDataLine
        TargetDataLine systemDefaultTargetDataLine = null;
        try {
            DataLine.Info defaultInfo = new DataLine.Info(TargetDataLine.class, RECORDING_FORMAT);
            if (AudioSystem.isLineSupported(defaultInfo)) {
                systemDefaultTargetDataLine = (TargetDataLine) AudioSystem.getLine(defaultInfo);
            }
        } catch (LineUnavailableException e) {
            log.warn("System default microphone line not available or supported: {}", e.getMessage());
        }

        Mixer.Info[] mixerInfos = AudioSystem.getMixerInfo();
        for (Mixer.Info mixerInfo : mixerInfos) {
            Mixer mixer = AudioSystem.getMixer(mixerInfo);
            
            // First, check if the mixer has any TargetDataLines at all
            if (mixer.getTargetLineInfo().length == 0) {
                continue; // Skip this mixer if it has no input lines
            }

            // Then, explicitly check if the mixer supports a TargetDataLine for our format
            DataLine.Info targetDataLineInfo = new DataLine.Info(TargetDataLine.class, RECORDING_FORMAT);
            if (mixer.isLineSupported(targetDataLineInfo)) {
                try {
                    TargetDataLine line = (TargetDataLine) mixer.getLine(targetDataLineInfo);
                    boolean isDefault = (systemDefaultTargetDataLine != null && line.equals(systemDefaultTargetDataLine));
                    LineInfo currentLineInfo = new LineInfo(line, null, mixerInfo, isDefault);

                    if (isDefault) {
                        defaultLineInfo = currentLineInfo;
                    } else {
                        lines.add(currentLineInfo);
                    }
                } catch (LineUnavailableException e) {
                    log.warn("Line for mixer {} is unavailable: {}", mixerInfo.getName(), e.getMessage());
                }
            }
        }
        
        // Add the default line at the beginning if found
        if (defaultLineInfo != null) {
            lines.add(0, defaultLineInfo);
        }
        
        return lines;
    }

    /**
     * Returns a list of all available SourceDataLines that support the default audio format,
     * with the system default line as the first item, wrapped in LineInfo objects for display purposes.
     * This method explicitly filters for actual playback output devices.
     * @return A list of available LineInfo objects for playback.
     */
    public static List<LineInfo> getAvailablePlaybackLines() {
        List<LineInfo> lines = new ArrayList<>();
        LineInfo defaultLineInfo = null;

        // Try to get the system's default SourceDataLine
        SourceDataLine systemDefaultSourceDataLine = null;
        try {
            DataLine.Info defaultInfo = new DataLine.Info(SourceDataLine.class, PLAYBACK_FORMAT);
            if (AudioSystem.isLineSupported(defaultInfo)) {
                systemDefaultSourceDataLine = (SourceDataLine) AudioSystem.getLine(defaultInfo);
            }
        } catch (LineUnavailableException e) {
            log.warn("System default playback line not available or supported: {}", e.getMessage());
        }

        Mixer.Info[] mixerInfos = AudioSystem.getMixerInfo();
        for (Mixer.Info mixerInfo : mixerInfos) {
            Mixer mixer = AudioSystem.getMixer(mixerInfo);
            
            // First, check if the mixer has any SourceDataLines at all
            if (mixer.getSourceLineInfo().length == 0) {
                continue; // Skip this mixer if it has no output lines
            }

            // Then, explicitly check if the mixer supports a SourceDataLine for our format
            DataLine.Info sourceDataLineInfo = new DataLine.Info(SourceDataLine.class, PLAYBACK_FORMAT);
            if (mixer.isLineSupported(sourceDataLineInfo)) {
                try {
                    SourceDataLine line = (SourceDataLine) mixer.getLine(sourceDataLineInfo);
                    boolean isDefault = (systemDefaultSourceDataLine != null && line.equals(systemDefaultSourceDataLine));
                    LineInfo currentLineInfo = new LineInfo(null, line, mixerInfo, isDefault);

                    if (isDefault) {
                        defaultLineInfo = currentLineInfo;
                    } else {
                        lines.add(currentLineInfo);
                    }
                } catch (LineUnavailableException e) {
                    log.warn("Line for mixer {} is unavailable: {}", mixerInfo.getName(), e.getMessage());
                }
            }
        }
        
        // Add the default line at the beginning if found
        if (defaultLineInfo != null) {
            lines.add(0, defaultLineInfo);
        }
        
        return lines;
    }
}
