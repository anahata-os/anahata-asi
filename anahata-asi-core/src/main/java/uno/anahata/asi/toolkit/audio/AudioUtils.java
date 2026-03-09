/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.toolkit.audio;

import lombok.extern.slf4j.Slf4j;

/**
 * Utility class for low-level audio signal processing.
 */
@Slf4j
public class AudioUtils {

    /**
     * Calculates the Root Mean Square (RMS) volume level of an audio buffer.
     * 
     * @param audioData The raw PCM data.
     * @param length The length of the data to process.
     * @return A normalized RMS value (0.0 to 1.0).
     */
    public static double calculateRMS(byte[] audioData, int length) {
        long sum = 0;
        for (int i = 0; i < length; i += 2) { 
            int sample = (audioData[i + 1] << 8) | (audioData[i] & 0xFF);
            if (sample >= 32768) {
                sample -= 65536; 
            }
            sum += (long) sample * sample;
        }
        double rms = Math.sqrt(sum / (length / 2.0));
        return Math.min(1.0, rms / 32768.0);
    }
}
