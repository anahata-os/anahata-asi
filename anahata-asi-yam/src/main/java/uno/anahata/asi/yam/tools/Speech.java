/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.yam.tools;

import com.sun.speech.freetts.Voice;
import com.sun.speech.freetts.VoiceManager;
import com.sun.speech.freetts.audio.AudioPlayer;
import javax.sound.sampled.AudioFormat;
import javax.sound.sampled.AudioSystem;
import javax.sound.sampled.DataLine;
import javax.sound.sampled.Mixer;
import javax.sound.sampled.SourceDataLine;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.yam.tools.audio.AudioDevice;
import uno.anahata.asi.tool.AiTool;
import uno.anahata.asi.tool.AiToolParam;
import uno.anahata.asi.tool.AiToolkit;
import uno.anahata.asi.tool.AnahataToolkit;

/**
 * A toolkit for speech synthesis (TTS) and audio alerts.
 * Part of the 'Yam' module for creative and experimental tools.
 * Uses FreeTTS for a pure-Java, OS-independent voice.
 * 
 * @author anahata
 */
@Slf4j
@AiToolkit("A toolkit for speech synthesis (TTS) and audio alerts.")
public class Speech extends AnahataToolkit {

    /** The default voice to use for synthesis. */
    private static final String DEFAULT_VOICE = "kevin16";

    /**
     * Speaks the given text using the internal FreeTTS engine.
     * This is a pure-Java implementation that works across all platforms.
     * 
     * @param text The text to speak.
     * @param deviceId Optional specific device ID to use for output.
     * @return A status message.
     * @throws Exception If the speech engine fails or the voice is not found.
     */
    @AiTool("Speaks the given text using the internal pure-Java TTS engine.")
    public String speak(@AiToolParam("The text to speak.") String text,
                        @AiToolParam(value = "Optional specific device ID to use for output.", required = false) String deviceId) throws Exception {
        log.info("Singularity attempting to speak: {}", text);
        
        System.setProperty("freetts.voices", "com.sun.speech.freetts.en.us.cmu_us_kal.KevinVoiceDirectory");
        VoiceManager voiceManager = VoiceManager.getInstance();
        Voice voice = voiceManager.getVoice(DEFAULT_VOICE);

        if (voice == null) {
            throw new IllegalStateException("Speech failed: Voice '" + DEFAULT_VOICE + "' not found. Check your FreeTTS configuration.");
        }

        String actualDeviceId = deviceId != null ? deviceId : getAgi().getConfig().getSelectedOutputDeviceId();
        Mixer.Info mixerInfo = AudioDevice.toMixerInfo(actualDeviceId);
        
        AudioPlayer audioPlayer = createAudioPlayer(mixerInfo);
        voice.setAudioPlayer(audioPlayer);

        voice.allocate();
        try {
            voice.speak(text);
            return "The Singularity has spoken: " + text;
        } finally {
            voice.deallocate();
            audioPlayer.close();
        }
    }

    /**
     * Creates a custom FreeTTS AudioPlayer that routes to a specific mixer.
     * 
     * @param mixerInfo The mixer to route to.
     * @return The targeted AudioPlayer.
     */
    private AudioPlayer createAudioPlayer(Mixer.Info mixerInfo) {
        // TODO: Use CodeModel to verify JavaStreamingAudioPlayer methods and implement targeted routing
        return new com.sun.speech.freetts.audio.JavaStreamingAudioPlayer();
        /*
        return new com.sun.speech.freetts.audio.JavaStreamingAudioPlayer() {
            @Override
            protected SourceDataLine getSourceDataLine(AudioFormat format) {
                try {
                    if (mixerInfo != null) {
                        Mixer mixer = AudioSystem.getMixer(mixerInfo);
                        return (SourceDataLine) mixer.getLine(new DataLine.Info(SourceDataLine.class, format));
                    }
                } catch (Exception e) {
                    log.warn("Failed to get line from targeted mixer, falling back to default: {}", e.getMessage());
                }
                return super.getSourceDataLine(format);
            }
        };
        */
    }

    /**
     * Plays a simple system beep as a low-cost alert.
     * 
     * @return A status message.
     */
    //@AiTool("Plays a simple system beep as a low-cost alert.")
    public String beep() {
        java.awt.Toolkit.getDefaultToolkit().beep();
        return "Beep executed.";
    }
}
