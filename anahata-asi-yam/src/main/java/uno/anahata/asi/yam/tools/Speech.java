/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.yam.tools;

import com.sun.speech.freetts.Voice;
import com.sun.speech.freetts.VoiceManager;
import com.sun.speech.freetts.audio.AudioPlayer;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.tool.AnahataToolkit;
import uno.anahata.asi.agi.tool.AgiToolkit;
import uno.anahata.asi.agi.tool.AgiToolParam;
import uno.anahata.asi.agi.tool.AgiTool;

/**
 * A toolkit for speech synthesis (TTS) and audio alerts.
 * Part of the 'Yam' module for creative and experimental tools.
 * Uses FreeTTS for a pure-Java, OS-independent voice.
 * 
 * @author anahata
 */
@Slf4j
@AgiToolkit("A toolkit for speech synthesis (TTS) and audio alerts.")
public class Speech extends AnahataToolkit {

    /** The default voice to use for synthesis. */
    private static final String DEFAULT_VOICE = "kevin16";

    /**
     * Dynamically configures the {@code freetts.voices} 
     * system property to locate the Kevin voice directory. Uses 
     * {@link com.sun.speech.freetts.audio.JavaStreamingAudioPlayer} for 
     * OS-independent playback.
     * @param text The text to speak.
     * @return A status message confirming the speech execution.
     * @throws Exception If the voice cannot be allocated or the engine fails.
     */
    @AgiTool("Speaks the given text using the internal pure-Java TTS engine.")
    public String speak(@AgiToolParam("The text to speak.") String text) throws Exception {
        log.info("Singularity attempting to speak: {}", text);
        
        System.setProperty("freetts.voices", "com.sun.speech.freetts.en.us.cmu_us_kal.KevinVoiceDirectory");
        VoiceManager voiceManager = VoiceManager.getInstance();
        Voice voice = voiceManager.getVoice(DEFAULT_VOICE);

        if (voice == null) {
            throw new IllegalStateException("Speech failed: Voice '" + DEFAULT_VOICE + "' not found. Check your FreeTTS configuration.");
        }

        // FreeTTS JavaStreamingAudioPlayer manages its own line logic. 
        // We use the default player for stability as configuring the line is not supported.
        AudioPlayer audioPlayer = new com.sun.speech.freetts.audio.JavaStreamingAudioPlayer();
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
     * Plays a simple system beep as a low-cost alert.
     * 
     * @return A status message.
     */
    //@AgiTool("Plays a simple system beep as a low-cost alert.")
    public String beep() {
        java.awt.Toolkit.getDefaultToolkit().beep();
        return "Beep executed.";
    }
}
