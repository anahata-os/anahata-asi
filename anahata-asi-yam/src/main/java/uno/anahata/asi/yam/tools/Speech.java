/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.yam.tools;

import com.sun.speech.freetts.Voice;
import com.sun.speech.freetts.VoiceManager;
import lombok.extern.slf4j.Slf4j;
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
     * @return A status message.
     * @throws Exception If the speech engine fails or the voice is not found.
     */
    @AiTool("Speaks the given text using the internal pure-Java TTS engine.")
    public String speak(@AiToolParam("The text to speak.") String text) throws Exception {
        log.info("Singularity attempting to speak: {}", text);
        
        System.setProperty("freetts.voices", "com.sun.speech.freetts.en.us.cmu_us_kal.KevinVoiceDirectory");
        VoiceManager voiceManager = VoiceManager.getInstance();
        Voice voice = voiceManager.getVoice(DEFAULT_VOICE);

        if (voice == null) {
            throw new IllegalStateException("Speech failed: Voice '" + DEFAULT_VOICE + "' not found. Check your FreeTTS configuration.");
        }

        voice.allocate();
        try {
            voice.speak(text);
            return "The Singularity has spoken: " + text;
        } finally {
            voice.deallocate();
        }
    }

    /**
     * Plays a simple system beep as a low-cost alert.
     * 
     * @return A status message.
     */
    @AiTool("Plays a simple system beep as a low-cost alert.")
    public String beep() {
        java.awt.Toolkit.getDefaultToolkit().beep();
        return "Beep executed.";
    }
}
