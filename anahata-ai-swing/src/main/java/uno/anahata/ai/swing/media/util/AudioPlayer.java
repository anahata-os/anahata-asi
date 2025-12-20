package uno.anahata.ai.swing.media.util;

import java.io.ByteArrayInputStream;
import java.util.concurrent.ExecutorService;
import java.util.function.Consumer;
import javax.sound.sampled.AudioInputStream;
import javax.sound.sampled.AudioSystem;
import javax.sound.sampled.Clip;
import javax.sound.sampled.LineEvent;
import javax.sound.sampled.LineListener;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.ai.AiExecutors;

/**
 * A simple UI utility to play short, non-blocking notification sounds from
 * resources. All playback is handled on a dedicated 'audio-player' thread pool.
 */
@Slf4j
public final class AudioPlayer {

    private static final ExecutorService audioExecutor = AiExecutors.newCachedThreadPoolExecutor("audio-player");

    private AudioPlayer() {
    }

    /**
     * Plays a short, non-blocking notification sound from the application's
     * resources.
     * <p>
     * The sound file is expected to be located in the {@code /sounds/}
     * directory within the classpath. Playback is handled on a dedicated
     * background thread and will be skipped if the microphone is currently
     * active.
     *
     * @param resourceName The simple name of the sound file to play (e.g.,
     * "idle.wav").
     */
    public static void playSound(final String resourceName) {
        audioExecutor.submit(() -> {
            try (AudioInputStream inputStream = AudioSystem.getAudioInputStream(
                    AudioPlayer.class.getResourceAsStream("/sounds/" + resourceName))) {
                if (inputStream == null) {
                    log.warn("Sound resource not found: {}", resourceName);
                    return;
                }
                Clip clip = AudioSystem.getClip();
                clip.addLineListener(event -> {
                    if (event.getType() == LineEvent.Type.STOP) {
                        clip.close();
                    }
                });
                clip.open(inputStream);
                clip.start();
            } catch (Exception e) {
                log.warn("Could not play sound resource: {}", resourceName, e);
            }
        });
    }

    /**
     * Plays audio from a byte array in a non-blocking manner.
     * The clip will be closed automatically when playback stops.
     * @param data The audio data as a byte array.
     */
    public static void play(byte[] data) {
        audioExecutor.submit(() -> {
            try {
                AudioInputStream ais = AudioSystem.getAudioInputStream(new ByteArrayInputStream(data));
                Clip clip = AudioSystem.getClip();
                clip.addLineListener(event -> {
                    if (event.getType() == LineEvent.Type.STOP) {
                        clip.close();
                    }
                });
                clip.open(ais);
                clip.start();
            } catch (Exception e) {
                log.error("Error playing audio", e);
            }
        });
    }

    /**
     * Prepares and starts a toggleable audio playback from a byte array.
     * The returned Clip can be controlled (started/stopped) by the caller.
     * The onPlaybackStatusChange consumer will be called with true when playback starts
     * and false when it stops (either by explicit stop or natural end).
     * @param data The audio data as a byte array.
     * @param onPlaybackStatusChange A consumer to be notified of playback status changes (true for playing, false for stopped).
     * @return The Clip object for controlling playback.
     * @throws Exception if an error occurs during audio setup.
     */
    public static Clip playToggleable(byte[] data, Consumer<Boolean> onPlaybackStatusChange) throws Exception {
        AudioInputStream ais = AudioSystem.getAudioInputStream(new ByteArrayInputStream(data));
        Clip clip = AudioSystem.getClip();
        clip.open(ais);

        clip.addLineListener(event -> {
            if (event.getType() == LineEvent.Type.START) {
                onPlaybackStatusChange.accept(true);
            } else if (event.getType() == LineEvent.Type.STOP) {
                onPlaybackStatusChange.accept(false);
                clip.close();
            }
        });
        clip.start(); // Start playback immediately
        return clip;
    }

    /**
     * Stops and closes the given audio clip.
     * @param clip The Clip to stop and close.
     */
    public static void stop(Clip clip) {
        if (clip != null && clip.isRunning()) {
            clip.stop();
            clip.close();
        }
    }
}
