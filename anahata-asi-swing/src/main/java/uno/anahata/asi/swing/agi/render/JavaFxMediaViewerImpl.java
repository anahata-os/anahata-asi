/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.agi.render;

import java.awt.BorderLayout;
import java.awt.Color;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.net.URI;
import java.util.Locale;
import javax.swing.JComponent;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;
import javafx.application.Platform;
import javafx.embed.swing.JFXPanel;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.Slider;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Priority;
import javafx.scene.layout.Region;
import javafx.scene.layout.StackPane;
import javafx.scene.layout.VBox;
import javafx.scene.media.Media;
import javafx.scene.media.MediaPlayer;
import javafx.scene.media.MediaView;
import javafx.util.Duration;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.internal.TikaUtils;

/**
 * Native JavaFX hardware-accelerated media viewer implementing {@link MediaViewerComponent}.
 * <p>
 * Loaded dynamically via {@code JavaFxBridgeClassLoader} to prevent compile-time linkage
 * failures in modular IDE environments. Provides full audio and video playback using
 * {@link MediaPlayer} and {@link MediaView} embedded within a Swing {@link JFXPanel}.
 * </p>
 * <p>
 * <b>Features:</b>
 * </p>
 * <ul>
 *   <li>Hardware-accelerated video rendering (MP4, WebM, H.264) with aspect ratio preservation.</li>
 *   <li>Audio stream playback (MP3, WAV, AAC).</li>
 *   <li>Interactive playback controls: Play/Pause, Seek scrubber slider, elapsed/total time, volume slider, mute toggle.</li>
 *   <li>Integrated {@link MediaToolbar} providing Copy, Save As, and external player dispatch.</li>
 * </ul>
 * 
 * @author anahata
 */
@Slf4j
public class JavaFxMediaViewerImpl extends JPanel implements MediaViewerComponent {

    /** The action and metadata toolbar. */
    @Getter
    private final MediaToolbar toolbar = new MediaToolbar();

    /** The Swing JFXPanel hosting the JavaFX scene graph. */
    private final JFXPanel jfxPanel = new JFXPanel();

    /** The active JavaFX MediaPlayer instance. */
    private MediaPlayer mediaPlayer;

    /** The active JavaFX MediaView component. */
    private MediaView mediaView;

    /** Play/Pause toggle button in the JavaFX control bar. */
    private Button playPauseBtn;

    /** Time slider for scrubbing playback position. */
    private Slider timeSlider;

    /** Label displaying elapsed and total time (e.g. {@code "01:24 / 04:50"}). */
    private Label timeLabel;

    /** Volume slider in the JavaFX control bar. */
    private Slider volumeSlider;

    /** Mute toggle button. */
    private Button muteBtn;

    /** Flag indicating user is currently dragging the seek slider. */
    private boolean userIsSeeking = false;

    /** Cached total duration of the media stream. */
    private Duration totalDuration = Duration.ZERO;

    /** Temporary file holding media bytes if loaded from memory. */
    private File tempMediaFile;

    /**
     * Constructs a new JavaFxMediaViewerImpl.
     */
    public JavaFxMediaViewerImpl() {
        super(new BorderLayout());
        setOpaque(true);
        setBackground(new Color(20, 24, 32));

        add(jfxPanel, BorderLayout.CENTER);
        add(toolbar, BorderLayout.SOUTH);
    }

    /**
     * {@inheritDoc}
     * <p>Returns this JPanel as the primary visual component.</p>
     */
    @Override
    public JComponent getComponent() {
        return this;
    }

    /**
     * {@inheritDoc}
     * <p>Initializes the JavaFX media stream from a URI or memory buffer and constructs the player scene.</p>
     */
    @Override
    public void load(byte[] data, String mimeType, String displayName, URI sourceUri) {
        toolbar.setData(data);
        toolbar.setMimeType(mimeType);
        toolbar.setDisplayName(displayName);
        toolbar.setSourceUri(sourceUri);
        toolbar.updateMetadata(null);

        URI effectiveUri = resolveMediaUri(data, mimeType, displayName, sourceUri);
        if (effectiveUri == null) {
            log.warn("JavaFxMediaViewer: Unable to resolve a playable media URI.");
            return;
        }

        Platform.runLater(() -> {
            try {
                initFxPlayer(effectiveUri, mimeType);
            } catch (Throwable t) {
                log.error("Failed to initialize JavaFX media player for URI: {}", effectiveUri, t);
            }
        });
    }

    /**
     * Resolves the media URI to play, writing memory bytes to a temporary file if needed.
     *
     * @param data The raw binary data.
     * @param mimeType The MIME type.
     * @param displayName Display or file name.
     * @param sourceUri Optional original source URI.
     * @return A playable URI, or {@code null} on failure.
     */
    private URI resolveMediaUri(byte[] data, String mimeType, String displayName, URI sourceUri) {
        if (sourceUri != null && "file".equalsIgnoreCase(sourceUri.getScheme())) {
            File f = new File(sourceUri);
            if (f.exists()) {
                return f.toURI();
            }
        }

        if (data != null && data.length > 0) {
            try {
                String ext = TikaUtils.getExtension(mimeType);
                String prefix = displayName != null ? displayName.replaceAll("[^a-zA-Z0-9.-]", "_") + "_" : "media_";
                if (prefix.length() < 3) {
                    prefix = "media_";
                }
                this.tempMediaFile = File.createTempFile(prefix, ext);
                this.tempMediaFile.deleteOnExit();
                try (FileOutputStream fos = new FileOutputStream(tempMediaFile)) {
                    fos.write(data);
                }
                return tempMediaFile.toURI();
            } catch (IOException ex) {
                log.error("JavaFxMediaViewer: Failed to write temp file for media playback", ex);
            }
        }
        return sourceUri;
    }

    /**
     * Constructs and binds the JavaFX player, controls, and scene graph on the JavaFX Application Thread.
     *
     * @param mediaUri The URI of the media to play.
     * @param mimeType The detected MIME type.
     */
    private void initFxPlayer(URI mediaUri, String mimeType) {
        disposeFxPlayer();

        Media media = new Media(mediaUri.toString());
        this.mediaPlayer = new MediaPlayer(media);
        this.mediaView = new MediaView(mediaPlayer);

        mediaView.setPreserveRatio(true);

        // Dark aesthetic controls layout
        javafx.scene.layout.BorderPane rootLayout = new javafx.scene.layout.BorderPane();
        rootLayout.setStyle("-fx-background-color: #0b0f19;");

        // Media display container with responsive sizing
        StackPane mediaContainer = new StackPane(mediaView);
        mediaContainer.setStyle("-fx-background-color: #000000;");
        mediaView.fitWidthProperty().bind(mediaContainer.widthProperty());
        mediaView.fitHeightProperty().bind(mediaContainer.heightProperty());
        rootLayout.setCenter(mediaContainer);

        // Control bar at bottom
        HBox controlBar = buildControlBar();
        rootLayout.setBottom(controlBar);

        // Bind media player events
        bindPlayerEvents();

        Scene scene = new Scene(rootLayout);
        jfxPanel.setScene(scene);
    }

    /**
     * Builds the interactive JavaFX media control bar.
     *
     * @return The configured HBox control bar.
     */
    private HBox buildControlBar() {
        HBox bar = new HBox(10);
        bar.setAlignment(Pos.CENTER_LEFT);
        bar.setPadding(new Insets(8, 14, 8, 14));
        bar.setStyle("-fx-background-color: rgba(15, 23, 42, 0.95); -fx-border-color: rgba(255, 255, 255, 0.1); -fx-border-width: 1 0 0 0;");

        playPauseBtn = new Button("▶");
        playPauseBtn.setStyle("-fx-background-color: #2563eb; -fx-text-fill: white; -fx-font-weight: bold; -fx-font-size: 13px; -fx-background-radius: 6; -fx-cursor: hand; -fx-padding: 4 10 4 10;");
        playPauseBtn.setOnAction(e -> togglePlayPause());

        timeSlider = new Slider(0, 100, 0);
        HBox.setHgrow(timeSlider, Priority.ALWAYS);
        timeSlider.setStyle("-fx-control-inner-background: #334155; -fx-accent: #38bdf8;");

        timeSlider.setOnMousePressed(e -> userIsSeeking = true);
        timeSlider.setOnMouseReleased(e -> {
            userIsSeeking = false;
            if (mediaPlayer != null && totalDuration.greaterThan(Duration.ZERO)) {
                double targetSeconds = (timeSlider.getValue() / 100.0) * totalDuration.toSeconds();
                mediaPlayer.seek(Duration.seconds(targetSeconds));
            }
        });

        timeLabel = new Label("00:00 / 00:00");
        timeLabel.setStyle("-fx-text-fill: #94a3b8; -fx-font-family: 'Consolas', monospace; -fx-font-size: 11px;");

        muteBtn = new Button("🔊");
        muteBtn.setStyle("-fx-background-color: transparent; -fx-text-fill: #94a3b8; -fx-font-size: 13px; -fx-cursor: hand;");
        muteBtn.setOnAction(e -> toggleMute());

        volumeSlider = new Slider(0, 1.0, 0.85);
        volumeSlider.setPrefWidth(85);
        volumeSlider.setStyle("-fx-control-inner-background: #334155; -fx-accent: #38bdf8;");
        volumeSlider.valueProperty().addListener((obs, oldVal, newVal) -> {
            if (mediaPlayer != null) {
                mediaPlayer.setVolume(newVal.doubleValue());
                muteBtn.setText(newVal.doubleValue() == 0 ? "🔇" : "🔊");
            }
        });

        bar.getChildren().addAll(playPauseBtn, timeSlider, timeLabel, muteBtn, volumeSlider);
        return bar;
    }

    /**
     * Binds listeners to the JavaFX MediaPlayer for status and time updates.
     */
    private void bindPlayerEvents() {
        if (mediaPlayer == null) return;

        mediaPlayer.setOnReady(() -> {
            totalDuration = mediaPlayer.getMedia().getDuration();
            updateTimeDisplay(Duration.ZERO, totalDuration);

            // Update toolbar metadata with video dimensions or duration
            int w = mediaPlayer.getMedia().getWidth();
            int h = mediaPlayer.getMedia().getHeight();
            String durStr = formatTime(totalDuration);
            String extra = (w > 0 && h > 0) ? (w + " × " + h + " • " + durStr) : durStr;

            SwingUtilities.invokeLater(() -> toolbar.updateMetadata(extra));
        });

        mediaPlayer.currentTimeProperty().addListener((obs, oldTime, newTime) -> {
            if (!userIsSeeking && totalDuration.greaterThan(Duration.ZERO)) {
                double progress = (newTime.toSeconds() / totalDuration.toSeconds()) * 100.0;
                timeSlider.setValue(progress);
                updateTimeDisplay(newTime, totalDuration);
            }
        });

        mediaPlayer.setOnEndOfMedia(() -> {
            playPauseBtn.setText("▶");
            timeSlider.setValue(100);
            updateTimeDisplay(totalDuration, totalDuration);
        });

        mediaPlayer.setOnPlaying(() -> playPauseBtn.setText("❚❚"));
        mediaPlayer.setOnPaused(() -> playPauseBtn.setText("▶"));
        mediaPlayer.setOnStopped(() -> playPauseBtn.setText("▶"));
    }

    /**
     * Updates the time label display.
     *
     * @param current Current playback time.
     * @param total Total stream duration.
     */
    private void updateTimeDisplay(Duration current, Duration total) {
        String cur = formatTime(current);
        String tot = formatTime(total);
        timeLabel.setText(cur + " / " + tot);
    }

    /**
     * Formats a Duration object into a {@code mm:ss} string.
     *
     * @param d The duration.
     * @return Formatted string.
     */
    private String formatTime(Duration d) {
        if (d == null || d.isUnknown()) return "00:00";
        int totalSecs = (int) Math.floor(d.toSeconds());
        int mins = totalSecs / 60;
        int secs = totalSecs % 60;
        return String.format(Locale.US, "%02d:%02d", mins, secs);
    }

    /**
     * Toggles playback between play and pause.
     */
    private void togglePlayPause() {
        if (mediaPlayer == null) return;
        MediaPlayer.Status status = mediaPlayer.getStatus();
        if (status == MediaPlayer.Status.PLAYING) {
            mediaPlayer.pause();
        } else {
            mediaPlayer.play();
        }
    }

    /**
     * Toggles volume mute status.
     */
    private void toggleMute() {
        if (mediaPlayer == null) return;
        boolean isMute = !mediaPlayer.isMute();
        mediaPlayer.setMute(isMute);
        muteBtn.setText(isMute ? "🔇" : "🔊");
    }

    /**
     * {@inheritDoc}
     * <p>Stops active playback on the JavaFX thread.</p>
     */
    @Override
    public void stop() {
        if (mediaPlayer != null) {
            Platform.runLater(() -> {
                if (mediaPlayer != null) {
                    try {
                        mediaPlayer.stop();
                    } catch (Throwable ignored) {}
                }
            });
        }
    }

    /**
     * Disposes of JavaFX player instances on the FX thread.
     */
    private void disposeFxPlayer() {
        if (mediaPlayer != null) {
            MediaPlayer p = mediaPlayer;
            this.mediaPlayer = null;
            Platform.runLater(p::dispose);
        }
    }

    /**
     * {@inheritDoc}
     * <p>Permanently releases player resources, temporary files, and decoders.</p>
     */
    @Override
    public void dispose() {
        stop();
        disposeFxPlayer();
        if (tempMediaFile != null && tempMediaFile.exists()) {
            tempMediaFile.delete();
            tempMediaFile = null;
        }
    }
}
