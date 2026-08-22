/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.yam.tools.screenrecording;

import java.awt.Dimension;
import java.awt.Rectangle;
import java.awt.Robot;
import java.awt.Toolkit;
import java.awt.image.BufferedImage;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.concurrent.TimeUnit;
import javax.imageio.ImageIO;
import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.AbstractAsiContainer;

/**
 * Universal cross-platform screen recording engine utilizing native FFmpeg process execution.
 * <p>
 * Supports automated video capture and instantaneous thumbnail frame snapshots across
 * Linux (X11), macOS (AVFoundation), and Windows (gdigrab).
 * </p>
 * <p>
 * Supports customizable recording directories, explicit target file paths, and multi-monitor resolution routing.
 * </p>
 *
 * @author anahata
 */
@Slf4j
public class ScreenRecorder {

    /**
     * The active FFmpeg recording operating system process, or {@code null} if idle.
     */
    private Process ffmpegProcess;

    /**
     * The absolute path to the .mp4 file currently being recorded.
     */
    @Getter
    private Path currentVideoPath;

    /**
     * Start time in epoch milliseconds.
     */
    private long startEpochMillis;

    /**
     * Selected screen graphics device index.
     */
    @Getter
    private int selectedDeviceIndex = 0;

    /**
     * Customizable output directory for recorded videos.
     */
    @Getter
    @Setter
    private Path customRecordingsDirectory;

    /**
     * Resolves the directory where screen recordings are stored.
     *
     * @return The path to the recordings directory.
     */
    public Path getEffectiveRecordingsDirectory() {
        if (customRecordingsDirectory != null) {
            try {
                Files.createDirectories(customRecordingsDirectory);
            } catch (IOException e) {
                log.error("Could not create custom recordings directory: {}", customRecordingsDirectory, e);
            }
            return customRecordingsDirectory;
        }
        Path dir = AbstractAsiContainer.getWorkDirSubDir("recordings");
        try {
            Files.createDirectories(dir);
        } catch (IOException e) {
            log.error("Could not create recordings directory: {}", dir, e);
        }
        return dir;
    }

    /**
     * Checks if a screen recording process is currently active.
     *
     * @return {@code true} if FFmpeg is actively recording.
     */
    public synchronized boolean isRecording() {
        return ffmpegProcess != null && ffmpegProcess.isAlive();
    }

    /**
     * Initiates a new screen recording session to an explicit target file path on the specified screen device.
     *
     * @param targetFilePath The exact destination .mp4 file path.
     * @param deviceIndex The 0-based screen graphics device index to record.
     * @return The path to the destination .mp4 file.
     * @throws Exception If launching the FFmpeg process fails.
     */
    public synchronized Path startRecording(Path targetFilePath, int deviceIndex) throws Exception {
        if (isRecording()) {
            log.warn("Recording already in progress. Stopping previous recording first.");
            cancelRecording();
        }

        this.selectedDeviceIndex = deviceIndex;
        this.currentVideoPath = targetFilePath;
        Files.createDirectories(targetFilePath.getParent());
        this.startEpochMillis = System.currentTimeMillis();

        List<String> command = buildFfmpegCommand(currentVideoPath.toString(), deviceIndex);
        log.info("Starting screen recording with command: {}", String.join(" ", command));

        ProcessBuilder pb = new ProcessBuilder(command);
        pb.redirectErrorStream(true);

        this.ffmpegProcess = pb.start();

        // Drain process stdout/stderr in a background thread to prevent buffer deadlocks
        Thread drainThread = new Thread(() -> {
            try (var reader = new java.io.BufferedReader(new java.io.InputStreamReader(ffmpegProcess.getInputStream()))) {
                String line;
                while ((line = reader.readLine()) != null) {
                    log.trace("[FFmpeg] {}", line);
                }
            } catch (IOException ignored) {
            }
        }, "ScreenRecorder-FFmpeg-Drain");
        drainThread.setDaemon(true);
        drainThread.start();

        log.info("FFmpeg screen recording started on Screen {} -> {}", deviceIndex, currentVideoPath);
        return currentVideoPath;
    }

    /**
     * Initiates a new screen recording session using a generated filename with prefix and identifier.
     *
     * @param prefix The filename prefix (e.g., "desktop", "java-jna-1").
     * @param identifier The descriptor identifier (e.g., "gemini-3.6-flash").
     * @param deviceIndex The 0-based screen graphics device index.
     * @return The path to the destination .mp4 file.
     * @throws Exception If launching FFmpeg fails.
     */
    public synchronized Path startRecording(String prefix, String identifier, int deviceIndex) throws Exception {
        String safePrefix = prefix != null ? prefix.replaceAll("[^a-zA-Z0-9.-]", "_") : "recording";
        String safeId = identifier != null ? identifier.replaceAll("[^a-zA-Z0-9.-]", "_") : "session";
        String timestamp = new SimpleDateFormat("yyyyMMdd_HHmmss").format(new Date());
        String filename = safePrefix + "_" + safeId + "_" + timestamp + ".mp4";

        Path target = getEffectiveRecordingsDirectory().resolve(filename);
        return startRecording(target, deviceIndex);
    }

    /**
     * Stops the active recording, captures an instantaneous screen frame thumbnail,
     * and finalizes the MP4 file by cleanly sending the 'q' quit signal to FFmpeg.
     *
     * @param captureThumbnail Whether to capture a PNG screenshot at the stop moment.
     * @param thumbnailDestination The optional destination path for the captured thumbnail PNG.
     * @return A {@link RecordedSession} containing paths and recording duration.
     * @throws Exception If process termination or thumbnail capture fails.
     */
    public synchronized RecordedSession stopRecording(boolean captureThumbnail, Path thumbnailDestination) throws Exception {
        if (!isRecording()) {
            log.warn("No active recording process to stop.");
            return null;
        }

        Path finalThumbPath = null;
        if (captureThumbnail) {
            finalThumbPath = captureScreenFrame(thumbnailDestination, selectedDeviceIndex);
        }

        long elapsedMillis = System.currentTimeMillis() - startEpochMillis;
        double durationSeconds = Math.round((elapsedMillis / 1000.0) * 100.0) / 100.0;

        log.info("Stopping FFmpeg recording process (elapsed: {}s)...", durationSeconds);
        try {
            // Gracefully send 'q' to FFmpeg standard input to flush H.264 moov atom
            OutputStream os = ffmpegProcess.getOutputStream();
            os.write("q\n".getBytes());
            os.flush();
            os.close();

            boolean finished = ffmpegProcess.waitFor(6, TimeUnit.SECONDS);
            if (!finished) {
                log.warn("FFmpeg did not stop within 6 seconds. Forcing destruction...");
                ffmpegProcess.destroy();
                ffmpegProcess.waitFor(2, TimeUnit.SECONDS);
            }
        } catch (Exception e) {
            log.error("Error gracefully stopping FFmpeg process", e);
            if (ffmpegProcess != null) {
                ffmpegProcess.destroyForcibly();
            }
        } finally {
            this.ffmpegProcess = null;
        }

        log.info("Screen recording finalized: {} (Thumbnail: {})", currentVideoPath, finalThumbPath);
        return RecordedSession.builder()
                .videoPath(currentVideoPath)
                .thumbnailPath(finalThumbPath)
                .durationSeconds(durationSeconds)
                .build();
    }

    /**
     * Cancels the active recording session, terminates FFmpeg, and deletes partial video files.
     */
    public synchronized void cancelRecording() {
        if (ffmpegProcess != null) {
            log.info("Cancelling screen recording...");
            try {
                ffmpegProcess.destroyForcibly();
            } catch (Exception ignored) {
            }
            this.ffmpegProcess = null;
        }

        if (currentVideoPath != null && Files.exists(currentVideoPath)) {
            try {
                Files.deleteIfExists(currentVideoPath);
                log.info("Deleted cancelled video file: {}", currentVideoPath);
            } catch (IOException e) {
                log.warn("Could not delete cancelled video file: {}", currentVideoPath, e);
            }
            this.currentVideoPath = null;
        }
    }

    /**
     * Captures a high-resolution snapshot of the recorded screen at the current instant.
     *
     * @param customDest The optional custom destination file path.
     * @param deviceIndex The screen device index.
     * @return The path to the saved PNG thumbnail file.
     */
    public Path captureScreenFrame(Path customDest, int deviceIndex) {
        try {
            java.awt.GraphicsEnvironment ge = java.awt.GraphicsEnvironment.getLocalGraphicsEnvironment();
            java.awt.GraphicsDevice[] devices = ge.getScreenDevices();
            Rectangle bounds;
            if (deviceIndex >= 0 && deviceIndex < devices.length) {
                bounds = devices[deviceIndex].getDefaultConfiguration().getBounds();
            } else {
                Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
                bounds = new Rectangle(screenSize);
            }

            Robot robot = new Robot();
            BufferedImage capture = robot.createScreenCapture(bounds);

            Path thumbPath = customDest != null ? customDest : getEffectiveRecordingsDirectory().resolve(
                    currentVideoPath != null
                            ? currentVideoPath.getFileName().toString().replace(".mp4", "_thumb.png")
                            : "thumb_" + System.currentTimeMillis() + ".png"
            );

            Files.createDirectories(thumbPath.getParent());
            ImageIO.write(capture, "png", thumbPath.toFile());
            log.info("Captured screen frame thumbnail to {}", thumbPath);
            return thumbPath;
        } catch (Exception e) {
            log.error("Failed to capture screen frame thumbnail", e);
            return null;
        }
    }

    /**
     * Builds the OS-specific FFmpeg CLI command for the given screen device.
     *
     * @param outputPath The target output .mp4 file path.
     * @param deviceIndex The target screen graphics device index.
     * @return List of command arguments.
     */
    private List<String> buildFfmpegCommand(String outputPath, int deviceIndex) {
        String osName = System.getProperty("os.name", "").toLowerCase();
        List<String> cmd = new ArrayList<>();
        cmd.add("ffmpeg");
        cmd.add("-y"); // Overwrite output

        java.awt.GraphicsEnvironment ge = java.awt.GraphicsEnvironment.getLocalGraphicsEnvironment();
        java.awt.GraphicsDevice[] devices = ge.getScreenDevices();
        Rectangle bounds;
        if (deviceIndex >= 0 && deviceIndex < devices.length) {
            bounds = devices[deviceIndex].getDefaultConfiguration().getBounds();
        } else {
            Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
            bounds = new Rectangle(0, 0, (int) screenSize.getWidth(), (int) screenSize.getHeight());
        }

        if (osName.contains("linux")) {
            cmd.add("-f");
            cmd.add("x11grab");
            cmd.add("-draw_mouse");
            cmd.add("1");
            cmd.add("-r");
            cmd.add("30");
            cmd.add("-s");
            cmd.add(bounds.width + "x" + bounds.height);
            cmd.add("-i");
            String display = System.getenv("DISPLAY");
            if (display == null || display.isBlank()) {
                display = ":0.0";
            }
            if (!display.contains("+")) {
                display = display + "+" + bounds.x + "," + bounds.y;
            }
            cmd.add(display);
        } else if (osName.contains("mac")) {
            cmd.add("-f");
            cmd.add("avfoundation");
            cmd.add("-r");
            cmd.add("30");
            cmd.add("-i");
            cmd.add((deviceIndex + 1) + ":0");
        } else if (osName.contains("win")) {
            cmd.add("-f");
            cmd.add("gdigrab");
            cmd.add("-framerate");
            cmd.add("30");
            cmd.add("-offset_x");
            cmd.add(String.valueOf(bounds.x));
            cmd.add("-offset_y");
            cmd.add(String.valueOf(bounds.y));
            cmd.add("-video_size");
            cmd.add(bounds.width + "x" + bounds.height);
            cmd.add("-i");
            cmd.add("desktop");
        } else {
            cmd.add("-f");
            cmd.add("x11grab");
            cmd.add("-r");
            cmd.add("30");
            cmd.add("-s");
            cmd.add(bounds.width + "x" + bounds.height);
            cmd.add("-i");
            cmd.add(":0.0+" + bounds.x + "," + bounds.y);
        }

        // Fast video encoding presets for low CPU overhead
        cmd.add("-c:v");
        cmd.add("libx264");
        cmd.add("-preset");
        cmd.add("ultrafast");
        cmd.add("-pix_fmt");
        cmd.add("yuv420p");
        cmd.add(outputPath);

        return cmd;
    }
}
