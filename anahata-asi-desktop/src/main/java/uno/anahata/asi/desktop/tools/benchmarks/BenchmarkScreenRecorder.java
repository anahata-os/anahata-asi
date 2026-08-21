/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.desktop.tools.benchmarks;

import java.awt.Dimension;
import java.awt.Rectangle;
import java.awt.Robot;
import java.awt.Toolkit;
import java.awt.image.BufferedImage;
import java.io.File;
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
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.AbstractAsiContainer;

/**
 * Cross-platform screen recording manager utilizing native FFmpeg process execution.
 * <p>
 * Supports automated video capture and instantaneous thumbnail frame snapshots across
 * Linux (X11), macOS (AVFoundation), and Windows (gdigrab).
 * </p>
 * <p>
 * Videos are saved temporarily to {@code ~/.anahata/asi/benchmarks/recordings/} to prevent
 * repository bloat. Upon run finalization, thumbnails are copied directly to the website assets tree.
 * </p>
 *
 * @author anahata
 */
@Slf4j
public class BenchmarkScreenRecorder {

    /**
     * The active FFmpeg recording operating system process, or {@code null} if idle.
     */
    private Process ffmpegProcess;

    /**
     * The absolute path to the .mp4 file currently being recorded.
     */
    private Path currentVideoPath;

    /**
     * Start time in epoch milliseconds.
     */
    private long startEpochMillis;

    /**
     * Resolves the temporary directory where benchmark screen recordings are stored.
     *
     * @return The path to {@code ~/.anahata/asi/benchmarks/recordings/}.
     */
    public static Path getRecordingsDirectory() {
        Path dir = AbstractAsiContainer.getWorkDirSubDir("benchmarks").resolve("recordings");
        try {
            Files.createDirectories(dir);
        } catch (IOException e) {
            log.error("Could not create benchmark recordings directory: {}", dir, e);
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
     * Initiates a new screen recording session for a benchmark test.
     *
     * @param testCode The benchmark test identifier code (e.g. {@code "JAVA-ARKANOID-1"}).
     * @param modelId The candidate model identifier string (e.g. {@code "gemini-3.6-flash"}).
     * @return The path to the destination .mp4 file.
     * @throws Exception If launching the FFmpeg process fails.
     */
    public synchronized Path startRecording(String testCode, String modelId) throws Exception {
        if (isRecording()) {
            log.warn("Recording already in progress. Stopping previous recording first.");
            cancelRecording();
        }

        String safeModel = modelId.replaceAll("[^a-zA-Z0-9.-]", "_");
        String timestamp = new SimpleDateFormat("yyyyMMdd_HHmmss").format(new Date());
        String filename = testCode.toLowerCase().replace('_', '-') + "_" + safeModel + "_" + timestamp + ".mp4";

        this.currentVideoPath = getRecordingsDirectory().resolve(filename);
        this.startEpochMillis = System.currentTimeMillis();

        List<String> command = buildFfmpegCommand(currentVideoPath.toString());
        log.info("Starting benchmark screen recording with command: {}", String.join(" ", command));

        ProcessBuilder pb = new ProcessBuilder(command);
        pb.redirectErrorStream(true);

        this.ffmpegProcess = pb.start();

        // Drain process stdout/stderr in a background thread to prevent buffer deadlocks
        Thread drainThread = new Thread(() -> {
            try (var reader = new java.io.BufferedReader(new java.io.InputStreamReader(ffmpegProcess.getInputStream()))) {
                String line;
                while ((line = reader.readLine()) != null) {
                    // Debug trace output
                    log.trace("[FFmpeg] {}", line);
                }
            } catch (IOException ignored) {
            }
        }, "Benchmark-FFmpeg-Drain");
        drainThread.setDaemon(true);
        drainThread.start();

        log.info("FFmpeg screen recording started for {} -> {}", testCode, currentVideoPath);
        return currentVideoPath;
    }

    /**
     * Stops the active recording, captures an instantaneous screen frame thumbnail,
     * and finalizes the MP4 file.
     *
     * @param captureThumbnail Whether to capture a PNG screenshot at the stop moment.
     * @param testCode The test identifier code.
     * @param modelId The candidate model identifier string.
     * @return A {@link RecordedBenchmarkSession} containing paths and recording duration.
     * @throws Exception If process termination or thumbnail capture fails.
     */
    public synchronized RecordedBenchmarkSession stopRecording(boolean captureThumbnail, String testCode, String modelId) throws Exception {
        if (!isRecording()) {
            log.warn("No active recording process to stop.");
            return null;
        }

        Path thumbnailPath = null;
        if (captureThumbnail) {
            thumbnailPath = captureScreenFrame(testCode, modelId);
        }

        long elapsedMillis = System.currentTimeMillis() - startEpochMillis;
        double durationSeconds = Math.round((elapsedMillis / 1000.0) * 100.0) / 100.0;

        log.info("Stopping FFmpeg recording process (elapsed: {}s)...", durationSeconds);
        try {
            // Gracefully send 'q' to FFmpeg standard input
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

        log.info("Benchmark recording finalized: {} (Thumbnail: {})", currentVideoPath, thumbnailPath);
        return RecordedBenchmarkSession.builder()
                .videoPath(currentVideoPath)
                .thumbnailPath(thumbnailPath)
                .durationSeconds(durationSeconds)
                .build();
    }

    /**
     * Cancels the active recording session, terminates FFmpeg, and deletes partial video files.
     */
    public synchronized void cancelRecording() {
        if (ffmpegProcess != null) {
            log.info("Cancelling benchmark screen recording...");
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
     * Captures a high-resolution snapshot of the primary screen at the current instant.
     *
     * @param testCode The test code.
     * @param modelId The candidate model ID.
     * @return The path to the saved PNG thumbnail file.
     */
    private Path captureScreenFrame(String testCode, String modelId) {
        try {
            Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
            Rectangle screenRect = new Rectangle(screenSize);
            Robot robot = new Robot();
            BufferedImage capture = robot.createScreenCapture(screenRect);

            String safeModel = modelId.replaceAll("[^a-zA-Z0-9.-]", "_");
            String filename = testCode.toLowerCase().replace('_', '-') + "_" + safeModel + "_thumb.png";
            Path localThumbPath = getRecordingsDirectory().resolve(filename);

            ImageIO.write(capture, "png", localThumbPath.toFile());
            log.info("Captured benchmark frame thumbnail to {}", localThumbPath);

            // Copy to web assets folder if development environment exists
            Path webAssetDir = Paths.get(System.getProperty("user.home"), "NetBeansProjects", "anahata-asi-parent",
                    "anahata-asi-web", "src", "main", "resources", "web", "assets", "benchmarks", "ANAHATA-AGI-1", testCode);
            if (Files.exists(webAssetDir.getParent())) {
                Files.createDirectories(webAssetDir);
                Path webThumbPath = webAssetDir.resolve(safeModel + ".png");
                Files.copy(localThumbPath, webThumbPath, StandardCopyOption.REPLACE_EXISTING);
                log.info("Copied benchmark thumbnail to website assets: {}", webThumbPath);
            }

            return localThumbPath;
        } catch (Exception e) {
            log.error("Failed to capture benchmark screen frame thumbnail", e);
            return null;
        }
    }

    /**
     * Builds the OS-specific FFmpeg CLI command.
     *
     * @param outputPath The target output .mp4 file path.
     * @return List of command arguments.
     */
    private List<String> buildFfmpegCommand(String outputPath) {
        String osName = System.getProperty("os.name", "").toLowerCase();
        List<String> cmd = new ArrayList<>();
        cmd.add("ffmpeg");
        cmd.add("-y"); // Overwrite output

        if (osName.contains("linux")) {
            Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
            cmd.add("-f");
            cmd.add("x11grab");
            cmd.add("-draw_mouse");
            cmd.add("1");
            cmd.add("-r");
            cmd.add("30");
            cmd.add("-s");
            cmd.add((int) screenSize.getWidth() + "x" + (int) screenSize.getHeight());
            cmd.add("-i");
            String display = System.getenv("DISPLAY");
            cmd.add(display != null && !display.isBlank() ? display : ":0.0");
        } else if (osName.contains("mac")) {
            cmd.add("-f");
            cmd.add("avfoundation");
            cmd.add("-r");
            cmd.add("30");
            cmd.add("-i");
            cmd.add("1:0"); // Screen index 1, default audio
        } else if (osName.contains("win")) {
            cmd.add("-f");
            cmd.add("gdigrab");
            cmd.add("-framerate");
            cmd.add("30");
            cmd.add("-i");
            cmd.add("desktop");
        } else {
            // Generic fallback
            cmd.add("-f");
            cmd.add("x11grab");
            cmd.add("-r");
            cmd.add("30");
            cmd.add("-i");
            cmd.add(":0.0");
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
