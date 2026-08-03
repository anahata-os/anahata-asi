/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.desktop;

import javax.imageio.ImageIO;
import javax.sound.sampled.*;
import javax.swing.*;
import java.awt.*;
import java.awt.image.BufferedImage;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.WebSocket;
import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Base64;
import java.util.List;
import java.util.concurrent.CompletionStage;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicLong;

/**
 * A standalone launcher test class for Gemini 3.1 Flash Live (Bidi WebSocket API).
 * <p>
 * This test class demonstrates full-duplex live audio (16kHz microphone recording,
 * 24kHz speaker playback with auto-mute echo cancellation), NetBeans IDE screen video
 * streaming at 1 FPS via {@code realtimeInput.video}, and a real-time Swing visualizer panel.
 * </p>
 * 
 * @author anahata
 */
public class GeminiLiveTest {

    private static class LiveStreamVisualizerPanel extends JPanel {
        private float micLevel = 0.0f;
        private float speakerLevel = 0.0f;
        private int screenFramesSent = 0;
        private String statusText = "Initializing...";
        private boolean isMicMuted = false;

        public void setMicLevel(float level) {
            this.micLevel = Math.min(1.0f, Math.max(0.0f, level));
            repaint();
        }

        public void setSpeakerLevel(float level) {
            this.speakerLevel = Math.min(1.0f, Math.max(0.0f, level));
            repaint();
        }

        public void incrementFrames() {
            this.screenFramesSent++;
            repaint();
        }

        public void setStatusText(String text) {
            this.statusText = text;
            repaint();
        }

        public void setMicMuted(boolean muted) {
            this.isMicMuted = muted;
            repaint();
        }

        @Override
        protected void paintComponent(Graphics g) {
            super.paintComponent(g);
            Graphics2D g2 = (Graphics2D) g.create();
            g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

            int width = getWidth();
            int height = getHeight();

            g2.setColor(new Color(24, 24, 28));
            g2.fillRect(0, 0, width, height);

            g2.setColor(Color.WHITE);
            g2.setFont(new Font("SansSerif", Font.BOLD, 15));
            g2.drawString("Gemini 3.1 Live - Voice + NetBeans IDE Screen Video Stream", 20, 28);

            g2.setFont(new Font("SansSerif", Font.PLAIN, 12));
            g2.setColor(new Color(180, 220, 255));
            g2.drawString("Status: " + statusText, 20, 50);

            // Screen Frames Counter
            g2.setColor(new Color(255, 215, 0));
            g2.drawString("🖼️ NetBeans Window Video Frames Streamed: " + screenFramesSent, 20, 72);

            // Mic Bar
            int barY1 = 90;
            g2.setColor(Color.LIGHT_GRAY);
            String micLabel = isMicMuted ? "🎙️ Mic (AUTO-MUTED during playback):" : "🎙️ Your Mic (Active):";
            g2.drawString(micLabel, 20, barY1 + 15);
            g2.setColor(new Color(40, 40, 50));
            g2.fillRect(270, barY1, width - 290, 20);

            if (isMicMuted) {
                g2.setColor(new Color(220, 80, 80));
            } else {
                g2.setColor(new Color(0, 230, 180));
            }
            int micWidth = (int) ((width - 290) * (isMicMuted ? 0.05f : micLevel));
            g2.fillRect(270, barY1, micWidth, 20);

            // Speaker Bar
            int barY2 = 125;
            g2.setColor(Color.LIGHT_GRAY);
            g2.drawString("🔊 Gemini Audio (Playback):", 20, barY2 + 15);
            g2.setColor(new Color(40, 40, 50));
            g2.fillRect(270, barY2, width - 290, 20);
            g2.setColor(new Color(255, 120, 0));
            int speakerWidth = (int) ((width - 290) * speakerLevel);
            g2.fillRect(270, barY2, speakerWidth, 20);

            g2.dispose();
        }
    }

    public static void main(String[] args) throws Exception {
        System.out.println("=== STARTING STANDALONE GEMINI LIVE TEST ===");

        // Resolve API key from standard Anahata Gemini keys file
        Path keyPath = Paths.get(System.getProperty("user.home"), ".anahata", "asi", "Gemini", "api_keys.txt");
        if (!Files.exists(keyPath)) {
            System.err.println("Gemini API key file not found at: " + keyPath);
            return;
        }

        List<String> keyLines = Files.readAllLines(keyPath);
        String apiKey = keyLines.stream()
                .map(String::trim)
                .filter(l -> !l.isEmpty() && !l.startsWith("#") && !l.startsWith("//"))
                .map(l -> l.contains("//") ? l.substring(0, l.indexOf("//")).trim() : l)
                .findFirst()
                .orElse(null);

        if (apiKey == null) {
            System.err.println("No valid API key found in " + keyPath);
            return;
        }

        String modelName = "models/gemini-3.1-flash-live-preview";

        String setupJson = "{\n"
                + "  \"setup\": {\n"
                + "    \"model\": \"" + modelName + "\",\n"
                + "    \"generationConfig\": {\n"
                + "      \"responseModalities\": [\"AUDIO\"],\n"
                + "      \"speechConfig\": {\n"
                + "        \"voiceConfig\": {\n"
                + "          \"prebuiltVoiceConfig\": {\n"
                + "            \"voiceName\": \"Puck\"\n"
                + "          }\n"
                + "        }\n"
                + "      }\n"
                + "    },\n"
                + "    \"systemInstruction\": {\n"
                + "      \"parts\": [\n"
                + "        {\n"
                + "          \"text\": \"You are Anahata ASI live voice pair programmer. You can see the user's NetBeans IDE screen in real-time via continuous JPEG video frames under realtimeInput.video! Ask the user what they are working on or comment on what you see on their IDE screen!\"\n"
                + "        }\n"
                + "      ]\n"
                + "    }\n"
                + "  }\n"
                + "}";

        AudioFormat speakerFormat = new AudioFormat(24000.0f, 16, 1, true, false);
        DataLine.Info speakerInfo = new DataLine.Info(SourceDataLine.class, speakerFormat);
        SourceDataLine speakerLine = (SourceDataLine) AudioSystem.getLine(speakerInfo);
        speakerLine.open(speakerFormat);
        speakerLine.start();

        AudioFormat micFormat = new AudioFormat(16000.0f, 16, 1, true, false);
        DataLine.Info micInfo = new DataLine.Info(TargetDataLine.class, micFormat);
        TargetDataLine micLine = (TargetDataLine) AudioSystem.getLine(micInfo);
        micLine.open(micFormat);
        micLine.start();

        final LiveStreamVisualizerPanel guiPanel = new LiveStreamVisualizerPanel();
        final JFrame frame = new JFrame("Anahata ASI - NetBeans Video + Voice Live Test");

        SwingUtilities.invokeLater(() -> {
            frame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
            frame.setSize(650, 220);
            frame.setLocationRelativeTo(null);
            frame.setAlwaysOnTop(true);
            frame.add(guiPanel, BorderLayout.CENTER);
            frame.setVisible(true);
        });

        AtomicBoolean sessionActive = new AtomicBoolean(true);
        AtomicLong lastSpeakerPlaybackTime = new AtomicLong(0);
        CountDownLatch sessionLatch = new CountDownLatch(1);

        String wsUrl = "wss://generativelanguage.googleapis.com/ws/google.ai.generativelanguage.v1alpha.GenerativeService.BidiGenerateContent?key=" + apiKey;

        HttpClient client = HttpClient.newHttpClient();
        WebSocket.Listener listener = new WebSocket.Listener() {
            private StringBuilder binBuffer = new StringBuilder();

            @Override
            public void onOpen(WebSocket webSocket) {
                System.out.println("[WS] Connected! Sending setup...");
                guiPanel.setStatusText("Connected. Initializing...");
                webSocket.request(100);
                webSocket.sendText(setupJson, true);
            }

            @Override
            public CompletionStage<?> onBinary(WebSocket webSocket, ByteBuffer data, boolean last) {
                byte[] bytes = new byte[data.remaining()];
                data.get(bytes);
                binBuffer.append(new String(bytes, StandardCharsets.UTF_8));

                if (last) {
                    String json = binBuffer.toString();
                    binBuffer.setLength(0);

                    if (json.contains("setupComplete")) {
                        System.out.println(">>> [LIVE READY] Voice & NetBeans Video screen capture live! Speak now! <<<");
                        guiPanel.setStatusText("LIVE READY! Streaming NetBeans video & Mic...");
                    }

                    if (json.contains("\"data\":")) {
                        int dataIdx = json.indexOf("\"data\":");
                        if (dataIdx != -1) {
                            int startQuote = json.indexOf("\"", dataIdx + 7);
                            int endQuote = json.indexOf("\"", startQuote + 1);
                            if (startQuote != -1 && endQuote != -1) {
                                String base64Pcm = json.substring(startQuote + 1, endQuote);
                                try {
                                    byte[] audioData = Base64.getDecoder().decode(base64Pcm);
                                    lastSpeakerPlaybackTime.set(System.currentTimeMillis());
                                    speakerLine.write(audioData, 0, audioData.length);

                                    long sum = 0;
                                    for (int i = 0; i < audioData.length - 1; i += 2) {
                                        short sample = (short) ((audioData[i + 1] << 8) | (audioData[i] & 0xFF));
                                        sum += sample * sample;
                                    }
                                    double rms = Math.sqrt((double) sum / (audioData.length / 2));
                                    float level = (float) (rms / 12000.0);
                                    guiPanel.setSpeakerLevel(level);
                                    guiPanel.setStatusText("Gemini is speaking...");
                                } catch (Exception e) {
                                    // ignore
                                }
                            }
                        }
                    }
                }

                webSocket.request(1);
                return null;
            }

            @Override
            public void onError(WebSocket webSocket, Throwable error) {
                System.err.println("[WS ERROR] " + error.getMessage());
                guiPanel.setStatusText("Error: " + error.getMessage());
                sessionLatch.countDown();
            }

            @Override
            public CompletionStage<?> onClose(WebSocket webSocket, int statusCode, String reason) {
                System.out.println("[WS CLOSED] Status: " + statusCode + ", Reason: " + reason);
                guiPanel.setStatusText("Closed: " + reason);
                sessionLatch.countDown();
                return null;
            }
        };

        WebSocket ws = client.newWebSocketBuilder()
                .buildAsync(URI.create(wsUrl), listener)
                .get(10, TimeUnit.SECONDS);

        // Mic streaming thread
        Thread.ofVirtual().start(() -> {
            byte[] buffer = new byte[2048];
            while (sessionActive.get()) {
                int bytesRead = micLine.read(buffer, 0, buffer.length);
                if (bytesRead > 0) {
                    long timeSinceSpeakerOutput = System.currentTimeMillis() - lastSpeakerPlaybackTime.get();
                    boolean isSpeakerActive = timeSinceSpeakerOutput < 600;
                    guiPanel.setMicMuted(isSpeakerActive);

                    if (isSpeakerActive) {
                        guiPanel.setMicLevel(0.0f);
                        continue;
                    }

                    guiPanel.setSpeakerLevel(0.0f);
                    long sum = 0;
                    for (int i = 0; i < bytesRead - 1; i += 2) {
                        short sample = (short) ((buffer[i + 1] << 8) | (buffer[i] & 0xFF));
                        sum += sample * sample;
                    }
                    double rms = Math.sqrt((double) sum / (bytesRead / 2));
                    float level = (float) (rms / 8000.0);
                    guiPanel.setMicLevel(level);

                    String base64Chunk = Base64.getEncoder().encodeToString(buffer);
                    String realtimeChunk = "{\n"
                            + "  \"realtimeInput\": {\n"
                            + "    \"audio\": {\n"
                            + "      \"mimeType\": \"audio/pcm;rate=16000\",\n"
                            + "      \"data\": \"" + base64Chunk + "\"\n"
                            + "    }\n"
                            + "  }\n"
                            + "}";
                    ws.sendText(realtimeChunk, true);
                }
            }
        });

        // Screen Capture Video Thread (NetBeans Application Frame at 1 FPS via realtimeInput.video)
        Robot robot = new Robot();
        Thread.ofVirtual().start(() -> {
            while (sessionActive.get()) {
                try {
                    Thread.sleep(1500); // Send 1 video frame every 1.5 seconds

                    Rectangle appBounds;
                    try {
                        Window activeWin = KeyboardFocusManager.getCurrentKeyboardFocusManager().getActiveWindow();
                        if (activeWin != null && activeWin.isShowing()) {
                            Point loc = activeWin.getLocationOnScreen();
                            Dimension size = activeWin.getSize();
                            appBounds = new Rectangle(loc.x, loc.y, size.width, size.height);
                        } else {
                            appBounds = GraphicsEnvironment.getLocalGraphicsEnvironment().getDefaultScreenDevice().getDefaultConfiguration().getBounds();
                        }
                    } catch (Throwable t) {
                        appBounds = new Rectangle(0, 0, 1920, 1080);
                    }

                    BufferedImage capture = robot.createScreenCapture(appBounds);

                    int targetWidth = Math.min(1024, capture.getWidth());
                    int targetHeight = (int) (((double) targetWidth / capture.getWidth()) * capture.getHeight());

                    Image scaledImg = capture.getScaledInstance(targetWidth, targetHeight, Image.SCALE_FAST);
                    BufferedImage scaledBuffered = new BufferedImage(targetWidth, targetHeight, BufferedImage.TYPE_INT_RGB);
                    Graphics2D g2 = scaledBuffered.createGraphics();
                    g2.drawImage(scaledImg, 0, 0, null);
                    g2.dispose();

                    ByteArrayOutputStream baos = new ByteArrayOutputStream();
                    ImageIO.write(scaledBuffered, "jpg", baos);
                    byte[] jpegBytes = baos.toByteArray();

                    String base64Jpeg = Base64.getEncoder().encodeToString(jpegBytes);
                    String videoFrameJson = "{\n"
                            + "  \"realtimeInput\": {\n"
                            + "    \"video\": {\n"
                            + "      \"mimeType\": \"image/jpeg\",\n"
                            + "      \"data\": \"" + base64Jpeg + "\"\n"
                            + "    }\n"
                            + "  }\n"
                            + "}";

                    ws.sendText(videoFrameJson, true);
                    guiPanel.incrementFrames();
                } catch (Exception e) {
                    // ignore
                }
            }
        });

        System.out.println("Session active for 60 seconds... Talk and test screen vision!");
        sessionLatch.await(60, TimeUnit.SECONDS);

        sessionActive.set(false);
        micLine.stop();
        micLine.close();
        speakerLine.drain();
        speakerLine.stop();
        speakerLine.close();
        ws.sendClose(WebSocket.NORMAL_CLOSURE, "Done");

        SwingUtilities.invokeLater(() -> {
            frame.setVisible(false);
            frame.dispose();
        });

        System.out.println("Standalone Gemini Live Test Complete!");
    }
}
