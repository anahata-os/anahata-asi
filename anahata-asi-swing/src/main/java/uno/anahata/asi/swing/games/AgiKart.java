/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.games;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.RenderingHints;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.geom.GeneralPath;
import java.awt.image.BufferedImage;
import java.awt.image.DataBufferInt;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import javax.sound.midi.MidiSystem;
import javax.sound.midi.MidiChannel;
import javax.sound.midi.Synthesizer;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;
import javax.swing.Timer;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.swing.icons.AgiKartIcon;

/**
 * AgiKart is a high-performance pseudo-3D retro kart racing game in Java Swing.
 * <p>
 * Utilizing a Mode 7 style perspective ground projection algorithm, depth-sorted
 * 3D billboard rendering for sprites (trees, item boxes, karts, and obstacles),
 * responsive drift-and-slide kart physics, and waypoint-following AI opponents,
 * it provides a highly polished, authentic classic 16-bit racing experience.
 * It also features procedural audio synthesis using Java's built-in MIDI Synthesizer.
 * </p>
 * 
 * @author anahata
 */
@Slf4j
public class AgiKart extends JPanel implements ActionListener {

    /**
     * The logical screen width for the Mode 7 low-res rendering buffer.
     */
    private static final int SCREEN_WIDTH = 320;
    /**
     * The logical screen height for the Mode 7 low-res rendering buffer.
     */
    private static final int SCREEN_HEIGHT = 240;
    /**
     * The actual display width of the game window.
     */
    private static final int WINDOW_WIDTH = 800;
    /**
     * The actual display height of the game window.
     */
    private static final int WINDOW_HEIGHT = 600;

    /**
     * Coordinates representing the center loop of the race track.
     */
    private static final double[][] TRACK_POINTS = {
        { 150, 450 },
        { 140, 300 },
        { 180, 180 },
        { 320, 120 },
        { 500, 140 },
        { 650, 120 },
        { 800, 160 },
        { 880, 280 },
        { 850, 450 },
        { 750, 580 },
        { 820, 720 },
        { 780, 850 },
        { 600, 880 },
        { 450, 820 },
        { 300, 750 },
        { 200, 600 }
    };

    /**
     * Center coordinates for item box spawners.
     */
    private static final double[][] ITEM_BOX_LOCATIONS = {
        { 140, 300 },
        { 500, 140 },
        { 880, 280 },
        { 820, 720 },
        { 450, 820 }
    };

    /**
     * List of static tree coordinates placed alongside the road borders.
     */
    private final List<PineTree> trees = new ArrayList<>();
    /**
     * List of active item boxes in the world.
     */
    private final List<ItemBox> itemBoxes = new ArrayList<>();
    /**
     * List of bananas placed on the track.
     */
    private final List<PlacedBanana> bananas = new ArrayList<>();
    /**
     * List of active red shells traveling on the track.
     */
    private final List<ThrownShell> shells = new ArrayList<>();
    /**
     * List of active roadside collectible Super Mushrooms.
     */
    private final List<TrackMushroom> trackMushrooms = new ArrayList<>();

    /**
     * The human player entity.
     */
    private KartEntity player;
    /**
     * The list of AI competitor entities.
     */
    private final List<KartEntity> opponents = new ArrayList<>();

    // Mode 7 Camera Settings
    /**
     * Height of the camera above the ground plane.
     */
    private final double cameraHeight = 35.0;
    /**
     * Field of View perspective scaling factor.
     */
    private double fov = 1.25;
    /**
     * Horizon row index on the low-res screen buffer.
     */
    private final int horizon = 85;

    // Rendering Buffers
    /**
     * Low-resolution rendering canvas.
     */
    private BufferedImage screenBuffer;
    /**
     * Direct pointer to the low-resolution canvas pixels.
     */
    private int[] screenPixels;
    /**
     * High-resolution, off-screen track texture map.
     */
    private BufferedImage trackTexture;
    /**
     * Direct pointer to the track texture map pixels.
     */
    private int[] trackPixels;

    // Loop & Controls
    /**
     * High-frequency game loop timer.
     */
    private Timer timer;
    /**
     * Steering flag (turning left).
     */
    private boolean keyLeft = false;
    /**
     * Steering flag (turning right).
     */
    private boolean keyRight = false;
    /**
     * Acceleration flag.
     */
    private boolean keyUp = false;
    /**
     * Braking / Reverse flag.
     */
    private boolean keyDown = false;

    // Race State
    /**
     * Flag indicating if the race has started (after countdown).
     */
    private boolean raceStarted = false;
    /**
     * The starting countdown timer frames.
     */
    private int countdownFrames = 180; // 3 seconds at 60 FPS
    /**
     * Flag indicating if the player has finished all laps.
     */
    private boolean gameOver = false;
    /**
     * Global frame tick counter.
     */
    private int gameTicks = 0;
    /**
     * Direct blaster shell fire cooldown.
     */
    private int shootCooldown = 0;
    /**
     * Warning triggered when the player enters the final lap.
     */
    private boolean finalLapWarningPlayed = false;
    /**
     * Camera screen shake intensity timer.
     */
    private int screenShake = 0;

    /**
     * Coordinates for stationary roadside Oil Slicks that trigger spinouts.
     */
    private static final double[][] OIL_SLICKS = {
        { 320, 180 },
        { 680, 200 },
        { 780, 800 },
        { 310, 680 }
    };

    // Sound System
    /**
     * Single-threaded pool for non-blocking procedural sound playbacks.
     */
    private final ExecutorService soundPool = Executors.newSingleThreadExecutor();
    /**
     * MIDI Synthesizer interface.
     */
    private Synthesizer synth;
    /**
     * The main MIDI channel for lead synthesizers.
     */
    private MidiChannel midiChannel;
    /**
     * Continuous MIDI channel for engine humming sound.
     */
    private MidiChannel engineChannel;

    // Race Standings and Speedrunning Timers
    private long raceStartTime = 0;
    private long currentLapStartTime = 0;
    private double bestLapTime = -1.0;
    private double lastLapTime = -1.0;

    // Particle System
    private final List<Particle> particles = new ArrayList<>();

    /**
     * Constructs a new AgiKart game panel and triggers resources initialization.
     */
    public AgiKart() {
        setPreferredSize(new Dimension(WINDOW_WIDTH, WINDOW_HEIGHT));
        setBackground(Color.BLACK);
        setFocusable(true);
        initSound();
        generateTrackTexture();
        initEnvironment();
        initKarts();
        initControls();
        
        timer = new Timer(16, this);
        timer.start();
    }

    /**
     * Dispatches a musical note command asynchronously to prevent blocking the game thread.
     */
    private void playNoteAsync(int note, int velocity, int durationMs) {
        if (midiChannel == null) {
            return;
        }
        soundPool.submit(() -> {
            try {
                midiChannel.noteOn(note, velocity);
                Thread.sleep(durationMs);
                midiChannel.noteOff(note);
            } catch (Exception ex) {
                log.warn("MIDI note playback failed: {}", ex.getMessage());
            }
        });
    }

    /**
     * Plays a high-pitched ding sound when picking up items.
     */
    private void playItemPickupSound() {
        playNoteAsync(72, 85, 80);
        try {
            Thread.sleep(80);
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }
        playNoteAsync(76, 85, 120);
    }

    /**
     * Plays a sweeping pitch sound during a mushroom boost.
     */
    private void playBoostSound() {
        soundPool.submit(() -> {
            try {
                for (int note = 60; note <= 82; note += 3) {
                    if (midiChannel != null) {
                        midiChannel.noteOn(note, 80);
                    }
                    Thread.sleep(30);
                    if (midiChannel != null) {
                        midiChannel.noteOff(note);
                    }
                }
            } catch (Exception ex) {
                // Ignore silently
            }
        });
    }

    /**
     * Plays a descending pitch sound when a kart spins out.
     */
    private void playCrashSound() {
        soundPool.submit(() -> {
            try {
                for (int note = 70; note >= 44; note -= 4) {
                    if (midiChannel != null) {
                        midiChannel.noteOn(note, 90);
                    }
                    Thread.sleep(40);
                    if (midiChannel != null) {
                        midiChannel.noteOff(note);
                    }
                }
            } catch (Exception ex) {
                // Ignore silently
            }
        });
    }

    /**
     * Plays a high-intensity warning fanfare when entering the final lap.
     */
    private void playFinalLapSound() {
        soundPool.submit(() -> {
            try {
                int[] notes = { 72, 72, 72, 76, 72, 76, 81 };
                int[] delays = { 80, 80, 80, 160, 80, 80, 400 };
                for (int i = 0; i < notes.length; i++) {
                    if (midiChannel != null) {
                        midiChannel.noteOn(notes[i], 95);
                    }
                    Thread.sleep(delays[i]);
                    if (midiChannel != null) {
                        midiChannel.noteOff(notes[i]);
                    }
                }
            } catch (Exception ex) {
                // Ignore silently
            }
        });
    }

    /**
     * Plays a grand championship lap completed fanfare.
     */
    private void playLapCompletedSound() {
        soundPool.submit(() -> {
            try {
                int[] notes = { 60, 64, 67, 72, 67, 72 };
                int[] delays = { 120, 120, 120, 240, 120, 350 };
                for (int i = 0; i < notes.length; i++) {
                    if (midiChannel != null) {
                        midiChannel.noteOn(notes[i], 90);
                    }
                    Thread.sleep(delays[i]);
                    if (midiChannel != null) {
                        midiChannel.noteOff(notes[i]);
                    }
                }
            } catch (Exception ex) {
                // Ignore silently
            }
        });
    }

    /**
     * Initializes the Java MIDI Synthesizer and configures instrument patching.
     */
    private void initSound() {
        try {
            synth = MidiSystem.getSynthesizer();
            synth.open();
            midiChannel = synth.getChannels()[0];
            // Patch 80 represents a retro Lead synth (Square Wave)
            midiChannel.programChange(80);

            // Channel 1 is used for pitch-bendable engine hum
            engineChannel = synth.getChannels()[1];
            engineChannel.programChange(38); // Synth Bass 1
        } catch (Exception e) {
            log.error("Failed to initialize MIDI Synthesizer: {}", e.getMessage());
        }
    }

    /**
     * Generates a beautiful 1024x1024 track texture procedurally in memory.
     * <p>
     * Renders a forest green background grid, draws a custom winding race course,
     * overlays red-and-white striped curb borders, and stamps a checkered finish line.
     * </p>
     */
    private void generateTrackTexture() {
        trackTexture = new BufferedImage(1024, 1024, BufferedImage.TYPE_INT_RGB);
        Graphics2D g = trackTexture.createGraphics();
        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

        // Grass Background with checkered tiles
        g.setColor(new Color(34, 139, 34)); // Lush dark green
        g.fillRect(0, 0, 1024, 1024);
        g.setColor(new Color(46, 204, 113)); // Lighter forest green
        for (int i = 0; i < 1024; i += 64) {
            for (int j = 0; j < 1024; j += 64) {
                if (((i / 64) + (j / 64)) % 2 == 0) {
                    g.fillRect(i, j, 64, 64);
                }
            }
        }

        // Build Track Path
        GeneralPath path = new GeneralPath();
        path.moveTo(TRACK_POINTS[0][0], TRACK_POINTS[0][1]);
        for (int i = 1; i < TRACK_POINTS.length; i++) {
            path.lineTo(TRACK_POINTS[i][0], TRACK_POINTS[i][1]);
        }
        path.closePath();

        // 1. Red outer solid curb outline
        g.setStroke(new BasicStroke(132f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND));
        g.setColor(new Color(220, 53, 69));
        g.draw(path);

        // 2. White dashed inner curb outline
        float[] dashPattern = { 30f, 30f };
        g.setStroke(new BasicStroke(132f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND, 10f, dashPattern, 0f));
        g.setColor(Color.WHITE);
        g.draw(path);

        // 3. Dark Asphalt road surface
        g.setStroke(new BasicStroke(120f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND));
        g.setColor(new Color(75, 75, 75)); // Charcoal Grey
        g.draw(path);

        // 4. Checkered start/finish line
        g.setColor(Color.WHITE);
        g.setStroke(new BasicStroke(12f));
        g.drawLine(150 - 60, 450, 150 + 60, 450);

        g.setColor(Color.BLACK);
        g.setStroke(new BasicStroke(6f));
        for (int x = 150 - 60; x < 150 + 60; x += 12) {
            g.drawLine(x, 447, x + 6, 447);
            g.drawLine(x + 6, 453, x + 12, 453);
        }

        // 5. Draw Oil Slicks on track texture
        g.setColor(new Color(25, 25, 25)); // Slick glossy dark black
        for (double[] slick : OIL_SLICKS) {
            g.fillOval((int) slick[0] - 15, (int) slick[1] - 15, 30, 30);
            g.setColor(new Color(40, 40, 40));
            g.drawOval((int) slick[0] - 15, (int) slick[1] - 15, 30, 30);
        }

        g.dispose();

        // Bind direct pixel buffer access for maximum blitting performance
        trackPixels = ((DataBufferInt) trackTexture.getRaster().getDataBuffer()).getData();

        // Initialize low-resolution perspective screen buffer
        screenBuffer = new BufferedImage(SCREEN_WIDTH, SCREEN_HEIGHT, BufferedImage.TYPE_INT_RGB);
        screenPixels = ((DataBufferInt) screenBuffer.getRaster().getDataBuffer()).getData();
    }

    /**
     * Programmatically populates environment details, including curbside pine trees and item boxes.
     */
    private void initEnvironment() {
        trees.clear();
        itemBoxes.clear();
        trackMushrooms.clear();

        // Spawn collectible Roadside Mushrooms
        trackMushrooms.add(new TrackMushroom(320, 120));
        trackMushrooms.add(new TrackMushroom(750, 580));
        trackMushrooms.add(new TrackMushroom(300, 750));

        // Place pine trees along both sides of each track waypoint
        for (int i = 0; i < TRACK_POINTS.length; i++) {
            double[] p1 = TRACK_POINTS[i];
            double[] p2 = TRACK_POINTS[(i + 1) % TRACK_POINTS.length];

            double dx = p2[0] - p1[0];
            double dy = p2[1] - p1[1];
            double len = Math.sqrt(dx * dx + dy * dy);
            if (len == 0.0) {
                continue;
            }

            double nx = -dy / len;
            double ny = dx / len;

            // Curbside pine trees placement (left and right of the track path)
            trees.add(new PineTree(p1[0] + nx * 85.0, p1[1] + ny * 85.0));
            trees.add(new PineTree(p1[0] - nx * 85.0, p1[1] - ny * 85.0));
        }

        // Add item boxes
        for (double[] loc : ITEM_BOX_LOCATIONS) {
            itemBoxes.add(new ItemBox(loc[0], loc[1]));
        }
    }

    /**
     * Initializes karts, setting up a starting grid behind the start-finish line.
     */
    private void initKarts() {
        opponents.clear();
        bananas.clear();
        shells.clear();

        // Calculate start direction heading angle
        double[] p1 = TRACK_POINTS[TRACK_POINTS.length - 1];
        double[] p2 = TRACK_POINTS[0];
        double startAngle = Math.atan2(p2[1] - p1[1], p2[0] - p1[0]);

        double dirX = Math.cos(startAngle);
        double dirY = Math.sin(startAngle);
        double normX = -Math.sin(startAngle);
        double normY = Math.cos(startAngle);

        // Lined up nicely on a starting grid
        player = new KartEntity("Player", 150 + dirX * -70 + normX * 18, 450 + dirY * -70 + normY * 18, new Color(0, 123, 255)); // Barça Blue
        player.angle = startAngle;

        KartEntity o1 = new KartEntity("Gemini", 150 + dirX * -45 - normX * 18, 450 + dirY * -45 - normY * 18, new Color(220, 53, 69)); // Barça Red
        o1.angle = startAngle;
        opponents.add(o1);

        KartEntity o2 = new KartEntity("Kryo", 150 + dirX * -95 + normX * 18, 450 + dirY * -95 + normY * 18, new Color(155, 89, 182)); // Purple
        o2.angle = startAngle;
        opponents.add(o2);

        KartEntity o3 = new KartEntity("Bug", 150 + dirX * -120 - normX * 18, 450 + dirY * -120 - normY * 18, new Color(46, 204, 113)); // Green
        o3.angle = startAngle;
        opponents.add(o3);
    }

    /**
     * Binds keyboard controls to manage player movement and item triggers.
     */
    private void initControls() {
        addKeyListener(new KeyAdapter() {
            @Override
            public void keyPressed(KeyEvent e) {
                int code = e.getKeyCode();
                if (code == KeyEvent.VK_LEFT) {
                    keyLeft = true;
                } else if (code == KeyEvent.VK_RIGHT) {
                    keyRight = true;
                } else if (code == KeyEvent.VK_UP) {
                    keyUp = true;
                } else if (code == KeyEvent.VK_DOWN) {
                    keyDown = true;
                } else if (code == KeyEvent.VK_SPACE) {
                    if (gameOver) {
                        resetGame();
                    } else {
                        triggerPlayerItem();
                    }
                } else if (code == KeyEvent.VK_F) {
                    shootGreenShellDirect();
                }
            }

            @Override
            public void keyReleased(KeyEvent e) {
                int code = e.getKeyCode();
                if (code == KeyEvent.VK_LEFT) {
                    keyLeft = false;
                } else if (code == KeyEvent.VK_RIGHT) {
                    keyRight = false;
                } else if (code == KeyEvent.VK_UP) {
                    keyUp = false;
                } else if (code == KeyEvent.VK_DOWN) {
                    keyDown = false;
                }
            }
        });
    }

    /**
     * Triggers the player's active item and manages world placements.
     */
    private void triggerPlayerItem() {
        if (player.currentItem == null || "SPINNING".equals(player.currentItem)) {
            return;
        }

        switch (player.currentItem) {
            case "MUSHROOM" -> {
                player.boostTimer = 90; // 1.5s boost
                playBoostSound();
            }
            case "BANANA" -> {
                double cos = Math.cos(player.angle);
                double sin = Math.sin(player.angle);
                bananas.add(new PlacedBanana(player.x - cos * 35.0, player.y - sin * 35.0));
                playNoteAsync(55, 80, 100);
            }
            case "RED_SHELL" -> {
                shells.add(new ThrownShell(player.x, player.y, player.nextCheckpoint, "Player", true, player.angle));
                playNoteAsync(68, 80, 150);
            }
            case "GREEN_SHELL" -> {
                shells.add(new ThrownShell(player.x, player.y, player.nextCheckpoint, "Player", false, player.angle));
                playNoteAsync(66, 80, 150);
            }
        }

        player.currentItem = null;
    }

    /**
     * Fires a green shell directly in front of the player's kart with a cooldown.
     */
    private void shootGreenShellDirect() {
        if (shootCooldown > 0 || gameOver || !raceStarted) {
            return;
        }
        shells.add(new ThrownShell(player.x, player.y, player.nextCheckpoint, "Player", false, player.angle));
        playNoteAsync(66, 85, 120);
        shootCooldown = 45; // 0.75s cooldown
    }

    /**
     * Resets the entire race standings and parameters to begin anew.
     */
    private void resetGame() {
        raceStarted = false;
        gameOver = false;
        countdownFrames = 180;
        gameTicks = 0;
        finalLapWarningPlayed = false;
        initEnvironment();
        initKarts();
    }

    /**
     * Shuts down background threads and releases MIDI Synthesizer resources.
     */
    public void cleanup() {
        if (timer != null) {
            timer.stop();
        }
        if (engineChannel != null) {
            try {
                engineChannel.noteOff(38);
            } catch (Exception e) {
                // Ignore
            }
        }
        try {
            soundPool.shutdownNow();
        } catch (Exception e) {
            // Ignore
        }
        if (synth != null && synth.isOpen()) {
            try {
                synth.close();
            } catch (Exception e) {
                // Ignore
            }
        }
    }

    /**
     * {@inheritDoc}
     * <p>Processes the 3D projection, depth-sorted blitting, and HUD rendering overlays.</p>
     */
    @Override
    protected void paintComponent(Graphics g) {
        super.paintComponent(g);

        // 1. Cast the ground perspective and clear the sky gradient
        renderMode7();

        // 2. Project, depth-sort, and draw 3D billboarded objects
        Graphics2D gBuffer = screenBuffer.createGraphics();
        gBuffer.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        renderBackgroundSceneries(gBuffer);
        render3DSprites(gBuffer);
        gBuffer.dispose();

        // Apply screen shake offsets if active
        int shakeX = 0;
        int shakeY = 0;
        if (screenShake > 0) {
            shakeX = (int) ((Math.random() - 0.5) * screenShake * 2.0);
            shakeY = (int) ((Math.random() - 0.5) * screenShake * 2.0);
            screenShake--; // Decrement shake timer
        }

        // 3. Blit scaled low-res canvas to the high-res panel
        g.drawImage(screenBuffer, shakeX, shakeY, getWidth(), getHeight(), null);

        // 4. Render crisp, vector-based HUD layer
        Graphics2D g2 = (Graphics2D) g;
        g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        renderHUD(g2);
    }

    /**
     * Implements a fast, hardware-friendly Mode 7 ground perspective floor caster.
     */
    private void renderMode7() {
        double cos = Math.cos(player.angle);
        double sin = Math.sin(player.angle);

        // Position camera behind and slightly above the player
        double camX = player.x - 45.0 * cos;
        double camY = player.y - 45.0 * sin;

        // Render vertical Sky gradient first
        for (int y = 0; y < horizon; y++) {
            float ratio = (float) y / horizon;
            int r = (int) (12 + ratio * 45);
            int g = (int) (15 + ratio * 85);
            int b = (int) (45 + ratio * 200);
            int rgb = (r << 16) | (g << 8) | b;
            int rowOffset = y * SCREEN_WIDTH;
            for (int x = 0; x < SCREEN_WIDTH; x++) {
                screenPixels[rowOffset + x] = rgb;
            }
        }

        // Cast Mode 7 Floor
        for (int y = horizon; y < SCREEN_HEIGHT; y++) {
            double z = cameraHeight * SCREEN_HEIGHT / (y - horizon);

            // Row step vectors
            double stepX = (z * -sin * fov) / SCREEN_WIDTH;
            double stepY = (z * cos * fov) / SCREEN_WIDTH;

            // Start leftmost coordinate
            double startX = camX + (z * cos) - (SCREEN_WIDTH / 2.0) * stepX;
            double startY = camY + (z * sin) - (SCREEN_WIDTH / 2.0) * stepY;

            int rowOffset = y * SCREEN_WIDTH;
            for (int x = 0; x < SCREEN_WIDTH; x++) {
                double tx = startX + x * stepX;
                double ty = startY + x * stepY;

                // Texture wrapping (1024x1024)
                int mapX = ((int) tx) & 1023;
                int mapY = ((int) ty) & 1023;

                screenPixels[rowOffset + x] = trackPixels[mapY * 1024 + mapX];
            }
        }
    }

    /**
     * Renders background mountain sceneries scrolling smoothly as the player turns.
     */
    private void renderBackgroundSceneries(Graphics2D g) {
        double angleRatio = player.angle / (2 * Math.PI);
        double scrollX = (angleRatio * SCREEN_WIDTH * 2.2) % SCREEN_WIDTH;

        g.setColor(new Color(46, 117, 89)); // Dark forest hills
        for (int i = -1; i < 3; i++) {
            int hx = (int) (i * SCREEN_WIDTH / 2 + scrollX);
            int[] mx = { hx, hx + SCREEN_WIDTH / 4, hx + SCREEN_WIDTH / 2 };
            int[] my = { horizon, horizon - 20, horizon };
            g.fillPolygon(mx, my, 3);
        }

        // Fluffy retro clouds
        g.setColor(new Color(255, 255, 255, 140));
        double cloudScroll = (scrollX * 0.45) % SCREEN_WIDTH;
        for (int i = -1; i < 3; i++) {
            int cx = (int) (i * 120 + cloudScroll);
            g.fillOval(cx, 18, 32, 14);
            g.fillOval(cx + 8, 14, 24, 12);
        }
    }

    /**
     * Gathers, depth-sorts, and draws all 3D projected billboard sprites on the screen.
     */
    private void render3DSprites(Graphics2D g) {
        List<SpriteProjection> list = new ArrayList<>();

        double cos = Math.cos(player.angle);
        double sin = Math.sin(player.angle);

        // Camera position behind player
        double camX = player.x - 45.0 * cos;
        double camY = player.y - 45.0 * sin;

        // 1. Gather opponents karts
        for (KartEntity opponent : opponents) {
            SpriteProjection proj = projectWorldPoint(opponent.x, opponent.y, 0, camX, camY, cos, sin);
            if (proj != null) {
                proj.drawable = (g2, sx, sy, scale) -> drawWorldKart(g2, sx, sy, scale, opponent.color, opponent.name, opponent.angle, opponent.spinTimer);
                list.add(proj);
            }
        }

        // 2. Gather pine trees
        for (PineTree tree : trees) {
            SpriteProjection proj = projectWorldPoint(tree.x, tree.y, 0, camX, camY, cos, sin);
            if (proj != null) {
                proj.drawable = this::drawPineTree;
                list.add(proj);
            }
        }

        // 3. Gather active item boxes
        for (ItemBox box : itemBoxes) {
            if (!box.active) {
                continue;
            }
            // Animate hovering height
            double hoverHeight = 2.0 + Math.sin(gameTicks * 0.15) * 0.6;
            SpriteProjection proj = projectWorldPoint(box.x, box.y, hoverHeight, camX, camY, cos, sin);
            if (proj != null) {
                proj.drawable = this::drawItemBox;
                list.add(proj);
            }
        }

        // 4. Gather placed bananas
        for (PlacedBanana banana : bananas) {
            SpriteProjection proj = projectWorldPoint(banana.x, banana.y, 0, camX, camY, cos, sin);
            if (proj != null) {
                proj.drawable = this::drawPlacedBanana;
                list.add(proj);
            }
        }

        // 5. Gather thrown shells
        for (ThrownShell shell : shells) {
            SpriteProjection proj = projectWorldPoint(shell.x, shell.y, 0.8, camX, camY, cos, sin);
            if (proj != null) {
                proj.drawable = this::drawThrownShell;
                list.add(proj);
            }
        }

        // 6. Gather collectible roadside mushrooms
        for (TrackMushroom shroom : trackMushrooms) {
            if (!shroom.active) {
                continue;
            }
            SpriteProjection proj = projectWorldPoint(shroom.x, shroom.y, 0, camX, camY, cos, sin);
            if (proj != null) {
                proj.drawable = (g2, sx, sy, scale) -> drawTrackMushroom(g2, sx, sy, scale);
                list.add(proj);
            }
        }

        // 7. Gather active projected particles
        for (Particle p : particles) {
            SpriteProjection proj = projectWorldPoint(p.x, p.y, 0.1, camX, camY, cos, sin);
            if (proj != null) {
                proj.drawable = (g2, sx, sy, scale) -> {
                    int pSize = (int) (Math.max(2, 5 * scale));
                    g2.setColor(p.color);
                    g2.fillOval((int) (sx - pSize * 0.5), (int) (sy - pSize * 0.5), pSize, pSize);
                };
                list.add(proj);
            }
        }

        // Sort sprites by distance descending (Painter's Algorithm)
        list.sort(Comparator.comparingDouble((SpriteProjection a) -> a.depth).reversed());

        // Draw sprites
        for (SpriteProjection sp : list) {
            sp.drawable.draw(g, sp.screenX, sp.screenY, sp.scale);
        }

        // 6. Draw the player's kart statically in the foreground center-bottom
        int px = SCREEN_WIDTH / 2;
        int py = SCREEN_HEIGHT - 32;
        double pScale = 1.0;
        drawWorldKart(g, px, py, pScale, player.color, player.name, player.angle, player.spinTimer);
    }

    /**
     * Projects a 3D world coordinate onto the low-resolution screen canvas relative to the camera.
     */
    private SpriteProjection projectWorldPoint(double wx, double wy, double wz, double camX, double camY, double cos, double sin) {
        double dx = wx - camX;
        double dy = wy - camY;

        // Rotate point relative to camera angle
        double cx = dx * -sin + dy * cos;
        double cy = dx * cos + dy * sin;

        // Distance clipping to prevent scale explosion or behind-camera glitches
        if (cy <= 12.0 || cy > 450.0) {
            return null; // Behind camera or too far away
        }

        double screenX = SCREEN_WIDTH / 2.0 + (cx * SCREEN_WIDTH) / (cy * fov);
        
        // Mathematically matched perspective projection to align perfectly with Mode 7 floor casting
        double screenY = horizon + (cameraHeight - wz) * SCREEN_HEIGHT / cy;

        // Perspective scale factor matches screen density
        double scale = 155.0 / cy;

        return new SpriteProjection(screenX, screenY, scale, cy);
    }

    /**
     * Vector draws a beautiful pine tree billboard sprite.
     */
    private void drawPineTree(Graphics2D g, double sx, double sy, double scale) {
        int w = (int) (18 * scale);
        int h = (int) (38 * scale);
        if (w < 2 || h < 2) {
            return;
        }

        // Trunk
        g.setColor(new Color(110, 75, 45));
        g.fillRect((int) (sx - w * 0.12), (int) (sy - h * 0.2), (int) (w * 0.24), (int) (h * 0.2));

        // Foliage
        g.setColor(new Color(39, 90, 48));
        int[] xPoints = { (int) (sx - w * 0.5), (int) sx, (int) (sx + w * 0.5) };
        int[] yPoints = { (int) (sy - h * 0.2), (int) (sy - h * 0.8), (int) (sy - h * 0.2) };
        g.fillPolygon(xPoints, yPoints, 3);

        int[] xPoints2 = { (int) (sx - w * 0.35), (int) sx, (int) (sx + w * 0.35) };
        int[] yPoints2 = { (int) (sy - h * 0.5), (int) (sy - h * 1.0), (int) (sy - h * 0.5) };
        g.fillPolygon(xPoints2, yPoints2, 3);
    }

    /**
     * Vector draws a hovering, glowing item box spawner.
     */
    private void drawItemBox(Graphics2D g, double sx, double sy, double scale) {
        int size = (int) (14 * scale);
        if (size < 2) {
            return;
        }

        // Holographic neon glow
        g.setColor(new Color(0, 200, 255, 140));
        g.fillRoundRect((int) (sx - size * 0.5), (int) (sy - size * 0.5), size, size, size / 4, size / 4);

        g.setColor(Color.WHITE);
        g.drawRoundRect((int) (sx - size * 0.5), (int) (sy - size * 0.5), size, size, size / 4, size / 4);

        // Question mark label
        g.setFont(new Font("Monospaced", Font.BOLD, Math.max(7, (int) (10 * scale))));
        g.drawString("?", (int) (sx - size * 0.16), (int) (sy + size * 0.28));
    }

    /**
     * Vector draws a placed banana peel on the road.
     */
    private void drawPlacedBanana(Graphics2D g, double sx, double sy, double scale) {
        int size = (int) (12 * scale);
        if (size < 2) {
            return;
        }
        Graphics2D g2 = (Graphics2D) g.create();
        g2.translate((int) sx, (int) sy);

        // Shadow underneath
        g2.setColor(new Color(0, 0, 0, 100));
        g2.fillOval(-size / 2, -size / 10, size, size / 5);

        // Left peeled skin
        g2.setColor(new Color(241, 196, 15)); // Yellow
        g2.fillArc(-size / 2, -size / 3, size * 2 / 3, size * 2 / 3, 45, 135);
        // Right peeled skin
        g2.fillArc(-size / 6, -size / 3, size * 2 / 3, size * 2 / 3, 0, 135);
        // Brown center tip
        g2.setColor(new Color(120, 100, 0));
        g2.fillRect(-size / 20, size / 6, size / 10, size / 8);

        g2.dispose();
    }

    /**
     * Vector draws a gorgeous 3D billboard mushroom standing on the ground.
     */
    private void drawTrackMushroom(Graphics2D g, double sx, double sy, double scale) {
        int size = (int) (14 * scale);
        if (size < 2) {
            return;
        }
        Graphics2D g2 = (Graphics2D) g.create();
        g2.translate((int) sx, (int) sy);

        // Shadow
        g2.setColor(new Color(0, 0, 0, 90));
        g2.fillOval(-size / 2, -size / 10, size, size / 5);

        // Stem
        g2.setColor(Color.WHITE);
        g2.fillRect(-size / 6, -size / 2, size / 3, size / 2);
        g2.setColor(Color.BLACK);
        g2.drawRect(-size / 6, -size / 2, size / 3, size / 2);
        // Eyes
        g2.fillRect(-size / 12, -size / 3, size / 30, size / 8);
        g2.fillRect(size / 12 - size / 30, -size / 3, size / 30, size / 8);

        // Cap (Red Dome)
        g2.setColor(new Color(231, 76, 60));
        g2.fillArc(-size / 2, -size, size, size, 0, 180);
        g2.setColor(Color.BLACK);
        g2.drawArc(-size / 2, -size, size, size, 0, 180);
        g2.drawLine(-size / 2, -size / 2, size / 2, -size / 2);

        // Spots
        g2.setColor(Color.WHITE);
        g2.fillOval(-size / 8, -size * 7 / 8, size / 5, size / 5);
        g2.fillOval(-size * 4 / 10, -size * 3 / 4, size / 6, size / 6);
        g2.fillOval(size * 2 / 10, -size * 3 / 4, size / 6, size / 6);

        g2.dispose();
    }

    /**
     * Vector draws a traveling homing red shell on the road.
     */
    private void drawThrownShell(Graphics2D g, double sx, double sy, double scale) {
        int size = (int) (9 * scale);
        if (size < 2) {
            return;
        }
        // Base red shell
        g.setColor(new Color(231, 76, 60));
        g.fillOval((int) (sx - size * 0.5), (int) (sy - size * 0.5), size, size);

        // Highlights & borders
        g.setColor(Color.WHITE);
        g.fillOval((int) (sx - size * 0.25), (int) (sy - size * 0.25), size / 4, size / 4);

        g.setColor(Color.BLACK);
        g.drawOval((int) (sx - size * 0.5), (int) (sy - size * 0.5), size, size);
    }

    /**
     * Vector draws a gorgeous detailed kart competitor with chassis, wheels, driver, and nametag.
     */
    private void drawWorldKart(Graphics2D g, double sx, double sy, double scale, Color color, String name, double angle, int spinTimer) {
        int w = (int) (18 * scale);
        int h = (int) (16 * scale);
        if (w < 2 || h < 2) {
            return;
        }

        Graphics2D g2 = (Graphics2D) g.create();
        g2.translate((int) sx, (int) sy);

        if (spinTimer > 0) {
            g2.rotate(spinTimer * 0.22);
        }

        // 1. Shadow underneath
        g2.setColor(new Color(0, 0, 0, 95));
        g2.fillOval(-w / 2, -h / 5, w, h / 3);

        // 2. Rear chunky tires
        g2.setColor(new Color(30, 30, 30));
        g2.fillRect(-w / 2 - 3, -h / 4, 5, h / 2);
        g2.fillRect(w / 2 - 2, -h / 4, 5, h / 2);

        // 3. Front smaller tires
        g2.fillRect(-w / 3 - 2, (int) (-h * 0.8), 3, h / 4);
        g2.fillRect(w / 3 - 1, (int) (-h * 0.8), 3, h / 4);

        // 4. Custom Chassis
        g2.setColor(color);
        int[] xc = { -w / 4, w / 4, w / 3, -w / 3 };
        int[] yc = { -h / 4, -h / 4, (int) (-h * 0.75), (int) (-h * 0.75) };
        g2.fillPolygon(xc, yc, 4);

        // 5. Driver helmet (white base + colorful top dome)
        g2.setColor(Color.WHITE);
        g2.fillOval(-w / 5, (int) (-h * 1.15), (int) (w * 0.4), (int) (h * 0.4));
        g2.setColor(color);
        g2.fillOval(-w / 6, (int) (-h * 1.12), (int) (w * 0.33), (int) (h * 0.33));

        // Visor
        g2.setColor(Color.BLACK);
        g2.fillRect(-w / 8, (int) (-h * 1.05), w / 4, h / 11);

        // 6. Overhead driver tag (high-contrast label)
        g2.setColor(Color.WHITE);
        g2.setFont(new Font("Monospaced", Font.BOLD, Math.max(7, (int) (8 * scale))));
        FontMetrics fm = g2.getFontMetrics();
        g2.drawString(name, -fm.stringWidth(name) / 2, (int) (-h * 1.3));

        g2.dispose();
    }

    /**
     * Renders a highly-polished high-resolution HUD, featuring real-time minimaps and race positions.
     */
    private void renderHUD(Graphics2D g) {
        int w = getWidth();
        int h = getHeight();

        // 1. Title Bar or Pre-race countdown text overlay
        if (!raceStarted) {
            g.setColor(new Color(0, 0, 0, 160));
            g.fillRect(0, h / 2 - 50, w, 100);

            g.setFont(new Font("Monospaced", Font.BOLD, 36));
            String txt;
            if (countdownFrames > 120) {
                txt = "READY... 3";
                g.setColor(Color.RED);
            } else if (countdownFrames > 60) {
                txt = "READY... 2";
                g.setColor(Color.ORANGE);
            } else if (countdownFrames > 0) {
                txt = "READY... 1";
                g.setColor(Color.YELLOW);
            } else {
                txt = "START! FORÇA BARÇA!";
                g.setColor(Color.GREEN);
            }
            g.drawString(txt, w / 2 - g.getFontMetrics().stringWidth(txt) / 2, h / 2 + 10);
            return;
        }

        // 2. Main HUD Indicators
        g.setColor(new Color(0, 0, 0, 110));
        g.fillRoundRect(20, 20, 170, 130, 12, 12);
        g.setColor(Color.WHITE);
        g.setStroke(new BasicStroke(2f));
        g.drawRoundRect(20, 20, 170, 130, 12, 12);

        // Standing / Placement Position
        g.setFont(new Font("Monospaced", Font.BOLD, 22));
        int rank = calculatePlayerPosition();
        String posStr = switch (rank) {
            case 1 -> "1st PLACE";
            case 2 -> "2nd PLACE";
            case 3 -> "3rd PLACE";
            default -> "4th PLACE";
        };
        g.setColor(rank == 1 ? Color.YELLOW : Color.WHITE);
        g.drawString(posStr, 35, 52);

        // Laps & Speed
        g.setFont(new Font("Monospaced", Font.BOLD, 14));
        g.setColor(Color.WHITE);
        g.drawString("LAP: " + Math.min(3, player.lap) + " / 3", 35, 72);

        int displaySpeed = (int) (player.speed * 42.0);
        g.drawString("SPEED: " + displaySpeed + " KM/H", 35, 90);

        // Speedometer horizontal gauge bar
        int bx_speed = 35;
        int by_speed = 96;
        int bw_speed = 140;
        int bh_speed = 6;
        g.setColor(new Color(60, 60, 60));
        g.fillRect(bx_speed, by_speed, bw_speed, bh_speed);
        
        double speedRatio = Math.max(0.0, Math.min(1.0, player.speed / 6.4));
        int fillWidth = (int) (bw_speed * speedRatio);
        if (player.boostTimer > 0) {
            g.setColor(Color.CYAN);
        } else if (speedRatio > 0.8) {
            g.setColor(Color.RED);
        } else if (speedRatio > 0.5) {
            g.setColor(Color.ORANGE);
        } else {
            g.setColor(Color.GREEN);
        }
        g.fillRect(bx_speed, by_speed, fillWidth, bh_speed);

        // Render Real-time Speedrun lap timer
        double activeTime = (System.currentTimeMillis() - currentLapStartTime) / 1000.0;
        String curTimeStr = String.format("TIME: %.1fs", activeTime);
        g.drawString(curTimeStr, 35, 115);

        if (bestLapTime > 0) {
            String bestTimeStr = String.format("BEST: %.1fs", bestLapTime);
            g.setColor(Color.YELLOW);
            g.drawString(bestTimeStr, 35, 135);
        }

        // Draw direct blaster status indicator
        int bx = 20;
        int by = 160;
        int bw = 170;
        int bh = 15;
        g.setColor(new Color(0, 0, 0, 130));
        g.fillRoundRect(bx, by, bw, bh, 6, 6);
        g.setColor(Color.WHITE);
        g.drawRoundRect(bx, by, bw, bh, 6, 6);

        if (shootCooldown <= 0) {
            g.setColor(Color.GREEN);
            g.setFont(new Font("Monospaced", Font.BOLD, 10));
            g.drawString("BLASTER (F KEY): READY", bx + 12, by + 11);
        } else {
            double chargeRatio = 1.0 - (double) shootCooldown / 45.0;
            g.setColor(Color.ORANGE);
            g.fillRect(bx + 2, by + 2, (int) ((bw - 4) * chargeRatio), bh - 4);
            g.setColor(Color.WHITE);
            g.setFont(new Font("Monospaced", Font.BOLD, 10));
            g.drawString("RECHARGING BLASTER...", bx + 18, by + 11);
        }

        // 3. Current Item Holder Frame (top center box)
        g.setColor(new Color(0, 0, 0, 130));
        g.fillRoundRect(w / 2 - 40, 20, 80, 80, 10, 10);
        g.setColor(Color.WHITE);
        g.drawRoundRect(w / 2 - 40, 20, 80, 80, 10, 10);

        if (player.currentItem != null) {
            if ("SPINNING".equals(player.currentItem)) {
                // Flash spinning roulette items visually
                String[] rouletteItems = { "MUSHROOM", "BANANA", "RED_SHELL", "GREEN_SHELL" };
                String activeDummy = rouletteItems[(gameTicks / 3) % rouletteItems.length];
                drawItemIcon(g, w / 2, 60, 42, activeDummy);
            } else {
                // Draw high-quality vector item icon
                drawItemIcon(g, w / 2, 60, 42, player.currentItem);
            }
        } else {
            g.setFont(new Font("Monospaced", Font.BOLD, 24));
            g.setColor(Color.GRAY);
            g.drawString("?", w / 2 - g.getFontMetrics().stringWidth("?") / 2, 68);
        }

        // 4. Vector-Sharp Corner Minimap
        int mx = w - 150;
        int my = 20;
        int mw = 130;
        int mh = 130;

        g.setColor(new Color(0, 0, 0, 110));
        g.fillRoundRect(mx, my, mw, mh, 12, 12);
        g.setColor(Color.WHITE);
        g.drawRoundRect(mx, my, mw, mh, 12, 12);

        // Draw track loop on minimap
        g.setColor(new Color(110, 110, 110));
        g.setStroke(new BasicStroke(4f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND));
        double mapScale = 100.0 / 1024.0;
        for (int i = 0; i < TRACK_POINTS.length; i++) {
            int next = (i + 1) % TRACK_POINTS.length;
            int x1 = mx + 15 + (int) (TRACK_POINTS[i][0] * mapScale);
            int y1 = my + 15 + (int) (TRACK_POINTS[i][1] * mapScale);
            int x2 = mx + 15 + (int) (TRACK_POINTS[next][0] * mapScale);
            int y2 = my + 15 + (int) (TRACK_POINTS[next][1] * mapScale);
            g.drawLine(x1, y1, x2, y2);
        }

        // Draw Player dot (Blinking gold)
        if (gameTicks % 20 < 10) {
            g.setColor(Color.YELLOW);
        } else {
            g.setColor(Color.BLUE);
        }
        int mapPx = mx + 15 + (int) (player.x * mapScale);
        int mapPy = my + 15 + (int) (player.y * mapScale);
        g.fillOval(mapPx - 4, mapPy - 4, 8, 8);

        // Draw AI dots
        for (KartEntity opponent : opponents) {
            g.setColor(opponent.color);
            int mapOx = mx + 15 + (int) (opponent.x * mapScale);
            int mapOy = my + 15 + (int) (opponent.y * mapScale);
            g.fillOval(mapOx - 3, mapOy - 3, 6, 6);
        }

        // Blinking Final Lap banner
        if (player.lap == 3 && gameTicks % 40 < 25) {
            g.setFont(new Font("Monospaced", Font.BOLD, 28));
            g.setColor(Color.RED);
            String flStr = "FINAL LAP!";
            g.drawString(flStr, w / 2 - g.getFontMetrics().stringWidth(flStr) / 2, h / 2 - 120);
        }

        // 5. Game Over / Victory Overlay Panel
        if (gameOver) {
            g.setColor(new Color(0, 0, 0, 195));
            g.fillRect(0, 0, w, h);

            g.setFont(new Font("Monospaced", Font.BOLD, 35));
            String title = (player.lap >= 4 && calculatePlayerPosition() == 1) ? "GP CHAMPION! FORÇA BARÇA!" : "RACE FINISHED!";
            g.setColor(Color.YELLOW);
            g.drawString(title, w / 2 - g.getFontMetrics().stringWidth(title) / 2, h / 2 - 80);

            // Display Standings Podium
            g.setFont(new Font("Monospaced", Font.BOLD, 18));
            g.setColor(Color.WHITE);
            g.drawString("FINAL STANDINGS:", w / 2 - 100, h / 2 - 30);

            List<KartEntity> podiumList = new ArrayList<>();
            podiumList.add(player);
            podiumList.addAll(opponents);
            sortCompetitors(podiumList);

            for (int i = 0; i < podiumList.size(); i++) {
                KartEntity k = podiumList.get(i);
                String textLine = (i + 1) + ". " + k.name + " (Laps: " + Math.min(3, k.lap) + ")";
                g.setColor(i == 0 ? Color.YELLOW : (i == 1 ? Color.LIGHT_GRAY : (i == 2 ? new Color(184, 115, 51) : Color.GRAY)));
                g.drawString(textLine, w / 2 - 100, h / 2 + 10 + i * 25);
            }

            g.setFont(new Font("Monospaced", Font.BOLD, 15));
            g.setColor(Color.GREEN);
            String actionPrompt = "PRESS SPACE TO RACING AGAIN";
            g.drawString(actionPrompt, w / 2 - g.getFontMetrics().stringWidth(actionPrompt) / 2, h / 2 + 140);
        }
    }

    /**
     * Vector draws stunning retro-quality HUD item icons.
     */
    private void drawItemIcon(Graphics2D g, int cx, int cy, int size, String itemType) {
        Graphics2D g2 = (Graphics2D) g.create();
        g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        g2.translate(cx, cy);

        switch (itemType) {
            case "MUSHROOM" -> {
                // Draw stem
                g2.setColor(Color.WHITE);
                g2.fillRoundRect(-size / 6, 0, size / 3, size / 2, size / 10, size / 10);
                g2.setColor(Color.BLACK);
                g2.drawRoundRect(-size / 6, 0, size / 3, size / 2, size / 10, size / 10);
                // Eyes on stem
                g2.fillRect(-size / 15, size / 6, size / 30, size / 6);
                g2.fillRect(size / 15 - size / 30, size / 6, size / 30, size / 6);

                // Draw cap (red dome)
                g2.setColor(new Color(231, 76, 60)); // Crimson Red
                g2.fillArc(-size / 2, -size / 2, size, size, 0, 180);
                g2.setColor(Color.BLACK);
                g2.drawArc(-size / 2, -size / 2, size, size, 0, 180);
                g2.drawLine(-size / 2, 0, size / 2, 0);

                // White spots on cap
                g2.setColor(Color.WHITE);
                g2.fillOval(-size / 6, -size / 3, size / 4, size / 4);
                g2.fillOval(-size * 4 / 10, -size / 5, size / 5, size / 5);
                g2.fillOval(size * 2 / 10, -size / 5, size / 5, size / 5);
            }
            case "BANANA" -> {
                // Stylized banana peel
                g2.rotate(-0.1);
                g2.setColor(new Color(241, 196, 15)); // Banana Yellow
                // Left peel
                g2.fillArc(-size / 2, -size / 3, size * 2 / 3, size * 2 / 3, 45, 135);
                // Right peel
                g2.fillArc(-size / 6, -size / 3, size * 2 / 3, size * 2 / 3, 0, 135);
                // Center stem
                g2.setColor(new Color(139, 120, 0)); // Brown stem tip
                g2.fillRect(-size / 20, size / 4, size / 10, size / 6);
                g2.setColor(new Color(241, 196, 15));
                g2.fillOval(-size / 4, -size / 4, size / 2, size / 2);
            }
            case "RED_SHELL" -> {
                // Outer white rim
                g2.setColor(Color.WHITE);
                g2.fillOval(-size * 9 / 20, -size * 7 / 20, size * 9 / 10, size * 7 / 10);
                g2.setColor(Color.BLACK);
                g2.drawOval(-size * 9 / 20, -size * 7 / 20, size * 9 / 10, size * 7 / 10);

                // Inner red shell dome
                g2.setColor(new Color(231, 76, 60));
                g2.fillOval(-size / 3, -size * 3 / 10, size * 2 / 3, size * 3 / 5);
                g2.setColor(Color.BLACK);
                g2.drawOval(-size / 3, -size * 3 / 10, size * 2 / 3, size * 3 / 5);

                // Ridges
                g2.drawLine(-size / 3, 0, size / 3, 0);
                g2.drawLine(-size / 4, -size / 6, size / 4, -size / 6);
                g2.drawLine(-size / 4, size / 6, size / 4, size / 6);
            }
            case "GREEN_SHELL" -> {
                // Outer white rim
                g2.setColor(Color.WHITE);
                g2.fillOval(-size * 9 / 20, -size * 7 / 20, size * 9 / 10, size * 7 / 10);
                g2.setColor(Color.BLACK);
                g2.drawOval(-size * 9 / 20, -size * 7 / 20, size * 9 / 10, size * 7 / 10);

                // Inner green shell dome
                g2.setColor(new Color(46, 204, 113));
                g2.fillOval(-size / 3, -size * 3 / 10, size * 2 / 3, size * 3 / 5);
                g2.setColor(Color.BLACK);
                g2.drawOval(-size / 3, -size * 3 / 10, size * 2 / 3, size * 3 / 5);

                // Ridges
                g2.drawLine(-size / 3, 0, size / 3, 0);
                g2.drawLine(-size / 4, -size / 6, size / 4, -size / 6);
                g2.drawLine(-size / 4, size / 6, size / 4, size / 6);
            }
        }

        g2.dispose();
    }

    /**
     * Calculates the player's real-time position rank compared to other karts.
     */
    private int calculatePlayerPosition() {
        List<KartEntity> all = new ArrayList<>();
        all.add(player);
        all.addAll(opponents);
        sortCompetitors(all);
        return all.indexOf(player) + 1;
    }

    /**
     * Sorts the racing competitors based on lap counts and next waypoint progress.
     */
    private void sortCompetitors(List<KartEntity> list) {
        list.sort((k1, k2) -> {
            if (k1.lap != k2.lap) {
                return Integer.compare(k2.lap, k1.lap);
            }
            if (k1.nextCheckpoint != k2.nextCheckpoint) {
                return Integer.compare(k2.nextCheckpoint, k1.nextCheckpoint);
            }
            return Double.compare(k1.distToNext, k2.distToNext);
        });
    }

    /**
     * {@inheritDoc}
     * <p>Executes the primary high-frequency physics tick, collisions, and state transitions.</p>
     */
    @Override
    public void actionPerformed(ActionEvent e) {
        gameTicks++;

        if (!raceStarted) {
            countdownFrames--;
            if (countdownFrames <= 0) {
                raceStarted = true;
                raceStartTime = System.currentTimeMillis();
                currentLapStartTime = raceStartTime;
                if (engineChannel != null) {
                    engineChannel.noteOn(38, 50); // Start continuous engine note
                }
                playNoteAsync(60, 85, 100);
            } else if (countdownFrames % 60 == 0) {
                playNoteAsync(50, 85, 100); // countdown ticks
            }
            repaint();
            return;
        }

        if (gameOver) {
            if (engineChannel != null) {
                engineChannel.noteOff(38); // stop engine hum on game over
            }
            repaint();
            return;
        }

        if (shootCooldown > 0) {
            shootCooldown--;
        }

        // Smoothly adjust Field of View (fov) for high-speed camera stretch during boosts!
        if (player.boostTimer > 0) {
            fov = fov * 0.90 + 1.50 * 0.10;
        } else {
            fov = fov * 0.93 + 1.25 * 0.07;
        }

        // Update Engine Hum Pitch Bend
        if (engineChannel != null) {
            double speedRatio = Math.max(0.0, Math.min(1.0, player.speed / player.maxSpeed));
            int bend = 8192 + (int) (speedRatio * 3500);
            engineChannel.setPitchBend(bend);
        }

        // 1. Process Player Physics & Input
        updatePlayerPhysics();

        // Update World Particles
        List<Particle> deadParticles = new ArrayList<>();
        for (Particle p : particles) {
            p.x += p.dx;
            p.y += p.dy;
            p.life--;
            if (p.life <= 0) {
                deadParticles.add(p);
            }
        }
        particles.removeAll(deadParticles);

        // Spawn drift sparks if steering under speed
        if (player.isSteering && player.speed > 1.5 && gameTicks % 2 == 0) {
            double cos = Math.cos(player.angle);
            double sin = Math.sin(player.angle);
            double rx = player.x - cos * 15.0 + (Math.random() - 0.5) * 6.0;
            double ry = player.y - sin * 15.0 + (Math.random() - 0.5) * 6.0;
            
            // Sparks evolve as you drift longer (Yellow -> Orange -> Blue mini-turbo)
            Color sparkColor;
            if (player.driftTimer > 70) {
                sparkColor = Color.CYAN;
            } else if (player.driftTimer > 30) {
                sparkColor = Color.ORANGE;
            } else {
                sparkColor = Color.YELLOW;
            }
            particles.add(new Particle(rx, ry, -cos * 1.5 + (Math.random() - 0.5) * 0.8, -sin * 1.5 + (Math.random() - 0.5) * 0.8, sparkColor, 12 + (int) (Math.random() * 8)));
        }

        // Spawn boost trail particles
        if (player.boostTimer > 0 && gameTicks % 2 == 0) {
            double cos = Math.cos(player.angle);
            double sin = Math.sin(player.angle);
            double rx = player.x - cos * 15.0;
            double ry = player.y - sin * 15.0;
            particles.add(new Particle(rx, ry, -cos * 1.0, -sin * 1.0, Color.CYAN, 10 + (int) (Math.random() * 6)));
        }

        // Spawn turf/grass trail particles when driving off-road (for all karts!)
        List<KartEntity> allKarts = new ArrayList<>();
        allKarts.add(player);
        allKarts.addAll(opponents);
        for (KartEntity kart : allKarts) {
            int rgb = trackTexture.getRGB(((int) kart.x) & 1023, ((int) kart.y) & 1023);
            Color terrainColor = new Color(rgb);
            boolean offRoad = (terrainColor.getGreen() > 100 && terrainColor.getRed() < 60);

            if (offRoad && Math.abs(kart.speed) > 0.5 && gameTicks % 3 == 0) {
                double cos = Math.cos(kart.angle);
                double sin = Math.sin(kart.angle);
                double rx = kart.x - cos * 12.0 + (Math.random() - 0.5) * 4.0;
                double ry = kart.y - sin * 12.0 + (Math.random() - 0.5) * 4.0;
                particles.add(new Particle(rx, ry, -cos * 0.8 + (Math.random() - 0.5) * 0.3, -sin * 0.8 + (Math.random() - 0.5) * 0.3, new Color(46, 139, 46), 8 + (int)(Math.random() * 5)));
            }
        }

        // 2. Process AI Competitors
        for (KartEntity ai : opponents) {
            updateAICompetitor(ai);
        }

        // 3. Update active item box spawners cooldowns
        for (ItemBox box : itemBoxes) {
            if (!box.active) {
                box.cooldown--;
                if (box.cooldown <= 0) {
                    box.active = true;
                }
            }
        }

        // Update track mushrooms cooldowns
        for (TrackMushroom shroom : trackMushrooms) {
            if (!shroom.active) {
                shroom.cooldown--;
                if (shroom.cooldown <= 0) {
                    shroom.active = true;
                }
            }
        }

        // 4. Update traveling red shells
        updateHomingShells();

        // 5. Evaluate overlapping bounding collisions
        checkBumpingCollisions();
        checkItemOverlaps();

        // 6. Check Race Completion Finish conditions
        if (player.lap >= 4) {
            gameOver = true;
            if (calculatePlayerPosition() == 1) {
                playBarsaAnthem();
            } else {
                playLapCompletedSound();
            }
        }

        repaint();
    }

    /**
     * Updates the player's kart speed, angles, drift sliding, and coordinate bounds.
     */
    private void updatePlayerPhysics() {
        if (player.oilCooldown > 0) {
            player.oilCooldown--;
        }

        if (player.spinTimer > 0) {
            player.spinTimer--;
            player.speed = player.speed * 0.93;
            // Complete sliding rotation
            player.angle += 0.12;
        } else {
            // Check off-road track surface penalty
            int rgb = trackTexture.getRGB(((int) player.x) & 1023, ((int) player.y) & 1023);
            Color terrainColor = new Color(rgb);
            boolean offRoad = (terrainColor.getGreen() > 100 && terrainColor.getRed() < 60);

            double targetMaxSpeed = offRoad ? 1.5 : player.maxSpeed;
            if (player.boostTimer > 0) {
                player.boostTimer--;
                targetMaxSpeed = 6.4; // Boost speed limit override
            }

            // Keyboard Acceleration & Braking Drag
            if (keyUp) {
                if (player.boostTimer > 0) {
                    player.speed = 6.4;
                } else {
                    player.speed += 0.12;
                }
            } else if (keyDown) {
                player.speed -= 0.15;
            } else {
                player.speed *= 0.95; // Frictional deceleration
            }

            // Apply terminal velocity speed clamps
            if (player.speed > targetMaxSpeed) {
                player.speed = Math.max(targetMaxSpeed, player.speed - 0.1);
            }
            if (player.speed < -1.5) {
                player.speed = -1.5;
            }

            // Steering Angle manipulation
            if (Math.abs(player.speed) > 1.5 && (keyLeft || keyRight)) {
                player.driftTimer++;
                player.isSteering = true;
                double steerFactor = (player.speed > 0) ? 0.045 : -0.045;
                if (keyLeft) {
                    player.angle -= steerFactor;
                    player.steerLeft = true;
                    player.steerRight = false;
                } else {
                    player.angle += steerFactor;
                    player.steerLeft = false;
                    player.steerRight = true;
                }
            } else {
                // Mini-Turbo Drift Boost release!
                if (player.driftTimer > 70) {
                    player.boostTimer = 40; // Mini-Turbo active!
                    playBoostSound();
                    
                    // Spawn Mini-turbo flash particles
                    for (int i = 0; i < 12; i++) {
                        double pAngle = Math.random() * Math.PI * 2;
                        particles.add(new Particle(player.x, player.y, Math.cos(pAngle) * 3.0, Math.sin(pAngle) * 3.0, Color.CYAN, 12));
                    }
                }
                player.driftTimer = 0;
                player.isSteering = false;
                player.steerLeft = false;
                player.steerRight = false;
            }

            // Drift slide vector handling (Responsive chasing mechanics)
            if (player.isSteering && player.speed > 2.0) {
                player.velocityAngle = player.velocityAngle * 0.91 + player.angle * 0.09;
            } else {
                player.velocityAngle = player.angle;
            }
        }

        // Apply physical coordinate transformations
        player.x += player.speed * Math.cos(player.velocityAngle);
        player.y += player.speed * Math.sin(player.velocityAngle);

        // Keep karts strictly bound inside the 1024x1024 track arena
        player.x = Math.max(0, Math.min(1023, player.x));
        player.y = Math.max(0, Math.min(1023, player.y));

        // Evaluate checkpoint waypoint updates
        updateKartWaypoint(player);

        // Update player item selection roulette timer
        if (player.currentItem != null && "SPINNING".equals(player.currentItem)) {
            player.rouletteTimer--;
            if (player.rouletteTimer % 5 == 0) {
                playNoteAsync(72 + (player.rouletteTimer / 5) % 6, 75, 40); // Tick SFX
            }
            if (player.rouletteTimer <= 0) {
                String[] selection = { "MUSHROOM", "BANANA", "RED_SHELL", "GREEN_SHELL" };
                player.currentItem = selection[(int) (Math.random() * selection.length)];
                playNoteAsync(76, 95, 180);
            }
        }
    }

    /**
     * Updates an AI competitor's intelligence, steering vectors, and waypoint progress.
     */
    private void updateAICompetitor(KartEntity ai) {
        if (ai.oilCooldown > 0) {
            ai.oilCooldown--;
        }

        if (ai.spinTimer > 0) {
            ai.spinTimer--;
            ai.speed *= 0.92;
            ai.angle += 0.15;
            ai.x += ai.speed * Math.cos(ai.angle);
            ai.y += ai.speed * Math.sin(ai.angle);
            ai.x = Math.max(0, Math.min(1023, ai.x));
            ai.y = Math.max(0, Math.min(1023, ai.y));
            return;
        }

        // Track off-road terrain penalty
        int rgb = trackTexture.getRGB(((int) ai.x) & 1023, ((int) ai.y) & 1023);
        Color terrainColor = new Color(rgb);
        boolean offRoad = (terrainColor.getGreen() > 100 && terrainColor.getRed() < 60);

        double targetMaxSpeed = offRoad ? 1.4 : ai.maxSpeed;
        if (ai.boostTimer > 0) {
            ai.boostTimer--;
            targetMaxSpeed = 5.8;
        }

        // Calculate steering angle towards target checkpoint
        double[] targetCp = TRACK_POINTS[ai.nextCheckpoint];
        double targetAngle = Math.atan2(targetCp[1] - ai.y, targetCp[0] - ai.x);

        // Wrap difference between target angle and current angle within [-PI, PI]
        double angleDiff = targetAngle - ai.angle;
        while (angleDiff < -Math.PI) {
            angleDiff += Math.PI * 2.0;
        }
        while (angleDiff > Math.PI) {
            angleDiff -= Math.PI * 2.0;
        }

        // Adjust heading slowly
        double maxSteer = 0.045;
        if (angleDiff < -maxSteer) {
            ai.angle -= maxSteer;
        } else if (angleDiff > maxSteer) {
            ai.angle += maxSteer;
        } else {
            ai.angle = targetAngle;
        }

        // Drive forward
        ai.speed += 0.11;
        if (ai.speed > targetMaxSpeed) {
            ai.speed = targetMaxSpeed;
        }

        ai.x += ai.speed * Math.cos(ai.angle);
        ai.y += ai.speed * Math.sin(ai.angle);

        // Arena boundary clamping
        ai.x = Math.max(0, Math.min(1023, ai.x));
        ai.y = Math.max(0, Math.min(1023, ai.y));

        updateKartWaypoint(ai);

        // Intelligent AI item usage
        if (ai.currentItem != null) {
            ai.rouletteTimer--;
            if (ai.rouletteTimer <= 0) {
                triggerAIItem(ai);
            }
        }
    }

    /**
     * Executes AI item activation decisions based on current item held.
     */
    private void triggerAIItem(KartEntity ai) {
        if (ai.currentItem == null) {
            return;
        }

        switch (ai.currentItem) {
            case "MUSHROOM" -> {
                ai.boostTimer = 80;
                playBoostSound();
            }
            case "BANANA" -> {
                double cos = Math.cos(ai.angle);
                double sin = Math.sin(ai.angle);
                bananas.add(new PlacedBanana(ai.x - cos * 35.0, ai.y - sin * 35.0));
                playNoteAsync(52, 70, 100);
            }
            case "RED_SHELL" -> {
                shells.add(new ThrownShell(ai.x, ai.y, ai.nextCheckpoint, ai.name, true, ai.angle));
                playNoteAsync(64, 70, 150);
            }
            case "GREEN_SHELL" -> {
                shells.add(new ThrownShell(ai.x, ai.y, ai.nextCheckpoint, ai.name, false, ai.angle));
                playNoteAsync(62, 70, 150);
            }
        }

        ai.currentItem = null;
    }

    /**
     * Updates waypoints checklist and lap completions for karts.
     */
    private void updateKartWaypoint(KartEntity kart) {
        double[] cp = TRACK_POINTS[kart.nextCheckpoint];
        double dx = kart.x - cp[0];
        double dy = kart.y - cp[1];
        double dist = Math.sqrt(dx * dx + dy * dy);
        kart.distToNext = dist;

        // Waypoint check-radius (within track width)
        if (dist < 92.0) {
            kart.nextCheckpoint++;
            if (kart.nextCheckpoint >= TRACK_POINTS.length) {
                kart.nextCheckpoint = 0;
                kart.lap++;
                if ("Player".equals(kart.name)) {
                    long now = System.currentTimeMillis();
                    double currentTime = (now - currentLapStartTime) / 1000.0;
                    lastLapTime = currentTime;
                    if (bestLapTime < 0 || currentTime < bestLapTime) {
                        bestLapTime = currentTime;
                    }
                    currentLapStartTime = now;
                    
                    if (player.lap == 3 && !finalLapWarningPlayed) {
                        finalLapWarningPlayed = true;
                        playFinalLapSound();
                    } else {
                        playLapCompletedSound();
                    }
                }
            }
        }
    }

    /**
     * Updates active traveling red shells on track checkpoints towards targets.
     */
    private void updateHomingShells() {
        List<ThrownShell> deadShells = new ArrayList<>();

        for (ThrownShell shell : shells) {
            shell.life--;
            if (shell.life <= 0) {
                deadShells.add(shell);
                continue;
            }

            if (shell.homing) {
                double[] targetCp = TRACK_POINTS[shell.targetCheckpoint];
                double dx = targetCp[0] - shell.x;
                double dy = targetCp[1] - shell.y;
                double dist = Math.sqrt(dx * dx + dy * dy);

                if (dist < 40.0) {
                    // Aim towards next checkpoint in sequence
                    shell.targetCheckpoint = (shell.targetCheckpoint + 1) % TRACK_POINTS.length;
                }

                // Move towards checkpoint target
                double angle = Math.atan2(dy, dx);
                shell.x += shell.speed * Math.cos(angle);
                shell.y += shell.speed * Math.sin(angle);
            } else {
                // Green Shell moves straight
                shell.x += shell.dx;
                shell.y += shell.dy;
            }

            // Spawn rocket smoke particles
            if (gameTicks % 2 == 0) {
                Color pColor = shell.homing ? Color.RED : Color.GREEN;
                particles.add(new Particle(shell.x, shell.y, (Math.random() - 0.5) * 0.5, (Math.random() - 0.5) * 0.5, pColor, 8 + (int)(Math.random() * 6)));
            }

            // Bounding Arena checks
            shell.x = Math.max(0, Math.min(1023, shell.x));
            shell.y = Math.max(0, Math.min(1023, shell.y));

            // Evaluate impact hitting competitors
            List<KartEntity> all = new ArrayList<>();
            all.add(player);
            all.addAll(opponents);

            for (KartEntity kart : all) {
                // Prevent owner self-hitting on initial 25 frames launch
                if (kart.name.equals(shell.owner) && shell.life > 275) {
                    continue;
                }

                double kdx = kart.x - shell.x;
                double kdy = kart.y - shell.y;
                double kdist = Math.sqrt(kdx * kdx + kdy * kdy);

                if (kdist < 18.0) {
                    // Impact!
                    kart.spinTimer = 50; // Spinout frames
                    kart.speed = 0;
                    playCrashSound();
                    deadShells.add(shell);
                    break;
                }
            }
        }

        shells.removeAll(deadShells);
    }

    /**
     * Calculates bumping/colliding bouncing interactions between karts.
     */
    private void checkBumpingCollisions() {
        List<KartEntity> all = new ArrayList<>();
        all.add(player);
        all.addAll(opponents);

        for (int i = 0; i < all.size(); i++) {
            for (int j = i + 1; j < all.size(); j++) {
                KartEntity k1 = all.get(i);
                KartEntity k2 = all.get(j);

                double dx = k2.x - k1.x;
                double dy = k2.y - k1.y;
                double dist = Math.sqrt(dx * dx + dy * dy);

                // Check collision intersection (kart width bounds)
                if (dist < 18.0) {
                    double angle = Math.atan2(dy, dx);
                    double overlap = 18.0 - dist;

                    // Relocate overlapping elements
                    k1.x -= Math.cos(angle) * overlap * 0.5;
                    k1.y -= Math.sin(angle) * overlap * 0.5;
                    k2.x += Math.cos(angle) * overlap * 0.5;
                    k2.y += Math.sin(angle) * overlap * 0.5;

                    // Damp speeds slightly on physical bumper hit
                    double sTemp = k1.speed;
                    k1.speed = k2.speed * 0.75;
                    k2.speed = sTemp * 0.75;

                    playNoteAsync(48, 80, 80);
                }
            }
        }
    }

    /**
     * Evaluates item pickup boxes and obstacle collisions on the track.
     */
    private void checkItemOverlaps() {
        List<KartEntity> karts = new ArrayList<>();
        karts.add(player);
        karts.addAll(opponents);

        // 1. Evaluate Item boxes pick ups
        for (KartEntity kart : karts) {
            if (kart.currentItem != null) {
                continue;
            }

            for (ItemBox box : itemBoxes) {
                if (!box.active) {
                    continue;
                }

                double dx = kart.x - box.x;
                double dy = kart.y - box.y;
                double dist = Math.sqrt(dx * dx + dy * dy);

                if (dist < 45.0) {
                    box.active = false;
                    box.cooldown = 300; // 5 seconds respawn cooldown

                    if ("Player".equals(kart.name)) {
                        kart.currentItem = "SPINNING";
                        kart.rouletteTimer = 55; // Spinning animation ticks
                        playItemPickupSound();
                    } else {
                        // AI immediately gets a random item
                        String[] selection = { "MUSHROOM", "BANANA", "RED_SHELL", "GREEN_SHELL" };
                        kart.currentItem = selection[(int) (Math.random() * selection.length)];
                        kart.rouletteTimer = 40 + (int) (Math.random() * 80); // AI delays item trigger
                    }
                }
            }
        }

        // 2. Evaluate Banana peel slips
        List<PlacedBanana> hitBananas = new ArrayList<>();
        for (PlacedBanana banana : bananas) {
            for (KartEntity kart : karts) {
                double dx = kart.x - banana.x;
                double dy = kart.y - banana.y;
                double dist = Math.sqrt(dx * dx + dy * dy);

                if (dist < 14.0) {
                    kart.spinTimer = 50;
                    kart.speed = 0;
                    playCrashSound();
                    hitBananas.add(banana);
                    break;
                }
            }
        }
        bananas.removeAll(hitBananas);

        // 3. Evaluate Track Mushrooms collection
        for (KartEntity kart : karts) {
            for (TrackMushroom shroom : trackMushrooms) {
                if (!shroom.active) {
                    continue;
                }

                double dx = kart.x - shroom.x;
                double dy = kart.y - shroom.y;
                double dist = Math.sqrt(dx * dx + dy * dy);

                if (dist < 40.0) {
                    shroom.active = false;
                    shroom.cooldown = 420; // 7 seconds respawn cooldown
                    kart.boostTimer = 100; // Boost!
                    playBoostSound();
                    
                    // Spawn Cyan boost particles
                    for (int i = 0; i < 15; i++) {
                        double pAngle = Math.random() * Math.PI * 2;
                        double pSpeed = 1.0 + Math.random() * 2.0;
                        particles.add(new Particle(kart.x, kart.y, Math.cos(pAngle) * pSpeed, Math.sin(pAngle) * pSpeed, Color.CYAN, 15 + (int) (Math.random() * 10)));
                    }
                }
            }
        }

        // 4. Evaluate Oil Slick slips
        for (double[] slick : OIL_SLICKS) {
            for (KartEntity kart : karts) {
                double dx = kart.x - slick[0];
                double dy = kart.y - slick[1];
                double dist = Math.sqrt(dx * dx + dy * dy);

                if (dist < 16.0 && kart.spinTimer <= 0 && kart.oilCooldown <= 0) {
                    kart.spinTimer = 45;
                    kart.oilCooldown = 120; // 2 seconds of slip immunity to escape the slick!
                    kart.speed = 0;
                    playCrashSound();
                    screenShake = 8;
                    // Spawn dark splash particles
                    for (int i = 0; i < 8; i++) {
                        double pAngle = Math.random() * Math.PI * 2;
                        particles.add(new Particle(kart.x, kart.y, Math.cos(pAngle) * 1.5, Math.sin(pAngle) * 1.5, Color.BLACK, 10));
                    }
                    break;
                }
            }
        }
    }

    /**
     * Standalone executable entry-point.
     */
    public static void main(String[] args) {
        SwingUtilities.invokeLater(() -> {
            JFrame frame = new JFrame("Anahata AgiKart (Mario Kart Clone)");
            frame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);

            // Set beautiful retro custom window icon
            try {
                AgiKartIcon gameIcon = new AgiKartIcon(32);
                BufferedImage iconImg = new BufferedImage(32, 32, BufferedImage.TYPE_INT_ARGB);
                Graphics2D gIcon = iconImg.createGraphics();
                gameIcon.paintIcon(null, gIcon, 0, 0);
                gIcon.dispose();
                frame.setIconImage(iconImg);
            } catch (Exception e) {
                // Fallback silently if icon drawing fails
            }

            AgiKart game = new AgiKart();
            frame.add(game);
            frame.pack();
            frame.setLocationRelativeTo(null);
            frame.setResizable(true);

            frame.addWindowListener(new java.awt.event.WindowAdapter() {
                @Override
                public void windowClosing(java.awt.event.WindowEvent e) {
                    game.cleanup();
                }
            });

            frame.setVisible(true);
        });
    }

    // Helper World Entities Inner Classes
    /**
     * DTO containing projected coordinates and scales for drawing billboarded entities.
     */
    private static class SpriteProjection {
        double screenX, screenY, scale, depth;
        SpriteDrawable drawable;

        SpriteProjection(double sx, double sy, double sc, double dp) {
            this.screenX = sx;
            this.screenY = sy;
            this.scale = sc;
            this.depth = dp;
        }
    }

    /**
     * Functional Interface defining custom sprite-drawing instructions.
     */
    @FunctionalInterface
    private interface SpriteDrawable {
        void draw(Graphics2D g, double sx, double sy, double scale);
    }

    /**
     * Data object model for tracking karts.
     */
    private static class KartEntity {
        String name;
        double x, y;
        double angle = 0;
        double velocityAngle = 0;
        double speed = 0;
        final double maxSpeed = 3.8;
        Color color;

        // Realtime checkpoint variables
        int nextCheckpoint = 0;
        int lap = 1;
        double distToNext = 0;

        // Controls indicators
        boolean isSteering = false;
        boolean steerLeft = false;
        boolean steerRight = false;

        // Items & States
        String currentItem = null;
        int rouletteTimer = 0;
        int spinTimer = 0;
        int boostTimer = 0;
        int driftTimer = 0;
        int oilCooldown = 0;

        KartEntity(String name, double x, double y, Color color) {
            this.name = name;
            this.x = x;
            this.y = y;
            this.color = color;
        }
    }

    /**
     * Data object model for placing Pine Trees.
     */
    private static class PineTree {
        double x, y;

        PineTree(double x, double y) {
            this.x = x;
            this.y = y;
        }
    }

    /**
     * Data object model for spawning item boxes.
     */
    private static class ItemBox {
        double x, y;
        boolean active = true;
        int cooldown = 0;

        ItemBox(double x, double y) {
            this.x = x;
            this.y = y;
        }
    }

    /**
     * Data object model for banana slip obstacles.
     */
    private static class PlacedBanana {
        double x, y;

        PlacedBanana(double x, double y) {
            this.x = x;
            this.y = y;
        }
    }

    /**
     * Data object model for traveling tracking shells.
     */
    private static class ThrownShell {
        double x, y;
        double speed = 7.4;
        double dx, dy; // Direction vector for straight shells
        int targetCheckpoint;
        String owner;
        int life = 300; // 5 seconds live lifespan clamp
        boolean homing;

        ThrownShell(double x, double y, int startCheckpoint, String owner, boolean homing, double angle) {
            this.x = x;
            this.y = y;
            this.targetCheckpoint = startCheckpoint;
            this.owner = owner;
            this.homing = homing;
            this.dx = Math.cos(angle) * 8.5;
            this.dy = Math.sin(angle) * 8.5;
        }
    }

    /**
     * World particle model for drift sparks and boost trails.
     */
    private static class Particle {
        double x, y;
        double dx, dy;
        Color color;
        int life;

        Particle(double x, double y, double dx, double dy, Color color, int life) {
            this.x = x;
            this.y = y;
            this.dx = dx;
            this.dy = dy;
            this.color = color;
            this.life = life;
        }
    }

    /**
     * Data object model for spawning collectible mushrooms on the track.
     */
    private static class TrackMushroom {
        double x, y;
        boolean active = true;
        int cooldown = 0;

        TrackMushroom(double x, double y) {
            this.x = x;
            this.y = y;
        }
    }

    /**
     * Plays an 8-bit retro melodic fanfare of the classic Barça Anthem.
     */
    private void playBarsaAnthem() {
        soundPool.submit(() -> {
            try {
                // Notes for "Tot el camp, és un clam, som la gent blaugrana..."
                int[] notes = {
                    67, 72, 72, 72, 71, 72, 74, 67, // Tot el camp, és un clam
                    74, 76, 76, 76, 74, 76, 77, 72, // Som la gent blaugrana
                    79, 79, 79, 77, 76, 74, 72, 74, 76, // Tant se val d'on venim
                    74, 74, 74, 72, 71, 69, 67, 69, 71, 72 // Si del sud o del nord...
                };
                int[] durations = {
                    200, 200, 200, 300, 100, 200, 400, 400,
                    200, 200, 200, 300, 100, 200, 400, 400,
                    150, 150, 300, 200, 200, 200, 200, 200, 200,
                    150, 150, 300, 200, 200, 200, 200, 200, 200, 400
                };
                
                for (int i = 0; i < notes.length; i++) {
                    if (midiChannel != null) {
                        midiChannel.noteOn(notes[i], 90);
                    }
                    Thread.sleep(durations[i]);
                    if (midiChannel != null) {
                        midiChannel.noteOff(notes[i]);
                    }
                    Thread.sleep(40); // small gap
                }
            } catch (Exception ex) {
                // Ignore
            }
        });
    }
}
