/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.games;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.GradientPaint;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.HierarchyEvent;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.geom.Ellipse2D;
import java.awt.geom.Line2D;
import java.awt.geom.Path2D;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.awt.geom.RoundRectangle2D;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.Random;
import javax.sound.midi.MidiChannel;
import javax.sound.midi.MidiSystem;
import javax.sound.midi.Synthesizer;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;
import javax.swing.Timer;

/**
 * An ultimate Barça-themed Arkanoid (Breakout) clone implemented as a Swing game.
 * <p>
 * This implementation features a professional physics engine for ball-paddle collisions,
 * responsive keyboard and mouse controls, MIDI sound synthesis for 8-bit retro audio, particle
 * systems, and multiple power-up items (multiball, wider paddle, and fireball).
 * </p>
 * <p>
 * It integrates a {@link java.awt.event.HierarchyListener} to automatically clean up resources
 * (stopping timers, closing synthesizers) when the panel is closed or removed from the screen.
 * </p>
 *
 * @author anahata
 */
public class Arkanoid extends JPanel implements ActionListener {

    /**
     * The width of the game arena in pixels.
     */
    private final int width = 800;

    /**
     * The height of the game arena in pixels.
     */
    private final int height = 600;

    /**
     * The fixed Y-coordinate position of the paddle.
     */
    private final double paddleY = 530;

    /**
     * The height of the paddle in pixels.
     */
    private final double paddleHeight = 15;

    /**
     * Game state: Main menu screen.
     */
    private final int stateMenu = 0;

    /**
     * Game state: Currently playing.
     */
    private final int statePlaying = 1;

    /**
     * Game state: Paused.
     */
    private final int statePaused = 2;

    /**
     * Game state: Game over / lost.
     */
    private final int stateGameover = 3;

    /**
     * Game state: Victory / level cleared.
     */
    private final int stateVictory = 4;

    /**
     * The active game state.
     */
    private int state = stateMenu;

    /**
     * The player's current score (Gols).
     */
    private int score = 0;

    /**
     * The player's remaining lives.
     */
    private int lives = 3;

    /**
     * The current game difficulty level.
     */
    private int level = 1;

    /**
     * The actual X-coordinate of the paddle's left edge.
     */
    private double paddleX = 340;

    /**
     * The target X-coordinate of the paddle's left edge for smooth interpolation.
     */
    private double targetPaddleX = 340;

    /**
     * The current width of the paddle, which changes with power-ups.
     */
    private double paddleWidth = 120;

    /**
     * Active balls in the game field.
     */
    private final ArrayList<Ball> balls = new ArrayList<>();

    /**
     * Active bricks in the level.
     */
    private final ArrayList<Brick> bricks = new ArrayList<>();

    /**
     * Active dropping power-up items.
     */
    private final ArrayList<PowerUp> powerUps = new ArrayList<>();

    /**
     * Visual particle effects.
     */
    private final ArrayList<Particle> particles = new ArrayList<>();

    /**
     * Timer for wide-paddle power-up duration.
     */
    private int widePaddleTimer = 0;

    /**
     * Timer for fireball power-up duration.
     */
    private int fireballTimer = 0;

    /**
     * Flag indicating if the fireball power-up is active.
     */
    private boolean fireballActive = false;

    /**
     * MIDI Synthesizer used for retro sound generation.
     */
    private Synthesizer synth;

    /**
     * MIDI channel dedicated to game sound effects.
     */
    private MidiChannel channel;

    /**
     * Flag indicating if MIDI system has been initialized.
     */
    private boolean synthInitialized = false;

    /**
     * The high-frequency game loop timer.
     */
    private Timer timer;

    /**
     * Constructs a new Arkanoid game panel and registers listeners.
     */
    public Arkanoid() {
        setPreferredSize(new Dimension(width, height));
        setFocusable(true);

        // Key interactions
        addKeyListener(new KeyAdapter() {
            /**
             * {@inheritDoc}
             * <p>Propagates the key event to the internal handler method.</p>
             */
            @Override
            public void keyPressed(KeyEvent e) {
                handleKeyPress(e);
            }
        });

        // Mouse interactions
        MouseAdapter mouseHandler = new MouseAdapter() {
            /**
             * {@inheritDoc}
             * <p>Tracks mouse movement to update the paddle's target coordinate.</p>
             */
            @Override
            public void mouseMoved(MouseEvent e) {
                if (state == statePlaying) {
                    targetPaddleX = e.getX() - paddleWidth / 2.0;
                }
            }

            /**
             * {@inheritDoc}
             * <p>Tracks mouse dragging to update the paddle's target coordinate.</p>
             */
            @Override
            public void mouseDragged(MouseEvent e) {
                if (state == statePlaying) {
                    targetPaddleX = e.getX() - paddleWidth / 2.0;
                }
            }

            /**
             * {@inheritDoc}
             * <p>Requests focus, starts playing, launches balls, or resets game states.</p>
             */
            @Override
            public void mousePressed(MouseEvent e) {
                requestFocusInWindow();
                if (state == stateMenu) {
                    state = statePlaying;
                    playSound(72, 100);
                } else if (state == statePlaying) {
                    launchBalls();
                } else if (state == stateGameover || state == stateVictory) {
                    resetFullGame();
                }
            }
        };

        addMouseListener(mouseHandler);
        addMouseMotionListener(mouseHandler);

        // Auto-cleanup on component displayability change (closed or removed)
        addHierarchyListener(e -> {
            if ((e.getChangeFlags() & HierarchyEvent.DISPLAYABILITY_CHANGED) != 0) {
                if (!isDisplayable()) {
                    cleanup();
                }
            }
        });

        initSynth();
        initLevel();

        // Game loop: ~60 FPS
        timer = new Timer(16, this);
        timer.start();
    }

    /**
     * Initializes the MIDI sound system asynchronously.
     * <p>
     * Synthesizer is acquired from MidiSystem and loaded with lead Square Lead program.
     * Runs on an autonomous thread to avoid blocking the Event Dispatch Thread during startup.
     * </p>
     */
    private void initSynth() {
        if (synthInitialized) {
            return;
        }
        new Thread(() -> {
            try {
                synth = MidiSystem.getSynthesizer();
                if (synth != null) {
                    synth.open();
                    channel = synth.getChannels()[0];
                    channel.programChange(0, 80); // Square lead instrument
                }
            } catch (Exception e) {
                System.err.println("MIDI synth failed: " + e.getMessage());
            } finally {
                synthInitialized = true;
            }
        }).start();
    }

    /**
     * Plays an 8-bit sound note on a separate thread.
     * <p>
     * Runs asynchronously using an individual short-lived thread to preserve
     * fluid game physics execution and UI performance.
     * </p>
     *
     * @param note     The MIDI note to play (0-127).
     * @param duration The duration in milliseconds.
     */
    private void playSound(int note, int duration) {
        if (channel != null) {
            new Thread(() -> {
                try {
                    channel.noteOn(note, 90);
                    Thread.sleep(duration);
                    channel.noteOff(note);
                } catch (Exception e) {
                    // Fail fast internally
                }
            }).start();
        }
    }

    /**
     * Plays a sequence melody asynchronously.
     * <p>
     * Plays multiple MIDI note events sequentially separated by duration delays.
     * </p>
     *
     * @param notes     Array of MIDI notes.
     * @param delays    Array of delays between notes.
     * @param durations Array of note play durations.
     */
    private void playMelody(int[] notes, int[] delays, int[] durations) {
        if (channel == null) {
            return;
        }
        new Thread(() -> {
            try {
                for (int i = 0; i < notes.length; i++) {
                    channel.noteOn(notes[i], 100);
                    Thread.sleep(durations[i]);
                    channel.noteOff(notes[i]);
                    if (delays[i] > 0) {
                        Thread.sleep(delays[i]);
                    }
                }
            } catch (Exception e) {
                // Fail fast internally
            }
        }).start();
    }

    /**
     * Initializes the brick and ball layouts for the current level.
     * <p>
     * Arranges the game bricks to represent a beautiful FCB layout:
     * - The "F" and "B" letters are colored in deep Grana (Type 2), requiring 2 hits.
     * - The "C" letter is colored in rich Azul/Blue (Type 3), requiring 3 hits.
     * - The empty background space is filled with warm Light Yellow bricks (Type 1), requiring 1 hit.
     * </p>
     */
    private void initLevel() {
        balls.clear();
        powerUps.clear();
        particles.clear();
        widePaddleTimer = 0;
        fireballTimer = 0;
        fireballActive = false;
        paddleWidth = 120;

        // Add launching ball resting on paddle
        Ball b = new Ball(width / 2.0, paddleY - 8, 0, 0);
        b.onPaddle = true;
        balls.add(b);

        // Brick Layout ( Azul, Grana, Gold )
        // Let's create an "FCB" layout representing Barcelona glory!
        bricks.clear();
        int[][] layout = {
            {3,3,3,3, 1, 3,3,3,3, 1, 3,3,3,1},
            {3,1,1,1, 1, 3,1,1,1, 1, 3,1,1,3},
            {3,3,3,1, 1, 3,1,1,1, 1, 3,3,3,1},
            {3,1,1,1, 1, 3,1,1,1, 1, 3,1,1,3},
            {3,1,1,1, 1, 3,3,3,3, 1, 3,3,3,1}
        };

        int rowsCount = layout.length;
        int colsCount = layout[0].length;
        int brickW = 46;
        int brickH = 20;
        int gap = 4;
        int startX = (width - (colsCount * (brickW + gap) - gap)) / 2;
        int startY = 80;

        for (int r = 0; r < rowsCount; r++) {
            for (int c = 0; c < colsCount; c++) {
                int originalVal = layout[r][c];
                int type;
                if (originalVal == 1) {
                    type = 1; // Light Yellow background
                } else {
                    // Part of a letter: "F" is Grana, "C" is Blue, "B" is Grana
                    if (c >= 0 && c <= 3) {
                        type = 2; // "F" -> Grana
                    } else if (c >= 5 && c <= 8) {
                        type = 3; // "C" -> Blue
                    } else {
                        type = 2; // "B" -> Grana
                    }
                }
                int x = startX + c * (brickW + gap);
                int y = startY + r * (brickH + gap);
                // Type 3 is Blue (3 hits), Type 2 is Grana (2 hits), Type 1 is Light Yellow (1 hit)
                int hits = (type == 3) ? 3 : (type == 2 ? 2 : 1);
                bricks.add(new Brick(x, y, brickW, brickH, type, hits));
            }
        }
    }

    /**
     * Resets score, lives, levels, and initializes the first stage.
     */
    private void resetFullGame() {
        score = 0;
        lives = 3;
        level = 1;
        initLevel();
        state = statePlaying;
    }

    /**
     * Cleans up running loops, timers, and MIDI synthesizer resources to prevent memory leaks.
     */
    public void cleanup() {
        if (timer != null) {
            timer.stop();
        }
        if (synth != null && synth.isOpen()) {
            synth.close();
            synth = null;
        }
    }

    /**
     * Handles key press inputs based on the current game state.
     *
     * @param e The key event.
     */
    private void handleKeyPress(KeyEvent e) {
        int key = e.getKeyCode();
        if (state == stateMenu) {
            if (key == KeyEvent.VK_SPACE || key == KeyEvent.VK_ENTER) {
                state = statePlaying;
                playSound(72, 100);
            }
        } else if (state == statePlaying) {
            if (key == KeyEvent.VK_LEFT || key == KeyEvent.VK_A) {
                targetPaddleX -= 35;
            } else if (key == KeyEvent.VK_RIGHT || key == KeyEvent.VK_D) {
                targetPaddleX += 35;
            } else if (key == KeyEvent.VK_SPACE) {
                launchBalls();
            } else if (key == KeyEvent.VK_P) {
                state = statePaused;
            }
        } else if (state == statePaused) {
            if (key == KeyEvent.VK_P) {
                state = statePlaying;
            }
        } else if (state == stateGameover || state == stateVictory) {
            if (key == KeyEvent.VK_SPACE || key == KeyEvent.VK_ENTER) {
                resetFullGame();
            }
        }
    }

    /**
     * Releases resting balls from the paddle.
     */
    private void launchBalls() {
        for (Ball b : balls) {
            if (b.onPaddle) {
                b.onPaddle = false;
                double speed = 6.0 + (level - 1) * 0.8;
                double angle = (Math.random() - 0.5) * (Math.PI / 4);
                b.dx = speed * Math.sin(angle);
                b.dy = -speed * Math.cos(angle);
                playSound(67, 100);
            }
        }
    }

    /**
     * Activates a captured power-up.
     *
     * @param type The power-up type.
     */
    private void activatePowerUp(int type) {
        if (type == 0) {
            ArrayList<Ball> newBalls = new ArrayList<>();
            for (Ball b : balls) {
                double speed = Math.sqrt(b.dx * b.dx + b.dy * b.dy);
                if (speed < 4) {
                    speed = 6;
                }
                newBalls.add(new Ball(b.x, b.y, -speed * 0.5, -speed * 0.866));
                newBalls.add(new Ball(b.x, b.y, speed * 0.5, -speed * 0.866));
            }
            balls.addAll(newBalls);
            playMelody(new int[]{72, 76, 79}, new int[]{50, 50, 50}, new int[]{80, 80, 100});
        } else if (type == 1) {
            paddleWidth = 180;
            widePaddleTimer = 600;
            playMelody(new int[]{60, 65, 72}, new int[]{50, 50, 50}, new int[]{80, 80, 100});
        } else if (type == 2) {
            fireballActive = true;
            fireballTimer = 360;
            playMelody(new int[]{67, 71, 79}, new int[]{50, 50, 50}, new int[]{80, 80, 100});
        }
    }

    /**
     * Plays victory fanfare and generates celebration particles.
     */
    private void playVictoryFanfare() {
        int[] notes = {60, 64, 67, 72, 72, 72, 67, 72};
        int[] delays = {100, 100, 100, 150, 150, 150, 150, 150};
        int[] durations = {150, 150, 150, 200, 200, 200, 200, 300};
        playMelody(notes, delays, durations);
    }

    /**
     * {@inheritDoc}
     * <p>Runs the high-frequency animation tick, updating positions, handling physics,
     * checking wall, brick, and paddle collisions, and evaluating win/loss conditions.</p>
     */
    @Override
    public void actionPerformed(ActionEvent e) {
        if (state != statePlaying) {
            repaint();
            return;
        }

        // Smooth paddle transition
        paddleX += (targetPaddleX - paddleX) * 0.25;
        if (paddleX < 20) {
            paddleX = 20;
        }
        if (paddleX + paddleWidth > width - 20) {
            paddleX = width - 20 - paddleWidth;
        }

        // Handle power-up timers
        if (widePaddleTimer > 0) {
            widePaddleTimer--;
            if (widePaddleTimer == 0) {
                paddleWidth = 120;
            }
        }
        if (fireballTimer > 0) {
            fireballTimer--;
            if (fireballTimer == 0) {
                fireballActive = false;
            }
        }

        // Update dropping items
        Iterator<PowerUp> puIt = powerUps.iterator();
        while (puIt.hasNext()) {
            PowerUp pu = puIt.next();
            pu.update();

            if (pu.y + 12 >= paddleY && pu.y <= paddleY + paddleHeight) {
                if (pu.x + 12 >= paddleX && pu.x <= paddleX + paddleWidth) {
                    activatePowerUp(pu.type);
                    puIt.remove();
                    continue;
                }
            }
            if (pu.y > height) {
                puIt.remove();
            }
        }

        // Update visual particles
        Iterator<Particle> pIt = particles.iterator();
        while (pIt.hasNext()) {
            Particle p = pIt.next();
            p.update();
            if (p.life <= 0) {
                pIt.remove();
            }
        }

        // Evaluate victory status
        boolean hasBricks = false;
        for (Brick br : bricks) {
            if (br.active) {
                hasBricks = true;
                break;
            }
        }

        if (!hasBricks) {
            state = stateVictory;
            playVictoryFanfare();
            for (int i = 0; i < 150; i++) {
                Color[] confCol = {new Color(0, 77, 152), new Color(165, 0, 68), new Color(237, 187, 0)};
                particles.add(new Particle(Math.random() * width, 50, (Math.random() - 0.5) * 6, Math.random() * 4 + 1, confCol[new Random().nextInt(3)]));
            }
            repaint();
            return;
        }

        // Handle active balls
        Iterator<Ball> bIt = balls.iterator();
        while (bIt.hasNext()) {
            Ball ball = bIt.next();
            if (ball.onPaddle) {
                ball.x = paddleX + paddleWidth / 2.0;
                ball.y = paddleY - ball.radius;
                continue;
            }

            ball.update();

            // Arena border bounces
            if (ball.x - ball.radius <= 20) {
                ball.x = 20 + ball.radius;
                ball.dx = -ball.dx;
                playSound(60, 50);
            } else if (ball.x + ball.radius >= width - 20) {
                ball.x = width - 20 - ball.radius;
                ball.dx = -ball.dx;
                playSound(60, 50);
            }

            if (ball.y - ball.radius <= 50) {
                ball.y = 50 + ball.radius;
                ball.dy = -ball.dy;
                playSound(60, 50);
            }

            // Ball dropped down
            if (ball.y - ball.radius > height) {
                bIt.remove();
                continue;
            }

            // Paddle bounce and reflection angle
            if (ball.y + ball.radius >= paddleY && ball.y - ball.radius <= paddleY + paddleHeight) {
                if (ball.x + ball.radius >= paddleX && ball.x - ball.radius <= paddleX + paddleWidth) {
                    double relativeHit = (ball.x - paddleX) / paddleWidth;
                    if (relativeHit < 0) {
                        relativeHit = 0;
                    }
                    if (relativeHit > 1) {
                        relativeHit = 1;
                    }
                    double angle = (relativeHit - 0.5) * 2 * (Math.PI / 3);
                    double speed = 6.0 + (level - 1) * 0.8;
                    ball.dx = speed * Math.sin(angle);
                    ball.dy = -speed * Math.cos(angle);
                    ball.y = paddleY - ball.radius;
                    playSound(55, 80);
                }
            }

            // Brick destruction detection
            Brick hitBrick = null;
            double minOverlap = Double.MAX_VALUE;
            boolean horizontalCollision = false;
            double overlapXResolved = 0;
            double overlapYResolved = 0;

            for (Brick brick : bricks) {
                if (!brick.active) {
                    continue;
                }

                if (ball.x + ball.radius >= brick.x && ball.x - ball.radius <= brick.x + brick.width &&
                    ball.y + ball.radius >= brick.y && ball.y - ball.radius <= brick.y + brick.height) {

                    double ballCenterX = ball.x;
                    double ballCenterY = ball.y;
                    double brickCenterX = brick.x + brick.width / 2.0;
                    double brickCenterY = brick.y + brick.height / 2.0;

                    double overlapX = (ball.radius + brick.width / 2.0) - Math.abs(ballCenterX - brickCenterX);
                    double overlapY = (ball.radius + brick.height / 2.0) - Math.abs(ballCenterY - brickCenterY);

                    if (overlapX > 0 && overlapY > 0) {
                        if (overlapX < overlapY) {
                            if (overlapX < minOverlap) {
                                minOverlap = overlapX;
                                hitBrick = brick;
                                horizontalCollision = true;
                                overlapXResolved = (ballCenterX < brickCenterX) ? -overlapX : overlapX;
                            }
                        } else {
                            if (overlapY < minOverlap) {
                                minOverlap = overlapY;
                                hitBrick = brick;
                                horizontalCollision = false;
                                overlapYResolved = (ballCenterY < brickCenterY) ? -overlapY : overlapY;
                            }
                        }
                    }
                }
            }

            if (hitBrick != null) {
                if (!fireballActive) {
                    if (horizontalCollision) {
                        ball.dx = -ball.dx;
                        ball.x += overlapXResolved;
                    } else {
                        ball.dy = -ball.dy;
                        ball.y += overlapYResolved;
                    }
                }

                hitBrick.hitsLeft--;
                score += 10;

                Color partColor;
                if (hitBrick.type == 3) {
                    partColor = new Color(0, 77, 152); // Azul (Blue) particles
                } else if (hitBrick.type == 2) {
                    partColor = new Color(165, 0, 68); // Grana (Deep Red) particles
                } else {
                    partColor = new Color(254, 230, 100); // Light Yellow particles
                }
                for (int i = 0; i < 8; i++) {
                    particles.add(new Particle(ball.x, ball.y, (Math.random() - 0.5) * 4, (Math.random() - 0.5) * 4 - 2, partColor));
                }

                if (hitBrick.hitsLeft <= 0) {
                    hitBrick.active = false;
                    playSound(81, 100);
                    if (Math.random() < 0.20) {
                        int powerType = new Random().nextInt(3);
                        powerUps.add(new PowerUp(hitBrick.x + hitBrick.width / 2.0, hitBrick.y + hitBrick.height, powerType));
                    }
                } else {
                    playSound(76, 50);
                }
            }
        }

        // All balls lost
        if (balls.isEmpty()) {
            lives--;
            playSound(48, 200);
            try {
                Thread.sleep(200);
            } catch (Exception ex) {
                // Ignore sleep interruption
            }
            playSound(45, 300);

            if (lives <= 0) {
                state = stateGameover;
            } else {
                Ball b = new Ball(paddleX + paddleWidth / 2.0, paddleY - 8, 0, 0);
                b.onPaddle = true;
                balls.add(b);
            }
        }

        repaint();
    }

    /**
     * {@inheritDoc}
     * <p>Draws the background arena, scoreboard, paddle, football, bricks, and overlays.</p>
     */
    @Override
    protected void paintComponent(Graphics g) {
        super.paintComponent(g);
        Graphics2D g2 = (Graphics2D) g;

        drawBackground(g2, width, height);

        for (Brick br : bricks) {
            drawBrick(g2, br);
        }

        for (PowerUp pu : powerUps) {
            drawPowerUp(g2, pu);
        }

        for (Particle p : particles) {
            p.draw(g2);
        }

        drawPaddle(g2, paddleX, paddleY, paddleWidth, paddleHeight);

        for (Ball ball : balls) {
            if (fireballActive) {
                for (int i = 0; i < ball.trail.size(); i++) {
                    Point2D.Double pt = ball.trail.get(i);
                    double r = ball.radius * (1.0 + (double) i / ball.trail.size() * 0.4);
                    float alpha = (float) i / ball.trail.size() * 0.35f;
                    g2.setColor(new Color(255, 69, 0, (int) (alpha * 255)));
                    g2.fill(new Ellipse2D.Double(pt.x - r, pt.y - r, r * 2, r * 2));
                }
            }
            drawFootball(g2, ball.x, ball.y, ball.radius);
        }

        // Render Scoreboard / Header
        g2.setColor(new Color(0, 77, 152, 220));
        g2.fillRect(0, 0, width, 50);
        g2.setColor(new Color(237, 187, 0));
        g2.fillRect(0, 48, width, 3);

        g2.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON);
        g2.setFont(new Font("SansSerif", Font.BOLD, 18));
        g2.drawString("BARÇA ARKANOID 🐐", 30, 31);

        g2.setFont(new Font("Monospaced", Font.BOLD, 18));
        g2.setColor(Color.WHITE);
        g2.drawString("GOLS: " + score, width / 2 - 50, 31);

        g2.setFont(new Font("SansSerif", Font.PLAIN, 18));
        StringBuilder livesStr = new StringBuilder();
        for (int i = 0; i < lives; i++) {
            livesStr.append("⚽ ");
        }
        g2.drawString("VIDES: " + livesStr.toString(), width - 180, 31);

        // Active Buff reminders
        if (fireballActive || widePaddleTimer > 0) {
            g2.setFont(new Font("SansSerif", Font.BOLD, 14));
            g2.setColor(new Color(255, 140, 0));
            String activeBuffs = "";
            if (fireballActive) {
                activeBuffs += "🔥 FIREBALL! ";
            }
            if (widePaddleTimer > 0) {
                activeBuffs += "🛡️ CAMP NOU WIDTH! ";
            }
            g2.drawString(activeBuffs, 30, 73);
        }

        // Draw overlay screens
        if (state == stateMenu) {
            drawSplashOverlay(g2, "BARÇA ARKANOID", "VISCA EL BARÇA & SMASH THE TILES!", "CLICK MOUSE OR PRESS SPACE TO PLAY");
        } else if (state == statePaused) {
            drawSplashOverlay(g2, "JOC PAUSAT", "READY TO SCORE MORE GOALS?", "PRESS 'P' TO RESUME");
        } else if (state == stateGameover) {
            drawSplashOverlay(g2, "CANDIDAT A L'INFERN! 😭", "The defense collapsed! Real Madrid got away!", "CLICK OR PRESS SPACE TO TRY AGAIN");
        } else if (state == stateVictory) {
            drawSplashOverlay(g2, "CAMPIONS DE EUROPA! 🏆", "You cleared the flag mosaic of glory!", "CLICK OR PRESS SPACE FOR NEXT LEVEL");
        }
    }

    /**
     * Draws the stadium background.
     *
     * @param g2 The graphics context.
     * @param w  The viewport width.
     * @param h  The viewport height.
     */
    private void drawBackground(Graphics2D g2, int w, int h) {
        GradientPaint gp = new GradientPaint(0, 0, new Color(5, 10, 30), 0, h, new Color(10, 20, 50));
        g2.setPaint(gp);
        g2.fillRect(0, 0, w, h);

        g2.setColor(new Color(0, 255, 128, 30));
        g2.setStroke(new BasicStroke(2f));
        g2.draw(new RoundRectangle2D.Double(20, 20, w - 40, h - 40, 15, 15));
        g2.draw(new Line2D.Double(20, h / 2.0, w - 20, h / 2.0));
        g2.draw(new Ellipse2D.Double(w / 2.0 - 80, h / 2.0 - 80, 160, 160));
        g2.draw(new Rectangle2D.Double(w / 2.0 - 150, h - 120, 300, 100));
        g2.draw(new Rectangle2D.Double(w / 2.0 - 70, h - 50, 140, 30));
    }

    /**
     * Draws the striped Barça player paddle.
     *
     * @param g2 The graphics context.
     * @param x  The paddle X position.
     * @param y  The paddle Y position.
     * @param w  The width of the paddle.
     * @param h  The height of the paddle.
     */
    private void drawPaddle(Graphics2D g2, double x, double y, double w, double h) {
        g2.setColor(new Color(237, 187, 0));
        g2.fill(new RoundRectangle2D.Double(x - 2, y - 2, w + 4, h + 4, 10, 10));

        int stripeCount = 5;
        double stripeW = w / stripeCount;
        for (int i = 0; i < stripeCount; i++) {
            Color c = (i % 2 == 0) ? new Color(0, 77, 152) : new Color(165, 0, 68);
            g2.setColor(c);
            g2.fill(new Rectangle2D.Double(x + i * stripeW, y, stripeW, h));
        }

        g2.setColor(new Color(255, 255, 255, 80));
        g2.fill(new Rectangle2D.Double(x, y, w, h / 2.0));
    }

    /**
     * Draws a football vector graphics ball.
     *
     * @param g2 The graphics context.
     * @param x  The ball's center X coordinate.
     * @param y  The ball's center Y coordinate.
     * @param r  The radius of the ball.
     */
    private void drawFootball(Graphics2D g2, double x, double y, double r) {
        g2.setColor(Color.WHITE);
        g2.fill(new Ellipse2D.Double(x - r, y - r, r * 2, r * 2));
        g2.setColor(Color.BLACK);
        g2.setStroke(new BasicStroke(1.5f));
        g2.draw(new Ellipse2D.Double(x - r, y - r, r * 2, r * 2));

        Path2D.Double path = new Path2D.Double();
        double cx = x, cy = y;
        double size = r * 0.4;
        for (int i = 0; i < 5; i++) {
            double angle = i * 2 * Math.PI / 5 - Math.PI / 2;
            double px = cx + size * Math.cos(angle);
            double py = cy + size * Math.sin(angle);
            if (i == 0) {
                path.moveTo(px, py);
            } else {
                path.lineTo(px, py);
            }
        }
        path.closePath();
        g2.setColor(Color.BLACK);
        g2.fill(path);

        for (int i = 0; i < 5; i++) {
            double angle = i * 2 * Math.PI / 5 - Math.PI / 2;
            double px = cx + size * Math.cos(angle);
            double py = cy + size * Math.sin(angle);
            double edgeX = cx + r * Math.cos(angle);
            double edgeY = cy + r * Math.sin(angle);
            g2.draw(new Line2D.Double(px, py, edgeX, edgeY));
        }
    }

    /**
     * Draws a Barça flag themed brick with proper highlight shine.
     * <p>
     * Renders cracks on damaged bricks based on remaining hit points:
     * - Azul/Blue bricks (Type 3) require 3 hits and show cracks on the 1st and 2nd hits.
     * - Grana bricks (Type 2) require 2 hits and show cracks on the 1st hit.
     * - Light Yellow background bricks (Type 1) require 1 hit and break instantly.
     * </p>
     *
     * @param g2    The graphics context.
     * @param brick The brick to render.
     */
    private void drawBrick(Graphics2D g2, Brick brick) {
        if (!brick.active) {
            return;
        }

        Color baseColor;
        Color borderColor;
        if (brick.type == 3) {
            baseColor = new Color(0, 77, 152); // Azul (Blue)
            borderColor = new Color(237, 187, 0); // Gold border
        } else if (brick.type == 2) {
            baseColor = new Color(165, 0, 68); // Grana (Deep Red)
            borderColor = new Color(237, 187, 0); // Gold border
        } else {
            baseColor = new Color(254, 230, 100); // Light Yellow
            borderColor = new Color(220, 190, 50); // Muted gold border
        }

        g2.setColor(baseColor);
        g2.fill(new RoundRectangle2D.Double(brick.x, brick.y, brick.width, brick.height, 6, 6));

        g2.setColor(borderColor);
        g2.setStroke(new BasicStroke(1.5f));
        g2.draw(new RoundRectangle2D.Double(brick.x, brick.y, brick.width, brick.height, 6, 6));

        g2.setColor(new Color(255, 255, 255, 120));
        g2.fill(new Rectangle2D.Double(brick.x + 1, brick.y + 1, brick.width - 2, brick.height / 3.0));

        // Draw visual cracks on damaged bricks to show damage progress
        int maxHits = (brick.type == 3) ? 3 : (brick.type == 2 ? 2 : 1);
        if (brick.hitsLeft < maxHits) {
            g2.setColor(new Color(0, 0, 0, 160)); // Dark crack lines
            g2.setStroke(new BasicStroke(1.5f));
            double bx = brick.x;
            double by = brick.y;
            double bw = brick.width;
            double bh = brick.height;
            if (brick.hitsLeft == 1) {
                // Major damage - multiple branching cracks
                g2.draw(new Line2D.Double(bx + bw * 0.2, by + 2, bx + bw * 0.4, by + bh * 0.8));
                g2.draw(new Line2D.Double(bx + bw * 0.4, by + bh * 0.8, bx + bw * 0.7, by + bh * 0.3));
                g2.draw(new Line2D.Double(bx + bw * 0.7, by + bh * 0.3, bx + bw * 0.9, by + bh * 0.9));
                g2.draw(new Line2D.Double(bx + bw * 0.1, by + bh * 0.5, bx + bw * 0.3, by + bh * 0.4));
                g2.draw(new Line2D.Double(bx + bw * 0.5, by + bh * 0.2, bx + bw * 0.6, by + bh * 0.5));
            } else if (brick.hitsLeft == 2) {
                // Moderate damage - single fracture crack
                g2.draw(new Line2D.Double(bx + bw * 0.3, by + 2, bx + bw * 0.5, by + bh * 0.6));
                g2.draw(new Line2D.Double(bx + bw * 0.5, by + bh * 0.6, bx + bw * 0.8, by + bh * 0.4));
            }
        }
    }

    /**
     * Draws floating power-up symbols.
     *
     * @param g2 The graphics context.
     * @param pu The power-up object to draw.
     */
    private void drawPowerUp(Graphics2D g2, PowerUp pu) {
        Color glowColor;
        String glyph;
        if (pu.type == 0) {
            glowColor = Color.YELLOW;
            glyph = "⭐";
        } else if (pu.type == 1) {
            glowColor = new Color(0, 191, 255);
            glyph = "🛡️";
        } else {
            glowColor = new Color(255, 69, 0);
            glyph = "🔥";
        }

        g2.setColor(new Color(glowColor.getRed(), glowColor.getGreen(), glowColor.getBlue(), 80));
        g2.fill(new Ellipse2D.Double(pu.x - 12, pu.y - 12, 24, 24));
        g2.setColor(glowColor);
        g2.draw(new Ellipse2D.Double(pu.x - 12, pu.y - 12, 24, 24));

        g2.setFont(new Font("SansSerif", Font.PLAIN, 14));
        g2.drawString(glyph, (int) pu.x - 7, (int) pu.y + 5);
    }

    /**
     * Renders overlays for splash, victory, or gameover states.
     *
     * @param g2    The graphics context.
     * @param title The primary overlay title.
     * @param sub   The secondary subtitle text.
     * @param action The call-to-action prompt.
     */
    private void drawSplashOverlay(Graphics2D g2, String title, String sub, String action) {
        g2.setColor(new Color(10, 20, 50, 220));
        g2.fillRect(20, 50, width - 40, height - 70);

        g2.setFont(new Font("SansSerif", Font.BOLD, 42));
        g2.setColor(new Color(237, 187, 0));
        int tw = g2.getFontMetrics().stringWidth(title);
        g2.drawString(title, (width - tw) / 2, height / 2 - 50);

        g2.setFont(new Font("SansSerif", Font.BOLD, 18));
        g2.setColor(Color.WHITE);
        int sw = g2.getFontMetrics().stringWidth(sub);
        g2.drawString(sub, (width - sw) / 2, height / 2 + 10);

        long blinkTime = System.currentTimeMillis() % 1000;
        if (blinkTime < 600) {
            g2.setFont(new Font("SansSerif", Font.ITALIC, 16));
            g2.setColor(new Color(0, 255, 128));
            int aw = g2.getFontMetrics().stringWidth(action);
            g2.drawString(action, (width - aw) / 2, height / 2 + 80);
        }
    }

    /**
     * Standalone main launcher for the Arkanoid game.
     *
     * @param args Command line arguments.
     */
    public static void main(String[] args) {
        SwingUtilities.invokeLater(() -> {
            JFrame frame = new JFrame("Anahata Barça Arkanoid");
            frame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
            Arkanoid game = new Arkanoid();
            frame.add(game);
            frame.pack();
            frame.setLocationRelativeTo(null);
            frame.setResizable(false);
            frame.setVisible(true);
        });
    }
}

/**
 * Represents a game ball.
 */
class Ball {
    /**
     * Ball center X-coordinate.
     */
    double x;

    /**
     * Ball center Y-coordinate.
     */
    double y;

    /**
     * Horizontal velocity vector.
     */
    double dx;

    /**
     * Vertical velocity vector.
     */
    double dy;

    /**
     * Ball collision radius.
     */
    double radius = 8;

    /**
     * Flag indicating if the ball is currently resting on the paddle.
     */
    boolean onPaddle = false;

    /**
     * Historical trails for visual motion glow.
     */
    final java.util.ArrayList<java.awt.geom.Point2D.Double> trail = new java.util.ArrayList<>();

    /**
     * Constructs a Ball with coordinates and velocity vectors.
     *
     * @param x  The starting X position.
     * @param y  The starting Y position.
     * @param dx The velocity vector X.
     * @param dy The velocity vector Y.
     */
    Ball(double x, double y, double dx, double dy) {
        this.x = x;
        this.y = y;
        this.dx = dx;
        this.dy = dy;
    }

    /**
     * Updates the ball's position and tracks trail coordinates.
     */
    void update() {
        if (!onPaddle) {
            x += dx;
            y += dy;
            trail.add(new java.awt.geom.Point2D.Double(x, y));
            if (trail.size() > 10) {
                trail.remove(0);
            }
        } else {
            trail.clear();
        }
    }
}

/**
 * Represents a breakable brick.
 */
class Brick {
    /**
     * Top-left X-coordinate of the brick.
     */
    double x;

    /**
     * Top-left Y-coordinate of the brick.
     */
    double y;

    /**
     * The width of the brick.
     */
    double width;

    /**
     * The height of the brick.
     */
    double height;

    /**
     * The brick color type (1: Azul, 2: Grana, 3: Gold).
     */
    int type;

    /**
     * Hits remaining to destroy the brick.
     */
    int hitsLeft;

    /**
     * Flag indicating if the brick is active.
     */
    boolean active = true;

    /**
     * Constructs a Brick with specifications.
     *
     * @param x        Top-left X.
     * @param y        Top-left Y.
     * @param width    Width.
     * @param height   Height.
     * @param type     Color Type.
     * @param hitsLeft Hits to break.
     */
    Brick(double x, double y, double width, double height, int type, int hitsLeft) {
        this.x = x;
        this.y = y;
        this.width = width;
        this.height = height;
        this.type = type;
        this.hitsLeft = hitsLeft;
    }
}

/**
 * Represents a power-up falling item.
 */
class PowerUp {
    /**
     * Center X-coordinate of the item.
     */
    double x;

    /**
     * Center Y-coordinate of the item.
     */
    double y;

    /**
     * Vertical falling speed.
     */
    double speedY = 3.0;

    /**
     * Power-up type (0: Multiball, 1: Wide Paddle, 2: Fireball).
     */
    int type;

    /**
     * Constructs a PowerUp item.
     *
     * @param x    Starting X.
     * @param y    Starting Y.
     * @param type Power Type index.
     */
    PowerUp(double x, double y, int type) {
        this.x = x;
        this.y = y;
        this.type = type;
    }

    /**
     * Advances the power-up falling position.
     */
    void update() {
        y += speedY;
    }
}

/**
 * Particle used for brick break celebrations and hits.
 */
class Particle {
    /**
     * Particle position X.
     */
    double x;

    /**
     * Particle position Y.
     */
    double y;

    /**
     * Drift velocity X.
     */
    double dx;

    /**
     * Drift velocity Y.
     */
    double dy;

    /**
     * Particle primary color.
     */
    Color color;

    /**
     * Active lifespan steps remaining.
     */
    int life = 30;

    /**
     * Maximum lifespan of the particle.
     */
    int maxLife = 30;

    /**
     * Constructs a particle.
     *
     * @param x     Starting X.
     * @param y     Starting Y.
     * @param dx    Horizontal velocity.
     * @param dy    Vertical velocity.
     * @param color Rendering color.
     */
    Particle(double x, double y, double dx, double dy, Color color) {
        this.x = x;
        this.y = y;
        this.dx = dx;
        this.dy = dy;
        this.color = color;
    }

    /**
     * Updates particle position, applies slight gravity, and decays life.
     */
    void update() {
        x += dx;
        y += dy;
        dy += 0.1; // gravity effect
        life--;
    }

    /**
     * Renders the particle.
     *
     * @param g2 The graphics context.
     */
    void draw(Graphics2D g2) {
        float alpha = (float) life / maxLife;
        if (alpha < 0) {
            alpha = 0;
        }
        g2.setColor(new Color(color.getRed(), color.getGreen(), color.getBlue(), (int) (alpha * 255)));
        g2.fillRect((int) x, (int) y, 4, 4);
    }
}
