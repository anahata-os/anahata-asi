/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.games;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.Random;

/**
 * Anahata Bug Swarm Defense (Tactical Edition).
 * <p>
 * A fast-paced "Space Invaders" style game where the user defends the 
 * digital continent from descending logic bugs.
 * </p>
 * 
 * @author anahata
 */
public class BugDefense extends JPanel implements ActionListener {
    /**
     * The game loop timer.
     */
    private Timer timer;
    /**
     * The horizontal X position of the player's shield.
     */
    private int shieldX = 225;
    /**
     * The fixed vertical Y position of the player's shield.
     */
    private final int shieldY = 540;
    /**
     * The width of the shield.
     */
    private final int shieldWidth = 50;
    /**
     * The height of the shield.
     */
    private final int shieldHeight = 20;
    
    /**
     * Flag indicating if the game is currently active.
     */
    private boolean playing = false;
    /**
     * The player's current score.
     */
    private int score = 0;
    /**
     * The player's remaining lives.
     */
    private int lives = 3;
    
    /**
     * The list of active projectile pulses fired by the player.
     */
    private java.util.List<Rectangle> optimizationPulses = new ArrayList<>();
    /**
     * The list of descending logic bug obstacles.
     */
    private java.util.List<Rectangle> bugs = new ArrayList<>();
    /**
     * Random number generator for spawning bug coordinates.
     */
    private final Random rand = new Random();
    /**
     * Tick counter to regulate enemy spawning frequency.
     */
    private int bugSpawnTick = 0;

    /**
     * Constructs a new BugDefense panel and adds the start button.
     */
    public BugDefense() {
        setPreferredSize(new Dimension(500, 600));
        setBackground(new Color(20, 20, 25));
        setFocusable(true);
        
        setLayout(new GridBagLayout());
        JButton startBtn = new JButton("ACTIVATE SHIELD (START)");
        startBtn.setBackground(new Color(0, 77, 152));
        startBtn.setForeground(Color.WHITE);
        startBtn.setFont(new Font("SansSerif", Font.BOLD, 16));
        startBtn.setFocusPainted(false);
        startBtn.addActionListener(e -> {
            remove(startBtn);
            startGame();
            revalidate();
            requestFocusInWindow();
        });
        add(startBtn);

        setupKeyBindings();
    }

    /**
     * Installs left/right arrow and spacebar key bindings.
     */
    private void setupKeyBindings() {
        InputMap im = getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW);
        ActionMap am = getActionMap();
        im.put(KeyStroke.getKeyStroke("LEFT"), "left");
        im.put(KeyStroke.getKeyStroke("RIGHT"), "right");
        im.put(KeyStroke.getKeyStroke("SPACE"), "shootOrRestart");
        am.put("left", new AbstractAction() { public void actionPerformed(ActionEvent e) { if (playing) shieldX = Math.max(0, shieldX - 20); } });
        am.put("right", new AbstractAction() { public void actionPerformed(ActionEvent e) { if (playing) shieldX = Math.min(getWidth() - shieldWidth, shieldX + 20); } });
        am.put("shootOrRestart", new AbstractAction() { 
            public void actionPerformed(ActionEvent e) { 
                if (playing) {
                    spawnPulse(); 
                } else if (lives <= 0) {
                    startGame();
                }
            } 
        });
    }

    /**
     * Starts/resets the game state and kicks off the timer.
     */
    private void startGame() {
        playing = true;
        score = 0;
        lives = 3;
        bugs.clear();
        optimizationPulses.clear();
        timer = new Timer(20, this);
        timer.start();
    }

    /**
     * Fires a new optimization pulse from the shield's position.
     */
    private void spawnPulse() {
        optimizationPulses.add(new Rectangle(shieldX + shieldWidth / 2 - 3, shieldY - 10, 6, 12));
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        if (!playing) return;
        Iterator<Rectangle> pIter = optimizationPulses.iterator();
        while (pIter.hasNext()) {
            Rectangle p = pIter.next();
            p.y -= 10;
            if (p.y < 0) pIter.remove();
        }
        bugSpawnTick++;
        if (bugSpawnTick > Math.max(10, 40 - (score / 100))) {
            bugs.add(new Rectangle(rand.nextInt(460), -30, 30, 30));
            bugSpawnTick = 0;
        }
        Iterator<Rectangle> bIter = bugs.iterator();
        while (bIter.hasNext()) {
            Rectangle b = bIter.next();
            b.y += 2 + (score / 500);
            if (b.y > getHeight()) {
                bIter.remove();
                lives--;
                if (lives <= 0) {
                    playing = false;
                    timer.stop();
                    // Custom Game Over overlay handles the rest
                }
            }
        }
        checkCollisions();
        repaint();
    }

    /**
     * Performs collision detection between optimization pulses and bugs, updating scores.
     */
    private void checkCollisions() {
        Iterator<Rectangle> bIter = bugs.iterator();
        while (bIter.hasNext()) {
            Rectangle b = bIter.next();
            Iterator<Rectangle> pIter = optimizationPulses.iterator();
            while (pIter.hasNext()) {
                Rectangle p = pIter.next();
                if (b.intersects(p)) {
                    bIter.remove();
                    pIter.remove();
                    score += 10;
                    break;
                }
            }
        }
    }

    @Override
    protected void paintComponent(Graphics g) {
        super.paintComponent(g);
        Graphics2D g2d = (Graphics2D) g;
        g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        if (!playing && lives > 0) return;

        if (lives <= 0) {
            renderGameOver(g2d);
            return;
        }

        g2d.setColor(new Color(165, 0, 68, 100));
        g2d.fillRect(0, 580, getWidth(), 20);
        g2d.setColor(new Color(0, 77, 152));
        g2d.fillRoundRect(shieldX, shieldY, shieldWidth, shieldHeight, 10, 10);
        g2d.setColor(new Color(40, 167, 69));
        for (Rectangle p : optimizationPulses) g2d.fillOval(p.x, p.y, p.width, p.height);
        g2d.setColor(Color.RED);
        for (Rectangle b : bugs) g2d.fillRoundRect(b.x, b.y, b.width, b.height, 5, 5);
        g2d.setColor(Color.WHITE);
        g2d.drawString("Score: " + score + " | Lives: " + lives, 10, 30);
    }

    /**
     * Renders the Barça Red 'PRODUCTION CRASHED' game over screen.
     * @param g2d The graphics context.
     */
    private void renderGameOver(Graphics2D g2d) {
        g2d.setColor(new Color(165, 0, 68)); // Barça Red
        g2d.setFont(new Font("SansSerif", Font.BOLD, 36));
        FontMetrics fm = g2d.getFontMetrics();
        String msg = "PRODUCTION CRASHED";
        g2d.drawString(msg, (getWidth() - fm.stringWidth(msg)) / 2, getHeight() / 2 - 40);

        g2d.setColor(Color.WHITE);
        g2d.setFont(new Font("SansSerif", Font.PLAIN, 20));
        fm = g2d.getFontMetrics();
        String scoreMsg = "Final Score: " + score;
        g2d.drawString(scoreMsg, (getWidth() - fm.stringWidth(scoreMsg)) / 2, getHeight() / 2 + 10);

        g2d.setColor(Color.LIGHT_GRAY);
        g2d.setFont(new Font("SansSerif", Font.ITALIC, 16));
        fm = g2d.getFontMetrics();
        String restartMsg = "Press SPACE to Hot-Reload";
        g2d.drawString(restartMsg, (getWidth() - fm.stringWidth(restartMsg)) / 2, getHeight() / 2 + 50);
    }

    /**
     * Launcher method for testing BugDefense as a standalone application.
     * @param args Command line arguments (ignored).
     */
    public static void main(String[] args) {
        JFrame frame = new JFrame("Anahata Bug Defense");
        frame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
        frame.add(new BugDefense());
        frame.pack();
        frame.setLocationRelativeTo(null);
        frame.setVisible(true);
    }
}
