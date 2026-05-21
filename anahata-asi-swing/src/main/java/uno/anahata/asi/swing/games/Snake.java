/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.games;

import java.awt.*;
import java.awt.event.*;
import java.util.Random;
import javax.swing.*;

/**
 * A classic Snake game implementation with a Mapacho theme.
 * <p>
 * Features grid-based movement, randomized "cigar" spawning, and a 
 * score tracking system. Optimized for execution as a standalone 
 * easter egg from the support panel.
 * </p>
 * 
 * @author anahata
 */
public class Snake extends JPanel implements ActionListener {

    /**
     * The size of a single grid square in pixels.
     */
    private final int TILE_SIZE = 25;
    /**
     * The width of the game board in pixels.
     */
    private final int WIDTH = 800;
    /**
     * The height of the game board in pixels.
     */
    private final int HEIGHT = 600;
    /**
     * The maximum possible tiles fitting onto the board.
     */
    private final int ALL_TILES = (WIDTH * HEIGHT) / (TILE_SIZE * TILE_SIZE);

    /**
     * Array tracking the horizontal coordinates of all snake segments.
     */
    private final int x[] = new int[ALL_TILES];
    /**
     * Array tracking the vertical coordinates of all snake segments.
     */
    private final int y[] = new int[ALL_TILES];

    /**
     * The current length of the snake.
     */
    private int bodyParts = 6;
    /**
     * The number of cigars eaten during this round.
     */
    private int cigarsEaten;
    /**
     * The current grid X coordinate of the cigar target.
     */
    private int cigarX;
    /**
     * The current grid Y coordinate of the cigar target.
     */
    private int cigarY;

    /**
     * The active snake movement direction (U, D, L, R).
     */
    private char direction = 'R';
    /**
     * Flag indicating if the game loop is active.
     */
    private boolean running = false;
    /**
     * The animation and tick update timer.
     */
    private Timer timer;
    /**
     * Random generator used to spawn new cigars.
     */
    private Random random;

    /**
     * Constructs a new Snake game panel, initializing keyboard events and starting the game.
     */
    public Snake() {
        random = new Random();
        setPreferredSize(new Dimension(WIDTH, HEIGHT));
        setBackground(Color.BLACK);
        setFocusable(true);
        addKeyListener(new MyKeyAdapter());
        startGame();
    }

    /**
     * Initializes the game state and starts the animation timer.
     */
    public void startGame() {
        newCigar();
        running = true;
        // Reduced delay from 75 to 50 for smoother tiki-taka movement
        timer = new Timer(50, this);
        timer.start();
    }

    /**
     * {@inheritDoc}
     * <p>Handles the visual rendering of the mapacho-snake, the cigars, 
     * and the HUD (score and status messages).</p>
     */
    @Override
    public void paintComponent(Graphics g) {
        super.paintComponent(g);
        draw(g);
    }

    /**
     * Renders the game elements.
     * @param g the graphics context.
     */
    public void draw(Graphics g) {
        if (running) {
            Graphics2D g2d = (Graphics2D) g;
            g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
            
            // Draw Cigar
            g2d.setColor(new Color(139, 69, 19)); 
            g2d.fillOval(cigarX, cigarY, TILE_SIZE, TILE_SIZE);
            g2d.setColor(Color.WHITE);
            g2d.drawOval(cigarX, cigarY, TILE_SIZE, TILE_SIZE);

            // Draw Snake
            for (int i = 0; i < bodyParts; i++) {
                if (i == 0) {
                    g2d.setColor(new Color(0, 150, 0)); 
                    g2d.fillRect(x[i], y[i], TILE_SIZE, TILE_SIZE);
                    // Eye
                    g2d.setColor(Color.WHITE);
                    int eyeSize = TILE_SIZE / 5;
                    g2d.fillOval(x[i] + TILE_SIZE - eyeSize * 2, y[i] + eyeSize, eyeSize, eyeSize);
                } else {
                    g2d.setColor(new Color(45, 180, 0)); 
                    g2d.fillRect(x[i], y[i], TILE_SIZE, TILE_SIZE);
                }
            }
            
            g2d.setColor(Color.WHITE);
            g2d.setFont(new Font("Monospaced", Font.BOLD, 20));
            FontMetrics metrics = getFontMetrics(g2d.getFont());
            g2d.drawString("Score: " + cigarsEaten, (WIDTH - metrics.stringWidth("Score: " + cigarsEaten)) / 2, g2d.getFont().getSize());
        } else {
            gameOver(g);
        }
    }

    /**
     * Randomly positions a new cigar on the grid.
     */
    public void newCigar() {
        cigarX = random.nextInt((int) (WIDTH / TILE_SIZE)) * TILE_SIZE;
        cigarY = random.nextInt((int) (HEIGHT / TILE_SIZE)) * TILE_SIZE;
    }

    /**
     * Updates the snake's position based on the current direction.
     */
    public void move() {
        for (int i = bodyParts; i > 0; i--) {
            x[i] = x[i - 1];
            y[i] = y[i - 1];
        }

        switch (direction) {
            case 'U': y[0] = y[0] - TILE_SIZE; break;
            case 'D': y[0] = y[0] + TILE_SIZE; break;
            case 'L': x[0] = x[0] - TILE_SIZE; break;
            case 'R': x[0] = x[0] + TILE_SIZE; break;
        }
    }

    /**
     * Checks if the head has collided with a cigar.
     */
    public void checkCigar() {
        if ((x[0] == cigarX) && (y[0] == cigarY)) {
            bodyParts++;
            cigarsEaten++;
            newCigar();
        }
    }

    /**
     * Checks for collisions with walls or the snake's own body.
     */
    public void checkCollisions() {
        // checks if head collides with body
        for (int i = bodyParts; i > 0; i--) {
            if ((x[0] == x[i]) && (y[0] == y[i])) {
                running = false;
            }
        }
        // check if head touches left border
        if (x[0] < 0) running = false;
        // check if head touches right border
        if (x[0] > WIDTH - TILE_SIZE) running = false;
        // check if head touches top border
        if (y[0] < 0) running = false;
        // check if head touches bottom border
        if (y[0] > HEIGHT - TILE_SIZE) running = false;

        if (!running) {
            timer.stop();
        }
    }

    /**
     * Renders the game over screen.
     * @param g the graphics context.
     */
    public void gameOver(Graphics g) {
        g.setColor(Color.RED);
        g.setFont(new Font("Monospaced", Font.BOLD, 75));
        FontMetrics metrics1 = getFontMetrics(g.getFont());
        g.drawString("GAME OVER", (WIDTH - metrics1.stringWidth("GAME OVER")) / 2, HEIGHT / 2);
        
        g.setColor(Color.WHITE);
        g.setFont(new Font("Monospaced", Font.BOLD, 25));
        FontMetrics metrics2 = getFontMetrics(g.getFont());
        g.drawString("Score: " + cigarsEaten, (WIDTH - metrics2.stringWidth("Score: " + cigarsEaten)) / 2, HEIGHT / 2 + 50);
        g.drawString("Press SPACE to restart", (WIDTH - metrics2.stringWidth("Press SPACE to restart")) / 2, HEIGHT / 2 + 100);
    }

    /**
     * {@inheritDoc}
     * <p>The primary game loop triggered by the timer.</p>
     */
    @Override
    public void actionPerformed(ActionEvent e) {
        if (running) {
            move();
            checkCigar();
            checkCollisions();
        }
        repaint();
    }

    /**
     * Inner class for handling directional input.
     */
    public class MyKeyAdapter extends KeyAdapter {
        @Override
        public void keyPressed(KeyEvent e) {
            switch (e.getKeyCode()) {
                case KeyEvent.VK_LEFT:
                    if (direction != 'R') direction = 'L';
                    break;
                case KeyEvent.VK_RIGHT:
                    if (direction != 'L') direction = 'R';
                    break;
                case KeyEvent.VK_UP:
                    if (direction != 'D') direction = 'U';
                    break;
                case KeyEvent.VK_DOWN:
                    if (direction != 'U') direction = 'D';
                    break;
                case KeyEvent.VK_SPACE:
                    if (!running) {
                        bodyParts = 6;
                        cigarsEaten = 0;
                        direction = 'R';
                        for(int i = 0; i < bodyParts; i++) {
                            x[i] = 0; y[i] = 0;
                        }
                        startGame();
                    }
                    break;
            }
        }
    }

    /**
     * Launcher for the Mapacho Snake game.
     * @param args command line arguments (ignored).
     */
    public static void main(String[] args) {
        SwingUtilities.invokeLater(() -> {
            JFrame frame = new JFrame("Mapacho Snake");
            frame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
            frame.add(new Snake());
            frame.pack();
            frame.setLocationRelativeTo(null);
            frame.setResizable(false);
            frame.setVisible(true);
        });
    }
}
