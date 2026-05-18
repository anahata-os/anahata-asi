package uno.anahata.asi.swing.games;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;
import javax.swing.Timer;

/**
 * A classic Arkanoid (Breakout) clone implemented as a Swing easter egg.
 * <p>This implementation features a physics engine for ball-paddle 
 * collisions, responsive keyboard controls, and a multi-colored brick 
 * layout. It is intended to be launched as a standalone frame from the 
 * support panel.</p>
 * @author anahata
 */
public class Arkanoid extends JPanel implements ActionListener {

    private final int WIDTH = 800;
    private final int HEIGHT = 600;
    private final int PADDLE_Y = 550;
    
    private Timer timer;
    private int ballX = WIDTH / 2;
    private int ballY = HEIGHT / 2;
    private int ballRadius = 8;
    private int ballDX = 5;
    private int ballDY = -5;
    
    private int paddleX = WIDTH / 2 - 60;
    private int paddleWidth = 120;
    private int paddleHeight = 15;
    private int paddleDX = 0;
    
    private boolean inGame = true;
    private boolean won = false;
    private int score = 0;
    
    private Rectangle[] bricks;
    private boolean[] brickActive;
    private int rows = 6;
    private int cols = 10;
    private int brickWidth = 70;
    private int brickHeight = 25;

    public Arkanoid() {
        initGame();
    }

    /**
     * Initializes the game environment, setting up the coordinate system, 
     * keyboard listeners, and the high-frequency animation timer.
     */
    private void initGame() {
        setPreferredSize(new Dimension(WIDTH, HEIGHT));
        setBackground(Color.BLACK);
        setFocusable(true);
        
        addKeyListener(new KeyAdapter() {
            @Override
            public void keyPressed(KeyEvent e) {
                if (e.getKeyCode() == KeyEvent.VK_LEFT) {
                    paddleDX = -8;
                } else if (e.getKeyCode() == KeyEvent.VK_RIGHT) {
                    paddleDX = 8;
                } else if (e.getKeyCode() == KeyEvent.VK_SPACE && (!inGame || won)) {
                    resetGame();
                }
            }

            @Override
            public void keyReleased(KeyEvent e) {
                if (e.getKeyCode() == KeyEvent.VK_LEFT || e.getKeyCode() == KeyEvent.VK_RIGHT) {
                    paddleDX = 0;
                }
            }
        });

        initBricks();
        timer = new Timer(16, this);
        timer.start();
    }
    
    /**
     * Generates the brick layout, calculating positions based on rows and 
     * columns to fill the upper portion of the screen.
     */
    private void initBricks() {
        bricks = new Rectangle[rows * cols];
        brickActive = new boolean[rows * cols];
        int startX = (WIDTH - (cols * (brickWidth + 5))) / 2 + 2;
        int startY = 60;
        
        for (int i = 0; i < rows; i++) {
            for (int j = 0; j < cols; j++) {
                bricks[i * cols + j] = new Rectangle(startX + j * (brickWidth + 5), startY + i * (brickHeight + 5), brickWidth, brickHeight);
                brickActive[i * cols + j] = true;
            }
        }
    }
    
    /**
     * Resets all game state variables (score, positions, bricks) to allow 
     * for a fresh start after a win or loss.
     */
    private void resetGame() {
        ballX = WIDTH / 2;
        ballY = HEIGHT / 2;
        ballDX = 5;
        ballDY = -5;
        paddleX = WIDTH / 2 - paddleWidth / 2;
        score = 0;
        inGame = true;
        won = false;
        initBricks();
        timer.start();
    }

    /**
     * {@inheritDoc}
     * <p>Handles the primary rendering loop, drawing the paddle, ball, and 
     * active bricks. Also renders the Game Over and Victory overlay states.</p>
     */
    @Override
    protected void paintComponent(Graphics g) {
        super.paintComponent(g);
        Graphics2D g2d = (Graphics2D) g;
        g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

        if (inGame && !won) {
            // Draw bricks
            for (int i = 0; i < bricks.length; i++) {
                if (brickActive[i]) {
                    g2d.setColor(Color.getHSBColor((float)i / bricks.length, 0.8f, 0.9f));
                    g2d.fill(bricks[i]);
                    g2d.setColor(Color.BLACK);
                    g2d.draw(bricks[i]);
                }
            }

            // Draw paddle
            g2d.setColor(new Color(100, 200, 255));
            g2d.fillRoundRect(paddleX, PADDLE_Y, paddleWidth, paddleHeight, 10, 10);

            // Draw ball
            g2d.setColor(Color.YELLOW);
            g2d.fillOval(ballX - ballRadius, ballY - ballRadius, ballRadius * 2, ballRadius * 2);
            
            // Draw score
            g2d.setColor(Color.WHITE);
            g2d.setFont(new Font("Monospaced", Font.BOLD, 20));
            g2d.drawString("Score: " + score, 20, 30);
        } else if (won) {
            g2d.setColor(Color.GREEN);
            g2d.setFont(new Font("Monospaced", Font.BOLD, 40));
            g2d.drawString("YOU WIN!", WIDTH / 2 - 100, HEIGHT / 2);
            g2d.setFont(new Font("Monospaced", Font.BOLD, 20));
            g2d.drawString("Score: " + score, WIDTH / 2 - 60, HEIGHT / 2 + 40);
            g2d.drawString("Press SPACE to restart", WIDTH / 2 - 130, HEIGHT / 2 + 80);
        } else {
            g2d.setColor(Color.RED);
            g2d.setFont(new Font("Monospaced", Font.BOLD, 40));
            g2d.drawString("GAME OVER", WIDTH / 2 - 110, HEIGHT / 2);
            g2d.setColor(Color.WHITE);
            g2d.setFont(new Font("Monospaced", Font.BOLD, 20));
            g2d.drawString("Score: " + score, WIDTH / 2 - 60, HEIGHT / 2 + 40);
            g2d.drawString("Press SPACE to restart", WIDTH / 2 - 130, HEIGHT / 2 + 80);
        }
    }

    /**
     * {@inheritDoc}
     * <p>The core physics and collision loop. Calculates trajectories, wall 
     * bounces, and brick destruction on every timer pulse.</p>
     */
    @Override
    public void actionPerformed(ActionEvent e) {
        if (!inGame || won) return;

        paddleX += paddleDX;
        if (paddleX < 0) paddleX = 0;
        if (paddleX > WIDTH - paddleWidth) paddleX = WIDTH - paddleWidth;

        ballX += ballDX;
        ballY += ballDY;

        // Walls
        if (ballX - ballRadius < 0) {
            ballDX = Math.abs(ballDX);
            ballX = ballRadius;
        }
        if (ballX + ballRadius > WIDTH) {
            ballDX = -Math.abs(ballDX);
            ballX = WIDTH - ballRadius;
        }
        if (ballY - ballRadius < 0) {
            ballDY = Math.abs(ballDY);
            ballY = ballRadius;
        }
        if (ballY > HEIGHT) {
            inGame = false;
        }

        Rectangle ballRect = new Rectangle(ballX - ballRadius, ballY - ballRadius, ballRadius * 2, ballRadius * 2);
        Rectangle paddleRect = new Rectangle(paddleX, PADDLE_Y, paddleWidth, paddleHeight);
        
        if (ballRect.intersects(paddleRect)) {
            ballDY = -Math.abs(ballDY);
            int hitPoint = ballX - (paddleX + paddleWidth / 2);
            ballDX = hitPoint / 6; 
            if (ballDX == 0) ballDX = (Math.random() > 0.5) ? 1 : -1;
        }

        boolean allDestroyed = true;
        for (int i = 0; i < bricks.length; i++) {
            if (brickActive[i]) {
                allDestroyed = false;
                if (ballRect.intersects(bricks[i])) {
                    brickActive[i] = false;
                    score += 10;
                    
                    if (ballX + ballRadius - ballDX <= bricks[i].x || ballX - ballRadius - ballDX >= bricks[i].x + bricks[i].width) {
                        ballDX = -ballDX;
                    } else {
                        ballDY = -ballDY;
                    }
                    break; 
                }
            }
        }
        
        if (allDestroyed) {
            won = true; 
        }

        repaint();
    }

    /**
     * Launcher method for the Arkanoid game. 
     * <p>Creates a new {@link JFrame} and ensures it is disposed on close 
     * to prevent IDE shutdown when executed in a shared JVM.</p>
     * @param args Command line arguments (ignored).
     */
    public static void main(String[] args) {
        SwingUtilities.invokeLater(() -> {
            JFrame frame = new JFrame("Anahata Arkanoid");
            // Safely dispose to avoid shutting down the whole IDE JVM if executed dynamically
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