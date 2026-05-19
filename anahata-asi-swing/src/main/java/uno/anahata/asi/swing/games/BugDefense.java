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
    private Timer timer;
    private int shieldX = 225;
    private final int shieldY = 540;
    private final int shieldWidth = 50;
    private final int shieldHeight = 20;
    
    private boolean playing = false;
    private int score = 0;
    private int lives = 3;
    
    private java.util.List<Rectangle> optimizationPulses = new ArrayList<>();
    private java.util.List<Rectangle> bugs = new ArrayList<>();
    private final Random rand = new Random();
    private int bugSpawnTick = 0;

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

    private void setupKeyBindings() {
        InputMap im = getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW);
        ActionMap am = getActionMap();
        im.put(KeyStroke.getKeyStroke("LEFT"), "left");
        im.put(KeyStroke.getKeyStroke("RIGHT"), "right");
        im.put(KeyStroke.getKeyStroke("SPACE"), "shoot");
        am.put("left", new AbstractAction() { public void actionPerformed(ActionEvent e) { if (playing) shieldX = Math.max(0, shieldX - 20); } });
        am.put("right", new AbstractAction() { public void actionPerformed(ActionEvent e) { if (playing) shieldX = Math.min(getWidth() - shieldWidth, shieldX + 20); } });
        am.put("shoot", new AbstractAction() { public void actionPerformed(ActionEvent e) { if (playing) spawnPulse(); } });
    }

    private void startGame() {
        playing = true;
        score = 0;
        lives = 3;
        bugs.clear();
        optimizationPulses.clear();
        timer = new Timer(20, this);
        timer.start();
    }

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
                    JOptionPane.showMessageDialog(this, "Production environment crashed!\nFinal Score: " + score);
                }
            }
        }
        checkCollisions();
        repaint();
    }

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

    public static void main(String[] args) {
        JFrame frame = new JFrame("Anahata Bug Defense");
        frame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
        frame.add(new BugDefense());
        frame.pack();
        frame.setLocationRelativeTo(null);
        frame.setVisible(true);
    }
}
