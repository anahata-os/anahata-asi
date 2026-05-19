/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.games;

import java.awt.*;
import java.awt.event.*;
import java.util.Random;
import javax.swing.*;
import javax.swing.Timer;

/**
 * Anahata Atoms Tetris - A high-fidelity implementation of the classic puzzle game.
 * <p>
 * This game uses programmatically drawn "Atoms" as blocks, providing a crisp, 
 * vector-like aesthetic. It features responsive keyboard controls, score tracking, 
 * and a smooth collision engine.
 * </p>
 * 
 * @author anahata
 */
public class Tetris extends JPanel implements ActionListener {

    private final int BOARD_WIDTH = 10;
    private final int BOARD_HEIGHT = 22;
    private final int TILE_SIZE = 30;
    private final Timer timer;
    private boolean isFallingFinished = false;
    private boolean isStarted = false;
    private boolean isPaused = false;
    private int numLinesRemoved = 0;
    private int curX = 0;
    private int curY = 0;
    private Shape curPiece;
    private final Color[] board;

    /**
     * Constructs a new Tetris game panel and initializes the environment.
     */
    public Tetris() {
        setPreferredSize(new Dimension(BOARD_WIDTH * TILE_SIZE, BOARD_HEIGHT * TILE_SIZE));
        setBackground(new Color(20, 20, 25));
        setFocusable(true);
        curPiece = new Shape();
        timer = new Timer(400, this);
        board = new Color[BOARD_WIDTH * BOARD_HEIGHT];
        clearBoard();
        addKeyListener(new TAdapter());
        start();
    }

    /**
     * Resets the game state and starts the animation timer.
     */
    public final void start() {
        isStarted = true;
        isFallingFinished = false;
        numLinesRemoved = 0;
        clearBoard();
        newPiece();
        timer.start();
    }

    private void pause() {
        isPaused = !isPaused;
        if (isPaused) {
            timer.stop();
        } else {
            timer.start();
        }
        repaint();
    }

    /**
     * {@inheritDoc}
     * <p>Renders the static board and the currently falling piece using 
     * shining Atom primitives.</p>
     */
    @Override
    public void paintComponent(Graphics g) {
        super.paintComponent(g);
        Graphics2D g2d = (Graphics2D) g;
        g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

        Dimension size = getSize();
        int boardTop = (int) size.getHeight() - BOARD_HEIGHT * TILE_SIZE;

        for (int i = 0; i < BOARD_HEIGHT; i++) {
            for (int j = 0; j < BOARD_WIDTH; j++) {
                Color color = shapeAt(j, BOARD_HEIGHT - i - 1);
                if (color != null) {
                    drawAtom(g2d, j * TILE_SIZE, boardTop + i * TILE_SIZE, color);
                }
            }
        }

        if (curPiece.getShape() != Tetrominoes.NoShape) {
            for (int i = 0; i < 4; i++) {
                int x = curX + curPiece.x(i);
                int y = curY - curPiece.y(i);
                drawAtom(g2d, x * TILE_SIZE, boardTop + (BOARD_HEIGHT - y - 1) * TILE_SIZE, curPiece.getColor());
            }
        }
        
        // HUD
        g2d.setColor(Color.WHITE);
        g2d.setFont(new Font("Monospaced", Font.BOLD, 14));
        g2d.drawString("Lines: " + numLinesRemoved, 10, 20);
        if (isPaused) g2d.drawString("PAUSED", 10, 40);
    }

    private void drawAtom(Graphics2D g2d, int x, int y, Color color) {
        g2d.setColor(color);
        g2d.fillOval(x + 2, y + 2, TILE_SIZE - 4, TILE_SIZE - 4);
        g2d.setColor(color.brighter());
        g2d.setStroke(new BasicStroke(2f));
        g2d.drawOval(x + 2, y + 2, TILE_SIZE - 4, TILE_SIZE - 4);
        // Shine
        g2d.setColor(new Color(255, 255, 255, 100));
        g2d.fillOval(x + 8, y + 8, 6, 6);
    }

    private void dropDown() {
        int newY = curY;
        while (newY > 0) {
            if (!tryMove(curPiece, curX, newY - 1)) break;
            newY--;
        }
        pieceDropped();
    }

    private void oneLineDown() {
        if (!tryMove(curPiece, curX, curY - 1)) {
            pieceDropped();
        }
    }

    private void clearBoard() {
        for (int i = 0; i < BOARD_HEIGHT * BOARD_WIDTH; i++) {
            board[i] = null;
        }
    }

    private void pieceDropped() {
        for (int i = 0; i < 4; i++) {
            int x = curX + curPiece.x(i);
            int y = curY - curPiece.y(i);
            board[(y * BOARD_WIDTH) + x] = curPiece.getColor();
        }
        removeFullLines();
        if (!isFallingFinished) newPiece();
    }

    private void newPiece() {
        curPiece.setRandomShape();
        curX = BOARD_WIDTH / 2 + 1;
        curY = BOARD_HEIGHT - 1 + curPiece.minY();
        if (!tryMove(curPiece, curX, curY)) {
            curPiece.setShape(Tetrominoes.NoShape);
            timer.stop();
            isStarted = false;
            JOptionPane.showMessageDialog(this, "GAME OVER\nLines: " + numLinesRemoved);
        }
    }

    private boolean tryMove(Shape newPiece, int newX, int newY) {
        for (int i = 0; i < 4; i++) {
            int x = newX + newPiece.x(i);
            int y = newY - newPiece.y(i);
            if (x < 0 || x >= BOARD_WIDTH || y < 0 || y >= BOARD_HEIGHT) return false;
            if (shapeAt(x, y) != null) return false;
        }
        curPiece = newPiece;
        curX = newX;
        curY = newY;
        repaint();
        return true;
    }

    private void removeFullLines() {
        int numFullLines = 0;
        for (int i = BOARD_HEIGHT - 1; i >= 0; i--) {
            boolean lineIsFull = true;
            for (int j = 0; j < BOARD_WIDTH; j++) {
                if (shapeAt(j, i) == null) {
                    lineIsFull = false;
                    break;
                }
            }
            if (lineIsFull) {
                numFullLines++;
                for (int k = i; k < BOARD_HEIGHT - 1; k++) {
                    for (int j = 0; j < BOARD_WIDTH; j++) {
                        board[(k * BOARD_WIDTH) + j] = shapeAt(j, k + 1);
                    }
                }
            }
        }
        if (numFullLines > 0) {
            numLinesRemoved += numFullLines;
            isFallingFinished = true;
            curPiece.setShape(Tetrominoes.NoShape);
            repaint();
        }
    }

    private Color shapeAt(int x, int y) { return board[(y * BOARD_WIDTH) + x]; }

    @Override
    public void actionPerformed(ActionEvent e) {
        if (isFallingFinished) {
            isFallingFinished = false;
            newPiece();
        } else {
            oneLineDown();
        }
    }

    enum Tetrominoes { NoShape, ZShape, SShape, LineShape, TShape, SquareShape, LShape, MirroredLShape }

    static class Shape {
        private Tetrominoes pieceShape;
        private final int[][] coords;
        private static final int[][][] coordsTable = new int[][][] {
            { { 0, 0 },   { 0, 0 },   { 0, 0 },   { 0, 0 } },
            { { 0, -1 },  { 0, 0 },   { -1, 0 },  { -1, 1 } },
            { { 0, -1 },  { 0, 0 },   { 1, 0 },   { 1, 1 } },
            { { 0, -1 },  { 0, 0 },   { 0, 1 },   { 0, 2 } },
            { { -1, 0 },  { 0, 0 },   { 1, 0 },   { 0, 1 } },
            { { 0, 0 },   { 1, 0 },   { 0, 1 },   { 1, 1 } },
            { { -1, -1 }, { 0, -1 },  { 0, 0 },   { 0, 1 } },
            { { 1, -1 },  { 0, -1 },  { 0, 0 },   { 0, 1 } }
        };

        public Shape() { coords = new int[4][2]; setShape(Tetrominoes.NoShape); }

        public void setShape(Tetrominoes shape) {
            for (int i = 0; i < 4 ; i++) {
                System.arraycopy(coordsTable[shape.ordinal()][i], 0, coords[i], 0, 2);
            }
            pieceShape = shape;
        }

        public void setRandomShape() {
            Random r = new Random();
            int x = Math.abs(r.nextInt()) % 7 + 1;
            Tetrominoes[] values = Tetrominoes.values();
            setShape(values[x]);
        }

        public Color getColor() {
            return switch (pieceShape) {
                case ZShape -> new Color(220, 53, 69);
                case SShape -> new Color(40, 167, 69);
                case LineShape -> new Color(0, 123, 255);
                case TShape -> new Color(111, 66, 193);
                case SquareShape -> new Color(255, 193, 7);
                case LShape -> new Color(253, 126, 20);
                case MirroredLShape -> new Color(32, 201, 151);
                default -> Color.GRAY;
            };
        }

        public int x(int index) { return coords[index][0]; }
        public int y(int index) { return coords[index][1]; }
        public Tetrominoes getShape() { return pieceShape; }
        public int minY() {
            int m = coords[0][1];
            for (int i=0; i<4; i++) m = Math.min(m, coords[i][1]);
            return m;
        }

        public Shape rotateLeft() {
            if (pieceShape == Tetrominoes.SquareShape) return this;
            Shape result = new Shape();
            result.pieceShape = pieceShape;
            for (int i = 0; i < 4; ++i) {
                result.coords[i][0] = y(i);
                result.coords[i][1] = -x(i);
            }
            return result;
        }
    }

    class TAdapter extends KeyAdapter {
        @Override
        public void keyPressed(KeyEvent e) {
            if (!isStarted || curPiece.getShape() == Tetrominoes.NoShape) return;
            int keycode = e.getKeyCode();
            if (keycode == 'p' || keycode == 'P') { pause(); return; }
            if (isPaused) return;
            switch (keycode) {
                case KeyEvent.VK_LEFT -> tryMove(curPiece, curX - 1, curY);
                case KeyEvent.VK_RIGHT -> tryMove(curPiece, curX + 1, curY);
                case KeyEvent.VK_UP -> tryMove(curPiece.rotateLeft(), curX, curY);
                case KeyEvent.VK_SPACE -> dropDown();
                case KeyEvent.VK_DOWN -> oneLineDown();
            }
        }
    }

    /**
     * Launcher for the Tetris game.
     * @param args command line arguments (ignored).
     */
    public static void main(String[] args) {
        SwingUtilities.invokeLater(() -> {
            JFrame frame = new JFrame("Anahata Atoms Tetris");
            frame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
            frame.add(new Tetris());
            frame.pack();
            frame.setLocationRelativeTo(null);
            frame.setResizable(false);
            frame.setVisible(true);
        });
    }
}
