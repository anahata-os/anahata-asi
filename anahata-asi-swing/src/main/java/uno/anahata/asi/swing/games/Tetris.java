/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.games;

import java.awt.*;
import java.awt.event.*;
import java.util.Random;
import javax.swing.*;
import javax.swing.Timer;

/**
 * Anahata Atoms Tetris - A high-fidelity implementation of the classic puzzle
 * game.
 * <p>
 * This game uses programmatically drawn "Atoms" as blocks, providing a crisp,
 * vector-like aesthetic. It features responsive keyboard controls, score
 * tracking, and a smooth collision engine.
 * </p>
 *
 * @author anahata
 */
public class Tetris extends JPanel implements ActionListener {

    /**
     * The grid width of the Tetris play board (10 columns).
     */
    private final int BOARD_WIDTH = 10;
    /**
     * The grid height of the Tetris play board (22 rows).
     */
    private final int BOARD_HEIGHT = 22;
    /**
     * The square dimension of each cell in pixels.
     */
    private final int TILE_SIZE = 30;
    /**
     * Central animation and speed controller tick timer.
     */
    private final Timer timer;
    /**
     * Flag signaling that the current piece has landed and hit-testing is
     * complete.
     */
    private boolean isFallingFinished = false;
    /**
     * Flag signaling whether the current game is active.
     */
    private boolean isStarted = false;
    /**
     * Flag signaling whether game execution is paused.
     */
    private boolean isPaused = false;
    /**
     * Total count of successfully completed rows cleared during the session.
     */
    private int numLinesRemoved = 0;
    /**
     * The active horizontal grid position of the falling piece.
     */
    private int curX = 0;
    /**
     * The active vertical grid position of the falling piece.
     */
    private int curY = 0;
    /**
     * The currently falling geometric block structure.
     */
    private Shape curPiece;
    /**
     * One-dimensional array matching grid coordinates to static landed block
     * colors.
     */
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

    /**
     * Toggles active gameplay state between paused and running.
     */
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
     * <p>
     * Renders the static board and the currently falling piece using shining
     * Atom primitives.</p>
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
        if (isPaused) {
            g2d.drawString("PAUSED", 10, 40);
        }
    }

    /**
     * Renders a high-fidelity glossy vector atom bubble at coordinates.
     *
     * @param y Visual Y coordinate.
     * @param color The design fill color of the atom.
     * @param g2d The Graphics context.
     * @param x Visual X coordinate.
     */
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

    /**
     * Instantly drops the current piece to the lowest possible free grid row.
     */
    private void dropDown() {
        int newY = curY;
        while (newY > 0) {
            if (!tryMove(curPiece, curX, newY - 1)) {
                break;
            }
            newY--;
        }
        pieceDropped();
    }

    /**
     * Moves the falling piece one step downwards if free space exists.
     */
    private void oneLineDown() {
        if (!tryMove(curPiece, curX, curY - 1)) {
            pieceDropped();
        }
    }

    /**
     * Resets the active game board grid by filling all indices with null.
     */
    private void clearBoard() {
        for (int i = 0; i < BOARD_HEIGHT * BOARD_WIDTH; i++) {
            board[i] = null;
        }
    }

    /**
     * Lamps the falling piece onto the static board, checks cleared rows, and
     * spawns the next piece.
     */
    private void pieceDropped() {
        for (int i = 0; i < 4; i++) {
            int x = curX + curPiece.x(i);
            int y = curY - curPiece.y(i);
            board[(y * BOARD_WIDTH) + x] = curPiece.getColor();
        }
        removeFullLines();
        if (!isFallingFinished) {
            newPiece();
        }
    }

    /**
     * Spawns a new random block, setting initial positions and terminating on
     * overflow.
     */
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

    /**
     * Evaluates prospective coordinates for boundary limits and existing
     * blocks, committing state on success.
     *
     * @param newPiece The shape to move or rotate.
     * @param newY Candidate Y coordinate.
     * @param newX Candidate X coordinate.
     * @return true if the move is legal and committed.
     */
    private boolean tryMove(Shape newPiece, int newX, int newY) {
        for (int i = 0; i < 4; i++) {
            int x = newX + newPiece.x(i);
            int y = newY - newPiece.y(i);
            if (x < 0 || x >= BOARD_WIDTH || y < 0 || y >= BOARD_HEIGHT) {
                return false;
            }
            if (shapeAt(x, y) != null) {
                return false;
            }
        }
        curPiece = newPiece;
        curX = newX;
        curY = newY;
        repaint();
        return true;
    }

    /**
     * Scans, identifies, clears, and shifts completed rows downwards with score
     * aggregation.
     */
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

    /**
     * Retrieves the color block at matching column and row coordinates.
     *
     * @param x Grid column index.
     * @param y Grid row index.
     * @return Color of the cell or null if empty.
     */
    private Color shapeAt(int x, int y) {
        return board[(y * BOARD_WIDTH) + x];
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        if (isFallingFinished) {
            isFallingFinished = false;
            newPiece();
        } else {
            oneLineDown();
        }
    }

    /**
     * Enumeration of all standard tetromino geometry variations.
     */
    enum Tetrominoes {
                /**
                 * Represents an empty grid cell or the absence of a falling piece.
                 */
                NoShape,
                /**
                 * The Z-shaped red tetromino block.
                 */
                ZShape,
                /**
                 * The S-shaped green tetromino block.
                 */
                SShape,
                /**
                 * The straight 1x4 light-blue line tetromino block.
                 */
                LineShape,
                /**
                 * The T-shaped purple tetromino block.
                 */
                TShape,
                /**
                 * The 2x2 square yellow tetromino block.
                 */
                SquareShape,
                /**
                 * The L-shaped orange tetromino block.
                 */
                LShape,
                /**
                 * The mirrored L-shaped green-blue tetromino block.
                 */
                MirroredLShape
    }

    /**
     * Represents the layout and operations of a single Tetris block geometry.
     */
    static class Shape {

        /**
         * The category of Tetrominoes shape this instance represents.
         */
        private Tetrominoes pieceShape;
        /**
         * The relative coordinates grid of the four child nodes.
         */
        private final int[][] coords;
        /**
         * Global constant mapping ordinal types to physical coordinate
         * patterns.
         */
        private static final int[][][] coordsTable = new int[][][]{
            {{0, 0}, {0, 0}, {0, 0}, {0, 0}},
            {{0, -1}, {0, 0}, {-1, 0}, {-1, 1}},
            {{0, -1}, {0, 0}, {1, 0}, {1, 1}},
            {{0, -1}, {0, 0}, {0, 1}, {0, 2}},
            {{-1, 0}, {0, 0}, {1, 0}, {0, 1}},
            {{0, 0}, {1, 0}, {0, 1}, {1, 1}},
            {{-1, -1}, {0, -1}, {0, 0}, {0, 1}},
            {{1, -1}, {0, -1}, {0, 0}, {0, 1}}
        };

        /**
         * Constructs a new blank shape initialized to NoShape.
         */
        public Shape() {
            coords = new int[4][2];
            setShape(Tetrominoes.NoShape);
        }

        /**
         * Sets the geometric shape and initializes relative coordinate offsets
         * from coordsTable.
         *
         * @param shape The Tetrominoes category of the shape.
         */
        public void setShape(Tetrominoes shape) {
            for (int i = 0; i < 4; i++) {
                System.arraycopy(coordsTable[shape.ordinal()][i], 0, coords[i], 0, 2);
            }
            pieceShape = shape;
        }

        /**
         * Randomly assigns a tetromino shape excluding NoShape.
         */
        public void setRandomShape() {
                        Random r = new Random();
                        int x = r.nextInt(7) + 1;
                        Tetrominoes[] values = Tetrominoes.values();
                        setShape(values[x]);
        }

        /**
         * Translates active tetromino types to high-salience design colors.
         *
         * @return The design color of this block.
         */
        public Color getColor() {
            return switch (pieceShape) {
                case ZShape ->
                    new Color(220, 53, 69);
                case SShape ->
                    new Color(40, 167, 69);
                case LineShape ->
                    new Color(0, 123, 255);
                case TShape ->
                    new Color(111, 66, 193);
                case SquareShape ->
                    new Color(255, 193, 7);
                case LShape ->
                    new Color(253, 126, 20);
                case MirroredLShape ->
                    new Color(32, 201, 151);
                default ->
                    Color.GRAY;
            };
        }

        /**
         * Retrieves the relative X coordinate offset for the child segment at
         * the specified index.
         *
         * @param index The 0-based index of the child block.
         * @return relative grid X coordinate offset.
         */
        public int x(int index) {
            return coords[index][0];
        }

        /**
         * Retrieves the relative Y coordinate offset for the child segment at
         * the specified index.
         *
         * @param index The 0-based index of the child block.
         * @return relative grid Y coordinate offset.
         */
        public int y(int index) {
            return coords[index][1];
        }

        /**
         * Accessor for the underlying tetromino category.
         *
         * @return The active Tetrominoes ordinal value.
         */
        public Tetrominoes getShape() {
            return pieceShape;
        }

        /**
         * Calculates the lowest Y coordinate among all active segment blocks.
         *
         * @return the minimum Y offset.
         */
        public int minY() {
            int m = coords[0][1];
            for (int i = 0; i < 4; i++) {
                m = Math.min(m, coords[i][1]);
            }
            return m;
        }

        /**
         * Performs a left-hand 90-degree vector rotation on the shape block
         * coordinates.
         *
         * @return a new rotated Shape instance.
         */
        public Shape rotateLeft() {
            if (pieceShape == Tetrominoes.SquareShape) {
                return this;
            }
            Shape result = new Shape();
            result.pieceShape = pieceShape;
            for (int i = 0; i < 4; ++i) {
                result.coords[i][0] = y(i);
                result.coords[i][1] = -x(i);
            }
            return result;
        }
    }

    /**
     * Custom keyboard listener to capture action keystrokes on the Event
     * Dispatch Thread.
     */
    class TAdapter extends KeyAdapter {

        @Override
        public void keyPressed(KeyEvent e) {
            if (!isStarted || curPiece.getShape() == Tetrominoes.NoShape) {
                return;
            }
            int keycode = e.getKeyCode();
            if (keycode == 'p' || keycode == 'P') {
                pause();
                return;
            }
            if (isPaused) {
                return;
            }
            switch (keycode) {
                case KeyEvent.VK_LEFT ->
                    tryMove(curPiece, curX - 1, curY);
                case KeyEvent.VK_RIGHT ->
                    tryMove(curPiece, curX + 1, curY);
                case KeyEvent.VK_UP ->
                    tryMove(curPiece.rotateLeft(), curX, curY);
                case KeyEvent.VK_SPACE ->
                    dropDown();
                case KeyEvent.VK_DOWN ->
                    oneLineDown();
            }
        }
    }

    /**
     * Launcher for the Tetris game.
     *
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
