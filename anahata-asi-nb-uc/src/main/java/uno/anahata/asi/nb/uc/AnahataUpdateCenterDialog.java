/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.uc;

import java.awt.Dimension;
import java.awt.Frame;
import java.awt.Image;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import javax.swing.JFrame;
import javax.swing.SwingUtilities;
import org.openide.util.ImageUtilities;
import org.openide.windows.WindowManager;

/**
 * Top-level window frame controller for the Anahata ASI NetBeans Update Center.
 * <p>
 * Displays a dedicated, non-blocking {@link JFrame} hosting the {@link AnahataUpdateCenterPanel}.
 * Automatically centers on the primary NetBeans IDE window and re-focuses if already open.
 * </p>
 *
 * @author anahata
 */
public final class AnahataUpdateCenterDialog {

    /**
     * Singleton instance of the update center JFrame.
     */
    private static JFrame frameInstance;

    /**
     * Private constructor to prevent direct instantiation.
     */
    private AnahataUpdateCenterDialog() {
    }

    /**
     * Displays or brings to front the Anahata ASI Update Center JFrame window.
     * <p>
     * Ensures execution takes place on the Swing Event Dispatch Thread (EDT).
     * </p>
     */
    public static void showDialog() {
        if (!SwingUtilities.isEventDispatchThread()) {
            SwingUtilities.invokeLater(AnahataUpdateCenterDialog::showDialog);
            return;
        }

        if (frameInstance == null) {
            frameInstance = new JFrame("Anahata ASI Update Center");
            
            // Set window icon
            Image icon = ImageUtilities.loadImage("icons/anahata_16.png");
            if (icon != null) {
                frameInstance.setIconImage(icon);
            }

            AnahataUpdateCenterPanel panel = new AnahataUpdateCenterPanel();
            frameInstance.setContentPane(panel);
            frameInstance.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
            frameInstance.setMinimumSize(new Dimension(840, 620));
            frameInstance.setPreferredSize(new Dimension(940, 750));
            frameInstance.pack();

            Frame mainWin = WindowManager.getDefault().getMainWindow();
            if (mainWin != null && mainWin.isShowing()) {
                frameInstance.setLocationRelativeTo(mainWin);
            } else {
                frameInstance.setLocationRelativeTo(null);
            }

            frameInstance.addWindowListener(new WindowAdapter() {
                @Override
                public void windowClosed(WindowEvent e) {
                    frameInstance = null;
                }
            });
        }

        if (!frameInstance.isVisible()) {
            frameInstance.setVisible(true);
        }

        frameInstance.toFront();
        frameInstance.requestFocus();
    }
}
