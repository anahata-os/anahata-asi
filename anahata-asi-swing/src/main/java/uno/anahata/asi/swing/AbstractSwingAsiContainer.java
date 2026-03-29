/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing;

import java.awt.Component;
import java.io.File;
import java.nio.file.Path;
import javax.swing.JFileChooser;
import javax.swing.filechooser.FileNameExtensionFilter;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.AbstractAsiContainer;
import uno.anahata.asi.agi.Agi;

/**
 * A Swing-specific base class for Anahata ASI containers.
 * <p>
 * This class bridges the gap between model-agnostic session logic and the 
 * Swing UI environment. It provides shared utilities for UI-based session 
 * imports and defines the hooks for environment-specific window/tab management.
 * </p>
 * 
 * @author anahata
 */
@Slf4j
public abstract class AbstractSwingAsiContainer extends AbstractAsiContainer {

    /**
     * Constructs a new Swing ASI container.
     * 
     * @param hostApplicationId The unique ID of the host application.
     */
    public AbstractSwingAsiContainer(String hostApplicationId) {
        super(hostApplicationId);
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Delegates the logical 'open' intent to the 
     * environment-specific {@link #focusUI(Agi)} method.
     * </p>
     */
    @Override
    protected void onAgiOpened(Agi agi) {
        focusUI(agi);
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Delegates the logical 'close' intent to the 
     * environment-specific {@link #closeUI(Agi)} method.
     * </p>
     */
    @Override
    protected void onAgiClosed(Agi agi) {
        closeUI(agi);
    }

    /**
     * Environment-specific logic to visually focus or select the UI component 
     * associated with the given session.
     * 
     * @param agi The session to focus.
     */
    protected abstract void focusUI(Agi agi);

    /**
     * Environment-specific logic to visually close or hide the UI component 
     * associated with the given session.
     * 
     * @param agi The session to close.
     */
    protected abstract void closeUI(Agi agi);

    /**
     * Opens a standard Swing {@link JFileChooser} to allow the user to select 
     * a saved session (.kryo) for import.
     * 
     * @param parent The parent component for the dialog.
     */
    public void importSessionWithUI(Component parent) {
        Path savedDir = getSavedSessionsDir();
        JFileChooser chooser = new JFileChooser(savedDir.toFile());
        chooser.setDialogTitle("Import Anahata Session");
        chooser.setFileFilter(new FileNameExtensionFilter("Anahata Sessions (*.kryo)", "kryo"));

        if (chooser.showOpenDialog(parent) == JFileChooser.APPROVE_OPTION) {
            File selectedFile = chooser.getSelectedFile();
            log.info("User selected file for import: {}", selectedFile);
            Agi imported = importSession(selectedFile.toPath());
            if (imported != null) {
                open(imported);
            }
        }
    }
}
