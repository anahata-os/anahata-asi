/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.openide.modules.ModuleInstall;
import org.openide.windows.TopComponent;
import org.openide.windows.WindowManager;
import uno.anahata.asi.nb.util.ElementHandleModule;
import uno.anahata.asi.tool.schema.SchemaProvider;

/**
 * Installer for the Anahata ASI V2 module.
 * Handles lifecycle management and session handoff during reloads.
 */
public class AnahataInstaller extends ModuleInstall {

    private static final Logger log = Logger.getLogger(AnahataInstaller.class.getName());
    private static final String HANDOFF_FILE_NAME = "asi-reload-handoff.dat";
    
    private static AsiContainer asiConfig;

    public static synchronized AsiContainer getAsiConfig() {
        if (asiConfig == null) {
            asiConfig = new NetBeansAsiContainer();
        }
        return asiConfig;
    }

    @Override
    public void restored() {
        log.info("Anahata ASI V2 Module Restored");
        
        // Register the ElementHandle module for global JSON support in the IDE
        SchemaProvider.OBJECT_MAPPER.registerModule(new ElementHandleModule());
        
        File handoffFile = getHandoffFile();
        if (handoffFile.exists()) {
            log.info("Handoff file found. Restoring sessions.");
            try (ObjectInputStream ois = new ObjectInputStream(new FileInputStream(handoffFile))) {
                List<String> sessionIds = (List<String>) ois.readObject();
                for (String id : sessionIds) {
                    AgiTopComponent tc = new AgiTopComponent();
                    tc.setSessionIdForHandoff(id);
                    tc.open();
                    tc.requestActive();
                }
            } catch (IOException | ClassNotFoundException e) {
                log.log(Level.SEVERE, "Failed to restore sessions", e);
            } finally {
                handoffFile.delete();
            }
        }
    }

    @Override
    public void uninstalled() {
        log.info("Anahata ASI V2 Module Uninstalled");
        
        List<String> sessionIds = new ArrayList<>();
        Set<TopComponent> openTcs = WindowManager.getDefault().getRegistry().getOpened();

        for (TopComponent tc : openTcs) {
            if (tc instanceof AgiTopComponent) {
                AgiTopComponent atc = (AgiTopComponent) tc;
                if (atc.getChat() != null) {
                    sessionIds.add(atc.getChat().getConfig().getSessionId());
                }
            }
        }

        if (!sessionIds.isEmpty()) {
            File handoffFile = getHandoffFile();
            try (ObjectOutputStream oos = new ObjectOutputStream(new FileOutputStream(handoffFile))) {
                oos.writeObject(sessionIds);
            } catch (IOException e) {
                log.log(Level.SEVERE, "Failed to write handoff file", e);
            }
        }
        
        // Close components to allow clean reload
        for (TopComponent tc : openTcs) {
            if (tc instanceof AgiTopComponent || tc instanceof AsiTopComponent) {
                tc.close();
            }
        }
    }

    private File getHandoffFile() {
        
        return new File(getAsiConfig().getAppDir().toFile(), HANDOFF_FILE_NAME);
    }
}
