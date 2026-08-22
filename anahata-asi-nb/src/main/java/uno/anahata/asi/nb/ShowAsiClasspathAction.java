/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi.nb;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.Arrays;
import java.util.stream.Collectors;
import org.openide.awt.ActionID;
import org.openide.awt.ActionReference;
import org.openide.awt.ActionRegistration;
import org.openide.windows.IOProvider;
import org.openide.windows.InputOutput;
import org.openide.windows.OutputWriter;
import uno.anahata.asi.nb.module.NetBeansModuleUtils;

/**
 * Action to display the Anahata ASI V2 classpath in a NetBeans output tab.
 * Grouped by directory for readability, followed by the full raw string.
 * 
 * @author anahata
 */
@ActionID(category = "Tools", id = "uno.anahata.asi.ShowAsiClasspathAction")
@ActionRegistration(displayName = "Show Default ASI Classpath", iconBase = "icons/anahata_16.png")
@ActionReference(path = "Menu/Tools", position = 11)
public final class ShowAsiClasspathAction implements ActionListener {

    /**
     * {@inheritDoc}
     * <p>
     * Displays the ASI classpath grouped by directory in a dedicated output tab.
     * </p>
     */
    @Override
    public void actionPerformed(ActionEvent e) {
        InputOutput io = IOProvider.getDefault().getIO("Default Anahata ASI Classpath", true);
        io.select();
        try (OutputWriter out = io.getOut()) {
            String cp = NetBeansModuleUtils.getFullAnahataAsiModuleClasspath();
            String[] entries = cp.split(File.pathSeparator);
            
            out.println("-----------------------------------------------------------------------");
            out.println("Default Anahata ASI Classpath Summary");
            out.println("-----------------------------------------------------------------------");
            out.println("Total JARs/Entries: " + entries.length);
            out.println("-----------------------------------------------------------------------");
            
            out.println("\nGrouped by Directory:");
            Arrays.stream(entries)
                .map(File::new)
                .collect(Collectors.groupingBy(f -> {
                    String parent = f.getParent();
                    return parent != null ? parent : "Unknown";
                }))
                .forEach((dir, files) -> {
                    out.println("\nDirectory: " + dir);
                    for (File f : files) {
                        out.println("  - " + f.getName());
                    }
                });

            out.println("\n-----------------------------------------------------------------------");
            out.println("Full Classpath String:");
            out.println("-----------------------------------------------------------------------");
            out.println(cp);
            out.println("-----------------------------------------------------------------------");

            String fxVersion = NetBeansModuleUtils.getJavaFxVersion();
            boolean fxInstalled = NetBeansModuleUtils.isJavaFxModuleInstalled();
            boolean fxEnabled = NetBeansModuleUtils.isJavaFxModuleEnabled();

            out.println("\n-----------------------------------------------------------------------");
            out.println("JavaFX Runtime & Classpath Status");
            out.println("-----------------------------------------------------------------------");
            out.println("Installed in NetBeans: " + (fxInstalled ? "Yes" : "No"));
            out.println("Active / Enabled: " + (fxEnabled ? "Yes" : "No"));
            if (fxVersion != null) {
                out.println("Version: " + fxVersion);
            }
            String fxCp = NetBeansModuleUtils.getJavaFxModuleClasspath();
            if (fxCp != null && !fxCp.isEmpty()) {
                String[] fxEntries = fxCp.split(File.pathSeparator);
                out.println("\nJavaFX Module JARs (" + fxEntries.length + " entries):");
                for (String fxEntry : fxEntries) {
                    out.println("  - " + fxEntry);
                }
                out.println("\nJavaFX Classpath String:\n" + fxCp);
            } else {
                out.println("JavaFX Module Classpath: N/A (Not active)");
            }
            out.println("-----------------------------------------------------------------------");
        }
    }
}
