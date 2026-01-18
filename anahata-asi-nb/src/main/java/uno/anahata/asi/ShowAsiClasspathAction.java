/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi;

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
@ActionRegistration(displayName = "Show ASI Classpath", iconBase = "icons/anahata_16.png")
@ActionReference(path = "Menu/Tools", position = 11)
public final class ShowAsiClasspathAction implements ActionListener {

    @Override
    public void actionPerformed(ActionEvent e) {
        InputOutput io = IOProvider.getDefault().getIO("Anahata ASI Classpath", true);
        io.select();
        try (OutputWriter out = io.getOut()) {
            String cp = NetBeansModuleUtils.getNetBeansClasspath();
            String[] entries = cp.split(File.pathSeparator);
            
            out.println("-----------------------------------------------------------------------");
            out.println("Anahata ASI Classpath Summary");
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
        }
    }
}
