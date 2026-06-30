/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.intellij.tools.project.context;

import com.intellij.openapi.project.Project;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.message.RagMessage;
import uno.anahata.asi.intellij.tools.project.Projects;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.stream.Stream;

/**
 * Provides a unified view of a project's physical file structure.
 * 
 * @author anahata
 */
@Slf4j
public class ProjectStructureContextProvider extends AbstractProjectContextProvider {

    /**
     * Constructs a new project structure context provider.
     * 
     * @param projectsToolkit The parent Projects toolkit.
     * @param projectPath The absolute path to the project.
     */
    public ProjectStructureContextProvider(Projects projectsToolkit, String projectPath) {
        super("structure", "Structure", "Unified physical project map", projectsToolkit, projectPath);
    }

    @Override
    public void populateMessage(RagMessage ragMessage) throws Exception {
        Project p = getProject();
        if (p == null) {
            ragMessage.addTextPart("Couldn't fetch IntelliJ Project, project may have been closed");
            return;
        }

        StringBuilder sb = new StringBuilder();
        sb.append("  ## Project Structure: ").append(p.getName()).append("\n\n");
        sb.append("  ### Directory Tree\n");
        
        try {
            appendDirectoryStructure(Path.of(projectPath), sb, "    ");
        } catch (Exception e) {
            sb.append("    * [Error loading structure: ").append(e.getMessage()).append("]\n");
        }
        
        ragMessage.addTextPart(sb.toString());
    }

    private void appendDirectoryStructure(Path dir, StringBuilder sb, String indent) throws IOException {
        try (Stream<Path> stream = Files.list(dir)) {
            List<Path> paths = stream
                .filter(p -> {
                    String name = p.getFileName().toString();
                    return !name.startsWith(".") && !name.equals("target") && !name.equals("out");
                })
                .sorted((p1, p2) -> {
                    boolean d1 = Files.isDirectory(p1);
                    boolean d2 = Files.isDirectory(p2);
                    if (d1 != d2) {
                        return d1 ? -1 : 1;
                    }
                    return p1.compareTo(p2);
                })
                .toList();

            for (Path path : paths) {
                String name = path.getFileName().toString();
                if (Files.isDirectory(path)) {
                    sb.append(indent).append("- 📂 `").append(name).append("/`\n");
                    if (indent.length() < 12) {
                        appendDirectoryStructure(path, sb, indent + "  ");
                    }
                } else {
                    long size = Files.size(path);
                    sb.append(indent).append("- 📄 `").append(name).append("` [")
                      .append(String.format("%.1f KB", size / 1024.0)).append("]\n");
                }
            }
        }
    }
}