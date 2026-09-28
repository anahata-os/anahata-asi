/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.intellij.tools.project.context;

import com.intellij.openapi.application.ReadAction;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.roots.ModuleRootManager;
import com.intellij.openapi.roots.ProjectFileIndex;
import com.intellij.openapi.roots.ProjectRootManager;
import com.intellij.openapi.vcs.FileStatus;
import com.intellij.openapi.vcs.FileStatusManager;
import com.intellij.openapi.vfs.VfsUtilCore;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiClass;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiJavaFile;
import com.intellij.psi.PsiManager;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.message.RagMessage;
import uno.anahata.asi.intellij.tools.project.Projects;
import uno.anahata.asi.toolkit.project.ProjectStructureScope;

/**
 * Provides a source-root-aware map of a project's Java structure.
 * <p>
 * Unlike a raw filesystem walk, this provider uses the IntelliJ project model
 * ({@link ProjectRootManager#getContentSourceRoots()} + {@link ProjectFileIndex}) so the
 * ASI sees the logical layout the IDE sees: each configured source root, labelled as
 * production or test, with its package/type tree. All model and VFS access runs inside a
 * {@link ReadAction} because context is assembled on a background thread.
 * </p>
 *
 * @author anahata
 */
@Slf4j
public class ProjectStructureContextProvider extends AbstractProjectContextProvider {

    private transient Module module;
    private final String moduleName;

    @Getter @Setter
    private ProjectStructureScope scope;

    /**
     * Constructs a new project structure context provider.
     *
     * @param projectsToolkit The parent Projects toolkit.
     * @param projectPath     The absolute path to the project.
     */
    public ProjectStructureContextProvider(Projects projectsToolkit, String projectPath) {
        this(projectsToolkit, projectPath, null, new ProjectStructureScope());
    }

    /**
     * Constructs a new structure context provider scoped to a specific module or project.
     *
     * @param projectsToolkit The parent Projects toolkit.
     * @param projectPath     The absolute path to the project or module directory.
     * @param module          The module to scope to, or null for project-wide.
     * @param scope           The granularity scope settings.
     */
    public ProjectStructureContextProvider(Projects projectsToolkit, String projectPath, Module module, ProjectStructureScope scope) {
        super("structure", "Structure", "Source-root-aware project type map", projectsToolkit, projectPath);
        this.module = module;
        this.moduleName = module != null ? module.getName() : null;
        if (module != null && !module.isDisposed()) {
            this.project = module.getProject();
        }
        this.scope = scope != null ? scope : new ProjectStructureScope();
    }

    /**
     * Resolves the active IntelliJ Module instance, restoring it from name if needed.
     * 
     * @return The active Module, or null if unconfigured or unloaded.
     */
    public Module getModule() {
        if (module != null && !module.isDisposed()) {
            return module;
        }
        if (moduleName != null) {
            Project p = getProject();
            if (p != null && !p.isDisposed()) {
                module = ModuleManager.getInstance(p).findModuleByName(moduleName);
            }
        }
        return module;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Renders each content source root (flagged main/test) and its package/type tree,
     * built from the IntelliJ project model inside a read action.
     * </p>
     */
    @Override
    public void populateMessage(RagMessage ragMessage) throws Exception {
        Project p = getProject();
        if (p == null) {
            ragMessage.addTextPart("Couldn't fetch IntelliJ Project, project may have been closed");
            return;
        }

        Module m = getModule();
        String markdown = ReadAction.computeBlocking(() -> {
            StringBuilder sb = new StringBuilder();
            if (m != null) {
                sb.append("  ## Module Structure: ").append(m.getName()).append("\n\n");
            } else {
                sb.append("  ## Project Structure: ").append(p.getName()).append("\n\n");
            }

            VirtualFile rootDir;
            VirtualFile[] sourceRoots;
            ProjectFileIndex fileIndex = ProjectRootManager.getInstance(p).getFileIndex();

            if (m != null) {
                ModuleRootManager rm = ModuleRootManager.getInstance(m);
                sourceRoots = rm.getSourceRoots();
                VirtualFile[] cr = rm.getContentRoots();
                rootDir = (cr.length > 0) ? cr[0] : null;
            } else {
                ProjectRootManager rm = ProjectRootManager.getInstance(p);
                sourceRoots = rm.getContentSourceRoots();
                VirtualFile[] cr = rm.getContentRoots();
                rootDir = (cr.length > 0) ? cr[0] : null;
            }

            if (scope.isShowRootFiles() && rootDir != null && rootDir.exists()) {
                appendRootFiles(rootDir, p, sb, "  ");
            }

            boolean hasSubmodules = ModuleManager.getInstance(p).getModules().length > 1;
            List<VirtualFile> submoduleRoots = new ArrayList<>();
            if (m == null && hasSubmodules) {
                for (Module mod : ModuleManager.getInstance(p).getModules()) {
                    String modPath = ModuleContextProvider.resolveModulePath(p, mod);
                    if (!modPath.equals(projectPath) && !mod.getName().equals(p.getName())) {
                        for (VirtualFile crRoot : ModuleRootManager.getInstance(mod).getContentRoots()) {
                            submoduleRoots.add(crRoot);
                        }
                    }
                }
            }

            List<VirtualFile> effectiveSourceRoots = new ArrayList<>();
            if (sourceRoots != null) {
                for (VirtualFile root : sourceRoots) {
                    if (m == null && hasSubmodules) {
                        boolean belongsToSubmodule = false;
                        for (VirtualFile subRoot : submoduleRoots) {
                            if (VfsUtilCore.isAncestor(subRoot, root, false)) {
                                belongsToSubmodule = true;
                                break;
                            }
                        }
                        if (belongsToSubmodule) {
                            continue;
                        }
                    }
                    effectiveSourceRoots.add(root);
                }
            }

            if (effectiveSourceRoots.isEmpty()) {
                if (m == null && hasSubmodules) {
                    sb.append("  - Source roots are organized under child modules.\n");
                } else {
                    sb.append("  - No configured source roots.\n");
                }
                return sb.toString();
            }

            for (VirtualFile root : effectiveSourceRoots) {
                String path = root.getPath();
                if (path.contains("generated-sources") || path.contains("generated-test-sources")) {
                    continue;
                }
                boolean isResource = path.contains("resources") || path.endsWith(".github");
                if (isResource && !scope.isShowResources()) {
                    continue;
                }
                boolean test = fileIndex.isInTestSourceContent(root);
                sb.append("  ### Source Root (").append(test ? "test" : "main")
                  .append(isResource ? " resources" : "")
                  .append("): `").append(path).append("`\n");
                appendTree(root, p, sb, "    ", 0);
            }
            return sb.toString();
        });

        ragMessage.addTextPart(markdown);
    }

    private void appendRootFiles(VirtualFile rootDir, Project p, StringBuilder sb, String indent) {
        VirtualFile[] children = rootDir.getChildren();
        List<VirtualFile> files = new ArrayList<>();
        List<String> folders = new ArrayList<>();
        VirtualFile githubDir = null;
        VirtualFile ideaDir = null;
        for (VirtualFile child : children) {
            String cname = child.getName();
            if (".github".equals(cname) && child.isDirectory()) {
                githubDir = child;
                continue;
            }
            if (".idea".equals(cname) && child.isDirectory()) {
                ideaDir = child;
                continue;
            }
            if (cname.startsWith(".")) continue;
            if ("target".equals(cname) || "out".equals(cname) || "build".equals(cname)) continue;
            if (child.isDirectory()) {
                folders.add(cname);
            } else {
                files.add(child);
            }
        }
        if (!files.isEmpty() || !folders.isEmpty() || githubDir != null || ideaDir != null) {
            sb.append(indent).append("### Root Directory\n");
            if (!folders.isEmpty()) {
                Collections.sort(folders);
                sb.append(indent).append("  - Folders: `").append(String.join("`, `", folders)).append("`\n");
            }
            FileStatusManager fsm = FileStatusManager.getInstance(p);
            files.sort(Comparator.comparing(VirtualFile::getName, String.CASE_INSENSITIVE_ORDER));
            for (VirtualFile file : files) {
                sb.append(indent).append("  - 📄 `").append(file.getName()).append("`");
                if (scope.isShowVcsStatus()) {
                    FileStatus st = fsm.getStatus(file);
                    if (st != FileStatus.NOT_CHANGED && st != null) {
                        sb.append(" [").append(st.getText()).append("]");
                    }
                }
                if (scope.isShowFileSizes()) {
                    sb.append(String.format(" [%.1f KB]", file.getLength() / 1024.0));
                }
                sb.append("\n");
            }
            if (githubDir != null && scope.isShowResources()) {
                sb.append(indent).append("  - 📦 `.github`\n");
                appendTree(githubDir, p, sb, indent + "    ", 0);
            }
            if (ideaDir != null && scope.isShowResources()) {
                sb.append(indent).append("  - 📦 `.idea`\n");
                appendTree(ideaDir, p, sb, indent + "    ", 0);
            }
        }
    }

    private void appendTree(VirtualFile dir, Project p, StringBuilder sb, String indent, int depth) {
        if (depth > 12) {
            return;
        }
        VirtualFile[] children = dir.getChildren();
        List<VirtualFile> sorted = new ArrayList<>(List.of(children));
        sorted.sort((f1, f2) -> {
            boolean d1 = f1.isDirectory();
            boolean d2 = f2.isDirectory();
            if (d1 != d2) return d1 ? -1 : 1;
            return f1.getName().compareToIgnoreCase(f2.getName());
        });
        for (VirtualFile child : sorted) {
            String name = child.getName();
            if (name.startsWith(".") || name.equals("target") || name.equals("out") || name.equals("build") || name.equals("caches")) {
                continue;
            }
            if (child.isDirectory()) {
                sb.append(indent).append("- 📦 `").append(name).append("`\n");
                appendTree(child, p, sb, indent + "  ", depth + 1);
            } else if ("java".equals(child.getExtension())) {
                sb.append(indent).append("- 🄹 `").append(name).append("`");
                PsiFile psi = PsiManager.getInstance(p).findFile(child);
                if (psi instanceof PsiJavaFile javaFile) {
                    for (PsiClass cls : javaFile.getClasses()) {
                        if (scope.isShowElementKind()) {
                            String k = cls.isInterface() ? (cls.isAnnotationType() ? "ANNOTATION_TYPE" : "INTERFACE")
                                     : cls.isEnum() ? "ENUM"
                                     : cls.isRecord() ? "RECORD" : "CLASS";
                            sb.append(" (").append(k).append(")");
                        }
                        if (scope.isShowVcsStatus()) {
                            FileStatus st = FileStatusManager.getInstance(p).getStatus(child);
                            if (st != FileStatus.NOT_CHANGED && st != null) {
                                sb.append(" [").append(st.getText()).append("]");
                            }
                        }
                        if (scope.isShowFileSizes()) {
                            sb.append(String.format(" [%.1f KB]", child.getLength() / 1024.0));
                        }
                        if (scope.isShowInnerClasses()) {
                            for (PsiClass inner : cls.getInnerClasses()) {
                                sb.append("\n").append(indent).append("  - `").append(inner.getName()).append("`");
                                if (scope.isShowElementKind()) {
                                    String ik = inner.isInterface() ? (inner.isAnnotationType() ? "ANNOTATION_TYPE" : "INTERFACE")
                                             : inner.isEnum() ? "ENUM"
                                             : inner.isRecord() ? "RECORD" : "CLASS";
                                    sb.append(" (").append(ik).append(")");
                                }
                            }
                        }
                    }
                } else {
                    if (scope.isShowVcsStatus()) {
                        FileStatus st = FileStatusManager.getInstance(p).getStatus(child);
                        if (st != FileStatus.NOT_CHANGED && st != null) {
                            sb.append(" [").append(st.getText()).append("]");
                        }
                    }
                    if (scope.isShowFileSizes()) {
                        sb.append(String.format(" [%.1f KB]", child.getLength() / 1024.0));
                    }
                }
                sb.append("\n");
            } else {
                sb.append(indent).append("- 📄 `").append(child.getName()).append("`");
                if (scope.isShowFileSizes()) {
                    sb.append(String.format(" [%.1f KB]", child.getLength() / 1024.0));
                }
                sb.append("\n");
            }
        }
    }
}
