/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.intellij.internal;

import com.intellij.openapi.vfs.VirtualFile;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.agi.resource.Resource;
import uno.anahata.asi.intellij.IntellijAsiContainer;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

/**
 * Bridge between IntelliJ {@link VirtualFile} selections and the per-session Anahata
 * {@code ResourceManager}, backing the Project-view "AGI Context" node decorator and popup
 * action.
 * <p>
 * This is the IntelliJ counterpart of the NetBeans {@code FilesContextActionLogic}. It adds
 * and removes files (recursing into directories) from a session's context, and reports
 * membership so the decorator can badge in-context files. All session lookups go through the
 * live {@link IntellijAsiContainer#getInstances()} registry.
 * </p>
 *
 * @author anahata
 */
public final class AgiContext {

    /**
     * The actor recorded when files are added to context from the Project view.
     */
    private static final String ACTOR = "added via Project view 'AGI Context' menu";

    /**
     * Non-instantiable utility holder.
     */
    private AgiContext() {
    }

    /**
     * Returns all active sessions across every live tool-window container.
     *
     * @return the active sessions.
     */
    public static List<Agi> activeSessions() {
        List<Agi> all = new ArrayList<>();
        for (IntellijAsiContainer container : IntellijAsiContainer.getInstances()) {
            all.addAll(container.getActiveAgis());
        }
        return all;
    }

    /**
     * Counts how many active sessions currently hold the given file in context.
     *
     * @param file the file to test.
     * @return the number of sessions containing the file (0 if none, or if it is a directory).
     */
    public static int sessionsContaining(VirtualFile file) {
        if (file.isDirectory()) {
            return 0;
        }
        String key = pathKey(file);
        int count = 0;
        for (Agi agi : activeSessions()) {
            if (agi.getResourceManager().findByPath(key).isPresent()) {
                count++;
            }
        }
        return count;
    }

    /**
     * Tests whether every file in the selection (directories expanded to their files) is in
     * the given session's context.
     *
     * @param agi   the session.
     * @param files the selected files/folders.
     * @return {@code true} if the selection is non-empty and fully in context.
     */
    public static boolean allInContext(Agi agi, VirtualFile[] files) {
        List<Path> paths = collect(files);
        if (paths.isEmpty()) {
            return false;
        }
        for (Path path : paths) {
            if (agi.getResourceManager().findByPath(path.toString()).isEmpty()) {
                return false;
            }
        }
        return true;
    }

    /**
     * Adds the selection (directories expanded to their files) to the session's context.
     *
     * @param agi   the session.
     * @param files the selected files/folders.
     * @return the number of files registered.
     */
    public static int add(Agi agi, VirtualFile[] files) {
        List<Path> paths = collect(files);
        if (!paths.isEmpty()) {
            agi.getResourceManager().registerPaths(paths, ACTOR);
        }
        return paths.size();
    }

    /**
     * Removes the selection (directories expanded to their files) from the session's context.
     *
     * @param agi   the session.
     * @param files the selected files/folders.
     * @return the number of resources unregistered.
     */
    public static int remove(Agi agi, VirtualFile[] files) {
        int removed = 0;
        for (Path path : collect(files)) {
            Optional<Resource> resource = agi.getResourceManager().findByPath(path.toString());
            if (resource.isPresent()) {
                agi.getResourceManager().unregister(resource.get().getId());
                removed++;
            }
        }
        return removed;
    }

    /**
     * Expands a selection into concrete local file paths, recursing into directories.
     *
     * @param files the selected files/folders.
     * @return the flattened list of local file paths.
     */
    private static List<Path> collect(VirtualFile[] files) {
        List<Path> paths = new ArrayList<>();
        if (files != null) {
            for (VirtualFile file : files) {
                collectInto(file, paths);
            }
        }
        return paths;
    }

    /**
     * Recursively accumulates local files under a node into the target list.
     *
     * @param file the file or directory node.
     * @param out  the accumulator.
     */
    private static void collectInto(VirtualFile file, List<Path> out) {
        if (file.isDirectory()) {
            for (VirtualFile child : file.getChildren()) {
                collectInto(child, out);
            }
        } else if (file.isInLocalFileSystem()) {
            out.add(Path.of(file.getPath()));
        }
    }

    /**
     * Canonical resource key for a local file, matching how {@code registerPaths} stores it.
     *
     * @param file the file.
     * @return the absolute path string key.
     */
    private static String pathKey(VirtualFile file) {
        return Path.of(file.getPath()).toString();
    }
}
