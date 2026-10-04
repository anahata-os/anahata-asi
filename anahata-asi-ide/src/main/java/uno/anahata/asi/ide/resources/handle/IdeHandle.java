/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.ide.resources.handle;

import java.io.IOException;
import java.net.URI;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import lombok.Getter;
import lombok.NonNull;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.resource.handle.ResourceHandle;
import uno.anahata.asi.ide.tools.hints.AbstractHints;
import uno.anahata.asi.ide.tools.hints.HintInfo;
import uno.anahata.asi.ide.tools.vcs.AbstractVCS;
import uno.anahata.asi.ide.tools.vcs.HistoryEntry;
import uno.anahata.asi.ide.tools.vcs.VcsDiff;
import uno.anahata.asi.persistence.Rebindable;

/**
 * Universal abstract base class for IDE-integrated resource handles.
 * <p>
 * Bridges host IDE Virtual File Systems (such as NetBeans {@code FileObject}
 * and IntelliJ {@code VirtualFile}) to the Anahata resource pipeline.
 * Manages URI normalization, absolute filesystem paths, existence checks,
 * unsaved editor state detection ({@link #isModified()}), and contextual
 * prompt augmentation via {@link #getAnnex()}.
 * </p>
 * <p>
 * <b>VCS and Local History Integration:</b> Dynamically queries the session's active
 * {@link AbstractVCS} toolkit to supply live working-copy diffs and chronological
 * history snapshots directly to the prompt annex without coupling the core framework.
 * </p>
 *
 * @author anahata
 */
@Slf4j
public abstract class IdeHandle extends ResourceHandle implements Rebindable {

    /**
     * The unique identifier URI for the resource.
     */
    @NonNull
    @Getter
    protected URI uri;

    /**
     * The absolute canonical filesystem path for local resources, or null if remote/archive.
     */
    @Getter
    protected String path;

    /**
     * Constructs a new IDE handle from a URI.
     *
     * @param uri The resource URI.
     */
    public IdeHandle(@NonNull URI uri) {
        setUri(uri);
    }

    /**
     * Constructs a new IDE handle from an absolute filesystem path.
     *
     * @param path The absolute path to the local file.
     */
    public IdeHandle(@NonNull String path) {
        this(Paths.get(path).toUri());
    }

    /**
     * Sets and normalizes the resource URI and derives the absolute filesystem path
     * for local files.
     *
     * @param uri The resource URI.
     */
    protected void setUri(URI uri) {
        if (uri != null && uri.getScheme() != null && uri.getScheme().equalsIgnoreCase("file")) {
            this.uri = Paths.get(uri).toUri();
            this.path = Paths.get(this.uri).toAbsolutePath().toString();
        } else {
            this.uri = uri;
            this.path = (uri != null) ? uri.getPath() : null;
        }
    }

    /**
     * {@inheritDoc}
     * <p>
     * IDE handles always represent persistent physical or VFS resources, not memory-backed snippets.
     * </p>
     */
    @Override
    public boolean isVirtual() {
        return false;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Checks whether the underlying file has unsaved in-memory modifications
     * in an open editor tab of the host IDE.
     * </p>
     */
    @Override
    public abstract boolean isModified();

    /**
     * {@inheritDoc}
     * <p>
     * Resolves the HTML-formatted display name with authentic IDE Version Control
     * status colors or diagnostic annotations.
     * </p>
     */
    @Override
    public abstract String getHtmlDisplayName();

    /**
     * {@inheritDoc}
     * <p>
     * Persists content changes through the host IDE's editor or filesystem APIs
     * while labeling Local History snapshots with the provided reason.
     * </p>
     */
    @Override
    public abstract void write(String content, String reason) throws IOException;

    /**
     * {@inheritDoc}
     * <p>
     * Re-normalizes the URI and derived path after session deserialization.
     * </p>
     */
    @Override
    public void rebind() {
        super.rebind();
        if (uri != null && uri.getScheme() == null) {
            setUri(URI.create(uri.toString()));
        } else {
            setUri(uri);
        }
    }

    /**
     * Queries the session's active {@link AbstractVCS} toolkit to generate
     * a unified diff against the repository pristine base.
     *
     * @return A {@link VcsDiff} DTO, or null if clean, untracked, newly added, or unsupported.
     */
    public VcsDiff getDiffToHead() {
        if (owner == null || owner.getAgi() == null || path == null) {
            return null;
        }
        Optional<AbstractVCS> vcsOpt = owner.getAgi().getToolkit(AbstractVCS.class);
        if (vcsOpt.isPresent()) {
            try {
                return vcsOpt.get().getDiff(path, null);
            } catch (Exception e) {
                log.debug("Failed to get diff to head for {}: {}", path, e.getMessage());
            }
        }
        return null;
    }

    /**
     * Queries the session's active {@link AbstractVCS} toolkit to retrieve
     * recent VCS and Local History revisions.
     *
     * @param maxEntries Maximum number of history entries to return.
     * @return A list of {@link HistoryEntry} DTOs.
     */
    public List<HistoryEntry> getHistory(int maxEntries) {
        if (owner == null || owner.getAgi() == null || path == null) {
            return Collections.emptyList();
        }
        Optional<AbstractVCS> vcsOpt = owner.getAgi().getToolkit(AbstractVCS.class);
        if (vcsOpt.isPresent()) {
            try {
                return vcsOpt.get().getHistory(path, maxEntries);
            } catch (Exception e) {
                log.debug("Failed to get history for {}: {}", path, e.getMessage());
            }
        }
        return Collections.emptyList();
    }

    /**
     * Queries the session's active {@link AbstractHints} toolkit to retrieve
     * live code inspection diagnostics, warnings, and hints for this file.
     *
     * @return A list of {@link HintInfo} diagnostics, or empty if none or unsupported.
     */
    public List<HintInfo> getHints() {
        if (path == null) {
            return Collections.emptyList();
        }
        Optional<AbstractHints> hintsOpt = owner.getAgi().getToolkit(AbstractHints.class);
        if (hintsOpt.isPresent()) {
            try {
                return hintsOpt.get().getFileHints(path);
            } catch (Exception e) {
                log.debug("Failed to get hints for {}: {}", path, e.getMessage());
            }
        }
        return Collections.emptyList();
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Aggregates live VCS diff to pristine base,
     * live code inspection hints, and recent commit/local history into the resource prompt annex.
     * </p>
     */
    @Override
    public List<String> getAnnex() {
        List<String> annex = new ArrayList<>();
        if (isTextual()) {
            VcsDiff diff = getDiffToHead();
            if (diff != null && diff.hasChanges() && !diff.isNewFile()) {
                annex.add(diff.toMarkdown());
            }
            List<HintInfo> hints = getHints();
            if (!hints.isEmpty()) {
                String hintsMd = HintInfo.toMarkdown(hints);
                if (hintsMd != null && !hintsMd.isBlank()) {
                    annex.add(hintsMd);
                }
            }
        }
        List<HistoryEntry> history = getHistory(5);
        if (history != null && !history.isEmpty()) {
            String md = HistoryEntry.toMarkdownTable(getName(), history);
            if (md != null && !md.isBlank()) {
                annex.add("### Recent History (`" + getName() + "`):\n" + md);
            }
        }
        return annex;
    }
}
