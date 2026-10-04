/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.tools.project.components;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import javax.lang.model.element.ElementKind;
import lombok.extern.slf4j.Slf4j;
import org.openide.filesystems.FileObject;
import uno.anahata.asi.ide.tools.project.ProjectStructureScope;

/**
 * High-performance, in-memory cache for resolved AST metadata of Java source files.
 * <p>
 * Binds resolved compiler metadata (element kinds, supertypes, Javadoc summaries, and recursive
 * inner class structures) to a {@link FileObject}'s path and its {@code lastModified} filesystem
 * timestamp. This prevents re-running the heavy Javac compiler pipeline ({@code JavaSource.toPhase(Phase.ELEMENTS_RESOLVED)})
 * on unchanged files on subsequent conversation turns, dropping warm-turn structure scans to sub-millisecond speeds.
 * </p>
 *
 * @param lastModified The filesystem modification timestamp (in epoch milliseconds) when this file was parsed.
 * @param kind The primary Java element kind of the top-level type (e.g. {@link ElementKind#CLASS}).
 * @param supertypes Formatted supertypes string (e.g. {@code "extends DesktopAgiTool implements Callable"}), or {@code null}.
 * @param javadocSummary Sanitized first sentence of class-level Javadoc summary, or {@code null}.
 * @param innerClasses Recursive descriptors for all inner, nested, and anonymous types declared in this file.
 *
 * @author Anahata
 */
@Slf4j
public record CachedAstMetadata(
        long lastModified,
        ElementKind kind,
        String supertypes,
        String javadocSummary,
        List<CachedInnerClass> innerClasses
) implements Serializable {

    /**
     * In-memory cache holding resolved AST metadata keyed by {@link FileObject#getPath()}.
     */
    private static final Map<String, CachedAstMetadata> CACHE = new ConcurrentHashMap<>();

    /**
     * Canonical constructor guaranteeing non-null, immutable child collections.
     *
     * @param lastModified Filesystem timestamp in epoch milliseconds.
     * @param kind The primary Java element kind.
     * @param supertypes Formatted supertypes declaration string, or null.
     * @param javadocSummary Sanitized first sentence of class Javadoc, or null.
     * @param innerClasses List of inner class descriptors.
     */
    public CachedAstMetadata {
        innerClasses = (innerClasses == null || innerClasses.isEmpty())
                ? Collections.emptyList()
                : List.copyOf(innerClasses);
    }

    /**
     * Retrieves cached AST metadata for a given file if present and still valid on disk.
     *
     * @param fo The NetBeans FileObject to query.
     * @return The cached metadata if the file is unmodified since the last scan, or {@code null} on a cache miss.
     */
    public static CachedAstMetadata get(FileObject fo) {
        if (!fo.isValid()) {
            return null;
        }
        CachedAstMetadata cached = CACHE.get(fo.getPath());
        if (cached != null && cached.lastModified() == fo.lastModified().getTime()) {
            return cached;
        }
        return null;
    }

    /**
     * Stores resolved AST metadata for a given file and component into the persistent cache.
     *
     * @param fo The NetBeans FileObject.
     * @param comp The enriched top-level ProjectComponent.
     */
    public static void put(FileObject fo, ProjectComponent comp) {
        List<CachedInnerClass> inners = extractCachedInnerClasses(comp.getChildren());
        CACHE.put(fo.getPath(), new CachedAstMetadata(
                fo.lastModified().getTime(),
                comp.getKind(),
                comp.getSupertypes(),
                comp.getJavadocSummary(),
                inners
        ));
    }

    /**
     * Applies cached AST metadata to a target project component, restoring its attributes and inner classes.
     *
     * @param comp The project component to enrich.
     * @param cached The cached metadata record to apply.
     * @param scope The active project structure granularity scope.
     */
    public static void apply(ProjectComponent comp, CachedAstMetadata cached, ProjectStructureScope scope) {
        comp.setKind(cached.kind());
        if (scope.isShowSupertypes()) {
            comp.setSupertypes(cached.supertypes());
        }
        if (scope.isShowJavadoc()) {
            comp.setJavadocSummary(cached.javadocSummary());
        }
        if (scope.isShowInnerClasses()) {
            comp.getChildren().clear();
            restoreCachedInnerClasses(comp, cached.innerClasses(), scope);
        }
    }

    /**
     * Clears all entries from the in-memory AST metadata cache.
     */
    public static void clearCache() {
        CACHE.clear();
    }

    /**
     * Returns the total count of cached file entries.
     *
     * @return Cache entry count.
     */
    public static int getCacheSize() {
        return CACHE.size();
    }

    /**
     * Recursively extracts lightweight cached inner class descriptors from a list of project components.
     *
     * @param children The list of project components representing nested types.
     * @return An immutable list of {@link CachedInnerClass} records.
     */
    private static List<CachedInnerClass> extractCachedInnerClasses(List<ProjectComponent> children) {
        if (children.isEmpty()) {
            return Collections.emptyList();
        }
        List<CachedInnerClass> result = new ArrayList<>(children.size());
        for (ProjectComponent child : children) {
            result.add(new CachedInnerClass(
                    child.getFqn(),
                    child.getKind(),
                    child.getSupertypes(),
                    child.getJavadocSummary(),
                    extractCachedInnerClasses(child.getChildren())
            ));
        }
        return result;
    }

    /**
     * Recursively reconstructs nested {@link ProjectComponent} trees from cached inner class descriptors.
     *
     * @param parentComp The parent component to attach reconstructed children to.
     * @param cachedInners The list of cached inner class descriptors to restore.
     * @param scope The active project structure granularity scope governing which attributes to apply.
     */
    private static void restoreCachedInnerClasses(ProjectComponent parentComp, List<CachedInnerClass> cachedInners, ProjectStructureScope scope) {
        for (CachedInnerClass cic : cachedInners) {
            try {
                ProjectComponent innerComp = new ProjectComponent(parentComp.getFileObject(), null);
                innerComp.setFqn(cic.fqn());
                innerComp.setKind(cic.kind());
                if (scope.isShowSupertypes()) {
                    innerComp.setSupertypes(cic.supertypes());
                }
                if (scope.isShowJavadoc()) {
                    innerComp.setJavadocSummary(cic.javadocSummary());
                }
                parentComp.addChild(innerComp);
                restoreCachedInnerClasses(innerComp, cic.children(), scope);
            } catch (Throwable t) {
                log.debug("Failed to restore cached inner class {}: {}", cic.fqn(), t.getMessage());
            }
        }
    }
}
