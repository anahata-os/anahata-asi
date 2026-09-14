/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.intellij.ui.resources;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Smoke tests for {@link IntellijTextResourceViewer#normalizeResourceFileName(String)}, the pure
 * mapping from raw resource/snippet names (and markdown language ids) to canonical file names the
 * IntelliJ {@code FileTypeManager} recognizes for syntax highlighting.
 *
 * @author anahata
 */
class IntellijTextResourceViewerTest {

    /**
     * Null or blank names fall back to a plain-text snippet name.
     */
    @Test
    void nullOrBlankFallsBackToTextSnippet() {
        assertEquals("snippet.txt", IntellijTextResourceViewer.normalizeResourceFileName(null));
        assertEquals("snippet.txt", IntellijTextResourceViewer.normalizeResourceFileName("   "));
    }

    /**
     * A bare language identifier (no extension) becomes a canonical snippet file.
     */
    @Test
    void bareLanguageIdBecomesSnippet() {
        assertEquals("snippet.java", IntellijTextResourceViewer.normalizeResourceFileName("java"));
        assertEquals("snippet.js", IntellijTextResourceViewer.normalizeResourceFileName("javascript"));
        assertEquals("snippet.py", IntellijTextResourceViewer.normalizeResourceFileName("python"));
    }

    /**
     * A real filename keeps its base and gets a canonical extension.
     */
    @Test
    void realFileNameKeepsBaseAndCanonicalizesExtension() {
        assertEquals("Main.java", IntellijTextResourceViewer.normalizeResourceFileName("Main.java"));
        assertEquals("notes.md", IntellijTextResourceViewer.normalizeResourceFileName("notes.markdown"));
        assertEquals("config.yml", IntellijTextResourceViewer.normalizeResourceFileName("config.yaml"));
    }

    /**
     * Unrecognized extensions pass through unchanged (lower-cased).
     */
    @Test
    void unknownExtensionPassesThrough() {
        assertEquals("data.xyz", IntellijTextResourceViewer.normalizeResourceFileName("data.XYZ"));
    }

    /**
     * A representative sample of the alias mappings resolve to their canonical extensions.
     */
    @Test
    void aliasesMapToCanonicalExtensions() {
        assertEquals("main.cpp", IntellijTextResourceViewer.normalizeResourceFileName("main.c++"));
        assertEquals("component.tsx", IntellijTextResourceViewer.normalizeResourceFileName("component.tsx"));
        assertEquals("query.graphql", IntellijTextResourceViewer.normalizeResourceFileName("query.gql"));
        assertEquals("run.sh", IntellijTextResourceViewer.normalizeResourceFileName("run.bash"));
    }
}
