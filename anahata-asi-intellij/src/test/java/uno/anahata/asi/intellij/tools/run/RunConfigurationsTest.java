/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.intellij.tools.run;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Smoke tests for {@link RunConfigurations#firstLine(String)}, the pure helper that trims a
 * (possibly multi-line) test failure message down to its first non-blank line for compact display.
 *
 * @author anahata
 */
class RunConfigurationsTest {

    /**
     * A single-line message is returned stripped.
     */
    @Test
    void singleLineIsStripped() {
        assertEquals("boom", RunConfigurations.firstLine("  boom  "));
    }

    /**
     * A multi-line message returns only its first line.
     */
    @Test
    void multiLineReturnsFirstLine() {
        assertEquals("expected 1 but was 2", RunConfigurations.firstLine("expected 1 but was 2\n\tat Foo.bar(Foo.java:10)\n\tat ..."));
    }

    /**
     * Leading blank lines are skipped/stripped before the first content line.
     */
    @Test
    void leadingWhitespaceAndCrlfHandled() {
        assertEquals("first real line", RunConfigurations.firstLine("\r\nfirst real line\r\nsecond"));
    }

    /**
     * A blank message yields an empty string.
     */
    @Test
    void blankYieldsEmpty() {
        assertEquals("", RunConfigurations.firstLine("   \n  "));
    }
}
