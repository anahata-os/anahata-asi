/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.yam.tools.benchmarks;

import java.util.List;
import java.util.Optional;
import uno.anahata.asi.agi.tool.ToolPermission;
import uno.anahata.asi.toolkit.History;
import uno.anahata.asi.toolkit.Host;
import uno.anahata.asi.toolkit.Session;
import uno.anahata.asi.toolkit.java.Java;
import uno.anahata.asi.toolkit.resources.Resources;
import uno.anahata.asi.toolkit.shell.Shell;

/**
 * Master catalog and repository of standardized test specifications for the Anahata-AGI-1 suite.
 * <p>
 * Serves as the single programmatic source of truth for benchmark prompts, isolated tool environments,
 * and security permissions.
 * </p>
 *
 * @author anahata
 */
public final class Agi1Catalog {

    /**
     * Test #1: OS Hardware & System Values Dashboard (JNA Native C-Library Binding).
     */
    public static final Agi1TestDefinition JAVA_JNA_1 = Agi1TestDefinition.builder()
            .testCode("JAVA-JNA-1")
            .title("OS Hardware & System Values Dashboard")
            .rawPrompt("Build a real-time, interactive Swing host system telemetry dashboard in a single Java class "
                    + "extending SwingAgiTool using JNA (com.sun.jna.Library) to monitor host system CPU/GPU temperatures "
                    + "and process ID. You have complete creative freedom to decide what metrics to retrieve, what UI "
                    + "framework to use, and how to design the interface. Window title MUST contain your Model ID.")
            .toolkits(List.of(
                    ToolkitSettings.of(Java.class, "compileAndExecute", ToolPermission.APPROVE_ALWAYS),
                    ToolkitSettings.of(Host.class),
                    ToolkitSettings.of(Shell.class)
            ))
            .timeoutSeconds(180)
            .build();

    /**
     * Test #2: Retro Arcade Game Execution (Swing EDT Loop & Physics).
     */
    public static final Agi1TestDefinition JAVA_ARKANOID_1 = Agi1TestDefinition.builder()
            .testCode("JAVA-ARKANOID-1")
            .title("Retro Arcade Game Execution")
            .rawPrompt("Build a fully playable, retro Arkanoid brick-breaker game in Swing in a single Java class "
                    + "extending SwingAgiTool with paddle movement, ball collision physics, power-ups, score counter, "
                    + "and smooth 60 FPS EDT animation loop. Window title MUST contain your Model ID.")
            .toolkits(List.of(
                    ToolkitSettings.of(Java.class, "compileAndExecute", ToolPermission.APPROVE_ALWAYS),
                    ToolkitSettings.of(Host.class),
                    ToolkitSettings.of(Session.class),
                    ToolkitSettings.of(History.class),
                    ToolkitSettings.of(Resources.class)
            ))
            .timeoutSeconds(240)
            .build();

    /**
     * An unmodifiable list of all active test definitions in the suite.
     */
    private static final List<Agi1TestDefinition> ALL_TESTS = List.of(
            JAVA_JNA_1,
            JAVA_ARKANOID_1
    );

    /**
     * Private constructor to prevent instantiation of utility catalog.
     */
    private Agi1Catalog() {
    }

    /**
     * Retrieves all registered test definitions in the Anahata-AGI-1 suite.
     *
     * @return An unmodifiable list of all test definitions.
     */
    public static List<Agi1TestDefinition> getAllTests() {
        return ALL_TESTS;
    }

    /**
     * Finds a test definition by its unique test code (e.g., "JAVA-JNA-1").
     *
     * @param testCode The test code to look up (case-insensitive).
     * @return An Optional containing the matching test definition if found, or empty otherwise.
     */
    public static Optional<Agi1TestDefinition> findByCode(String testCode) {
        if (testCode == null || testCode.isBlank()) {
            return Optional.empty();
        }
        return ALL_TESTS.stream()
                .filter(test -> test.testCode().equalsIgnoreCase(testCode.trim()))
                .findFirst();
    }
}
