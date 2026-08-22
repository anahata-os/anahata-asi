/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.yam.tools.benchmarks;

import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import uno.anahata.asi.AbstractAsiContainer;
import uno.anahata.asi.agi.tool.ToolPermission;
import uno.anahata.asi.toolkit.History;
import uno.anahata.asi.toolkit.Host;
import uno.anahata.asi.toolkit.Session;
import uno.anahata.asi.toolkit.java.Java;
import uno.anahata.asi.toolkit.resources.Resources;
import uno.anahata.asi.toolkit.shell.Shell;

/**
 * The official Anahata-AGI-1 benchmark suite catalog.
 * <p>
 * Standardizes prompts, isolated tool environments, and permissions for the
 * official pure-Java certification benchmark.
 * </p>
 *
 * @author anahata
 */
public class Agi1TestCatalog extends TestCatalog {

    /**
     * Standard header template for Anahata-AGI-1 tests.
     */
    public static final String STANDARD_HEADER =
            "You are participating in the official Anahata-AGI-1 Benchmark (%s: %s).\n"
            + "Your task must be executed autonomously with zero defects.";

    /**
     * Standard footer template for Anahata-AGI-1 tests.
     */
    public static final String STANDARD_FOOTER =
            "Do not seek user prompt, confirmation, or follow-up. Execute all necessary tools "
            + "and continue until your task has been completely fulfilled.";

    /**
     * Test #1: OS Hardware &amp; System Values Dashboard (JNA Native C-Library Binding).
     */
    public static final TestDefinition JAVA_JNA_1 = TestDefinition.builder()
            .testCode("JAVA-JNA-1")
            .title("OS Hardware & System Values Dashboard")
            .rawPrompt("Build a real-time, interactive system telemetry dashboard "
                    + "using JNA (com.sun.jna.Library) to monitor host system CPU/GPU temperatures "
                    + "and process ID. You have complete creative freedom to decide what metrics to retrieve, what UI "
                    + "framework to use, and how to design the interface. Window title MUST contain your Model ID.")
            .toolkits(List.of(
                    ToolkitSettings.of(Java.class, "compileAndExecute", ToolPermission.APPROVE_ALWAYS),
                    ToolkitSettings.of(Host.class),
                    ToolkitSettings.of(Shell.class)
            ))
            .build();

    /**
     * Test #2: Retro Arcade Game Execution (Swing EDT Loop &amp; Physics).
     */
    public static final TestDefinition JAVA_ARKANOID_1 = TestDefinition.builder()
            .testCode("JAVA-ARKANOID-1")
            .title("Retro Arcade Game Execution")
            .rawPrompt("Build a fully playable, retro Arkanoid brick-breaker game in Swing with smooth 60 FPS animation loop. Window title MUST contain your Model ID.")
            .toolkits(List.of(
                    ToolkitSettings.of(Java.class, "compileAndExecute", ToolPermission.APPROVE_ALWAYS),
                    ToolkitSettings.of(Host.class),
                    ToolkitSettings.of(Session.class),
                    ToolkitSettings.of(History.class),
                    ToolkitSettings.of(Resources.class)
            ))
            .build();

    /**
     * Test #3: Classic Snake Game.
     */
    public static final TestDefinition JAVA_SNAKE_GAME = TestDefinition.builder()
            .testCode("JAVA-SNAKEGAME-1")
            .title("Snake Game")
            .rawPrompt("Make an snake game using java tool. You can use the best tool for making this work in Java.")
            .toolkits(List.of(
                    ToolkitSettings.of(Java.class, "compileAndExecute", ToolPermission.APPROVE_ALWAYS),
                    ToolkitSettings.of(Host.class),
                    ToolkitSettings.of(Session.class),
                    ToolkitSettings.of(History.class),
                    ToolkitSettings.of(Resources.class)
            ))
            .build();

    /**
     * Resolves the official results directory in the website source tree or fallback.
     *
     * @return The path to the Anahata-AGI-1 results directory.
     */
    public static Path resolveOfficialResultsDirectory() {
        Path devWebPath = Paths.get(System.getProperty("user.home"), "NetBeansProjects", "anahata-asi-parent",
                "anahata-asi-web", "src", "main", "resources", "web", "benchmarks", "anahata-agi-1");
        if (Files.exists(devWebPath)) {
            return devWebPath;
        }
        Path relativeWebPath = Paths.get("anahata-asi-web", "src", "main", "resources", "web", "benchmarks", "anahata-agi-1");
        if (Files.exists(relativeWebPath)) {
            return relativeWebPath;
        }
        return AbstractAsiContainer.getWorkDirSubDir("benchmarks").resolve("anahata-agi-1");
    }

    /**
     * Constructs the official Anahata-AGI-1 test catalog with the default web results directory.
     */
    public Agi1TestCatalog() {
        this(resolveOfficialResultsDirectory());
    }

    /**
     * Constructs the official Anahata-AGI-1 test catalog with a custom results directory.
     *
     * @param resultsDirectory The filesystem directory where JSON results files are persisted.
     */
    public Agi1TestCatalog(Path resultsDirectory) {
        super(
                "ANAHATA-AGI-1",
                "Anahata-AGI-1",
                "Flagship pure-Java performance, resilience, and multi-modal autonomy suite.",
                STANDARD_HEADER,
                STANDARD_FOOTER,
                resultsDirectory
        );
        addTest(JAVA_JNA_1);
        addTest(JAVA_ARKANOID_1);
        addTest(JAVA_SNAKE_GAME);
    }
}
