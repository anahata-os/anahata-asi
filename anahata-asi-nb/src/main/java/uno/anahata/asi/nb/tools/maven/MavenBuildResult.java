/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi.nb.tools.maven;

import io.swagger.v3.oas.annotations.media.Schema;
import java.util.List;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Represents the detailed result of a Maven build execution, including status, 
 * exit code, captured output, and a breakdown of build phases.
 * 
 * @author anahata
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@Schema(description = "Represents the detailed result of a Maven build execution, including status, exit code, and captured output.")
public class MavenBuildResult {

    /**
     * The final status of the Maven process.
     */
    @Schema(description = "The final status of the Maven process (e.g., COMPLETED, TIMEOUT).")
    public enum ProcessStatus {
        /** The process completed normally. */
        COMPLETED,
        /** The process timed out. */
        TIMEOUT,
        /** The process was interrupted. */
        INTERRUPTED
    }

    /**
     * Represents a single phase or goal within a Maven build.
     */
    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @Schema(description = "Represents a single phase or goal within a Maven build.")
    public static class BuildPhase {
        /** The name of the Maven phase (e.g., 'compile', 'test'). */
        @Schema(description = "The name of the phase")
        private String name;
        /** The plugin and goal associated with this phase. */
        @Schema(description = "The plugin and goal")
        private String plugin;
        /** Whether this phase succeeded or failed. */
        @Schema(description = "Whether this phase succeeded")
        private boolean success;
        /** The duration of this phase in milliseconds. */
        @Schema(description = "The duration in milliseconds")
        private long durationMs;
    }

    /** The final status of the Maven process. */
    @Schema(description = "The final status of the Maven process.")
    private ProcessStatus status;

    /** The exit code of the Maven process. */
    @Schema(description = "The exit code of the Maven process. Note: This can be unreliable in some execution environments; always check the stdout for 'BUILD SUCCESS'.")
    private Integer exitCode;
    
    /** The captured standard output stream (last 100 lines). */
    @Schema(description = "The captured standard output stream (last 100 lines).")
    private String stdOutput;

    /** The captured standard error stream (last 100 lines). */
    @Schema(description = "The captured standard error stream (last 100 lines).")
    private String stdError;

    /** The absolute path to the full, untruncated log file saved on disk. */
    @Schema(description = "The absolute path to the full, untruncated log file saved on disk.")
    private String logFile;
    
    /** A list of build phases executed during the Maven run, with their individual outcomes. */
    @Schema(description = "A list of build phases executed during the Maven run")
    private List<BuildPhase> phases;
}
