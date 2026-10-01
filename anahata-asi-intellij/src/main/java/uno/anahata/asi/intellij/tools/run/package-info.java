/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */

/**
 * Toolkits for discovering and executing IntelliJ run, debug, and test configurations.
 * <p>
 * Bridges execution and test runner frameworks to the Anahata ASI agentic workflow:
 * </p>
 * <ul>
 *   <li>{@link uno.anahata.asi.intellij.tools.run.RunConfigurations}: Discovers and executes run configurations using
 *       {@link com.intellij.execution.RunManager} and {@link com.intellij.execution.ProgramRunnerUtil}. Supports synchronous
 *       execution with output tailing and deep integration with IntelliJ's Service Messages test runner (SMTRunner) to report
 *       structured passed/failed/ignored counts and failure details.</li>
 * </ul>
 *
 * @author anahata
 */
package uno.anahata.asi.intellij.tools.run;
