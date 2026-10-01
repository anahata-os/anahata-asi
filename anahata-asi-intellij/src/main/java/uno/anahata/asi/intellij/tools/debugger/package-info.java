/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */

/**
 * Debugger automation toolkit leveraging IntelliJ's platform-agnostic XDebugger API.
 * <p>
 * Encapsulates the {@link uno.anahata.asi.intellij.tools.debugger.Debugger} toolkit, providing
 * programmatically accessible tools for:
 * </p>
 * <ul>
 *   <li>Setting, listing, and clearing line breakpoints across any registered {@link com.intellij.xdebugger.breakpoints.XLineBreakpointType}.</li>
 *   <li>Inspecting and monitoring active debug sessions and their suspended states.</li>
 *   <li>Launching run configurations under the debugger with synchronous or bounded waiting.</li>
 *   <li>Session execution control: resume, step over, step into, step out, and stop.</li>
 *   <li>Asynchronous evaluation of expressions and top-frame variable inspection via bounded latch synchronization.</li>
 * </ul>
 *
 * @author anahata
 */
package uno.anahata.asi.intellij.tools.debugger;
