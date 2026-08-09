/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.toolkit;

import java.util.ArrayList;
import java.util.List;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.tool.ToolContext;
import uno.anahata.asi.swing.agi.tool.SwingAgiTool;
import uno.anahata.asi.toolkit.java.Java;
import uno.anahata.asi.agi.tool.AgiToolkit;
import uno.anahata.asi.swing.AbstractSwingAsiContainer;
import uno.anahata.asi.swing.agi.AgiPanel;
import uno.anahata.asi.swing.agi.SwingAgiConfig;

/**
 * An extension of the {@link Java} toolkit that provides Swing-specific
 * execution utilities, such as EDT synchronization with context propagation.
 * <p>
 * This toolkit ensures that code running on the Event Dispatch Thread (EDT)
 * can still access the tool execution context (logs, errors, attachments) 
 * by capturing and re-applying the thread-local state.
 * </p>
 * 
 * @author anahata
 */
@Slf4j
@AgiToolkit("A Swing-aware Java toolkit that supports EDT synchronization with context propagation.")
public class SwingJava extends Java {

    /**
     * Constructs a new instance of the toolkit and adds SwingAgiTool to the list of parentFirstClassess.
     */
    public SwingJava() {
        registerParentFirstClass(SwingAgiConfig.class);
        registerParentFirstClass(SwingAgiTool.class);
        registerParentFirstClass(AbstractSwingAsiContainer.class);
        registerParentFirstClass(AgiPanel.class);
    }
    
    /** 
     * {@inheritDoc} 
     * <p>
     * Injects Swing-specific execution helpers into the system instructions, 
     * enabling the model to use {@code runInEdt} and {@code runInEdtAndWait} 
     * for safe UI interactions.
     * </p> 
     */
    @Override
        public List<String> getSystemInstructions() throws Exception {
        List<String> instructions = new ArrayList<>(super.getSystemInstructions());

                StringBuilder sb = new StringBuilder();
                sb.append("\n**Swing & EDT Execution Helpers (Context Propagation)**\n");
                sb.append("You have direct access to these Swing execution helpers (inherited from `SwingAgiTool`):\n\n");
                sb.append("- `runInEdt(Runnable runnable)`: Safely executes code on the Event Dispatch Thread (EDT) while **automatically propagating the active tool context**. Calling `log()`, `error()`, or `addAttachment()` inside the runnable block works seamlessly and outputs directly to the UI response panel.\n");
                sb.append("- `runInEdtAndWait(Runnable runnable)`: Executes code on the EDT and blocks the execution thread until complete, maintaining full context awareness throughout.\n\n");
                sb.append("**Example usage**:\n");
                sb.append("```java\n");
                sb.append("runInEdtAndWait(() -> {\n");
                sb.append("    // ToolContext methods work directly on the EDT inside runInEdt/runInEdtAndWait!\n");
                sb.append("    log(\"Updating UI component on the Swing EDT...\");\n");
                sb.append("    myComponent.setText(\"New Value\");\n");
                sb.append("});\n");
                sb.append("```\n");

                instructions.add(sb.toString());
                return instructions;
    }
    
    /** 
     * {@inheritDoc} 
     * <p>
     * Configures the code generator to produce classes extending {@link SwingAgiTool}, 
     * which provides the necessary plumbing for EDT-aware context propagation.
     * </p> 
     */
    @Override
    protected Class<? extends ToolContext> getConcreteClassModelShouldExtend() {
        return SwingAgiTool.class;
    }

}
