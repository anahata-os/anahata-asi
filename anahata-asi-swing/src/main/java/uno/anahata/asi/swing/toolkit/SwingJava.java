/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.toolkit;

import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.List;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.tool.AgiTool;
import uno.anahata.asi.agi.tool.AiToolkit;
import uno.anahata.asi.agi.tool.ToolContext;
import uno.anahata.asi.agi.tool.spi.java.JavaMethodToolResponse;
import uno.anahata.asi.swing.agi.tool.SwingAgiTool;
import uno.anahata.asi.swing.internal.SwingUtils;
import uno.anahata.asi.toolkit.Java;

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
@AiToolkit("A Swing-aware Java toolkit that supports EDT synchronization with context propagation.")
public class SwingJava extends Java {

    /** {@inheritDoc} */
    @Override
    public List<String> getSystemInstructions() throws Exception {
        List<String> instructions = new ArrayList<>(super.getSystemInstructions());
        
        StringBuilder sb = new StringBuilder();
        sb.append("\n**Swing execution helpers**\n");
        sb.append("You have also access to these helpers for EDT operations:\n\n");
        sb.append("- `runInEdt(Runnable runnable)`: Safely executes code on the Event Dispatch Thread (EDT) while propagating the current tool context. Use this for non-blocking UI updates.\n");
        sb.append("- `runInEdtAndWait(Runnable runnable)`: Executes code on the EDT and waits for completion. Use this when you need to ensure the UI has updated before proceeding.\n\n");
        sb.append("**Example usage**:\n");
        sb.append("```java\n");
        sb.append("runInEdtAndWait(() -> {\n");
        sb.append("    log(\"Updating UI component...\");\n");
        sb.append("    myComponent.setText(\"New Value\");\n");
        sb.append("});\n");
        sb.append("```\n");
        
        instructions.add(sb.toString());
        return instructions;
    }
    
    /**
     * The class the model should extend when generating Agi tools.
     * 
     * @return the SwingAgiTool class.
     */
    @Override
    protected Class<? extends ToolContext> getConcreteClassModelShouldExtend() {
        return SwingAgiTool.class;
    }

}
