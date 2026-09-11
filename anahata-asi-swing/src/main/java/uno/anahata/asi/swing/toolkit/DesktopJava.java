/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.toolkit;

import java.util.ArrayList;
import java.util.List;
import javax.swing.LookAndFeel;
import javax.swing.UIManager;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.message.RagMessage;
import uno.anahata.asi.agi.tool.ToolContext;
import uno.anahata.asi.swing.agi.tool.DesktopAgiTool;
import uno.anahata.asi.toolkit.java.Java;
import uno.anahata.asi.agi.tool.AgiToolkit;
import uno.anahata.asi.swing.AbstractSwingAsiContainer;
import uno.anahata.asi.swing.agi.AgiPanel;
import uno.anahata.asi.swing.agi.SwingAgiConfig;

/**
 * An extension of the {@link Java} toolkit that provides Desktop java-specific
 * execution utilities, such as EDT synchronization with context propagation.
 * <p>
 * This toolkit ensures that code running on the Event Dispatch Thread (EDT) can
 * still access the tool execution context (logs, errors, attachments) by
 * capturing and re-applying the thread-local state.
 * </p>
 *
 * @author anahata
 */
@Slf4j
@AgiToolkit("A Java Desktop aware Java toolkit that supports EDT synchronization with context propagation.")
public class DesktopJava extends Java {

    /**
     * Constructs a new instance of the toolkit and adds DesktopAgiTool to the
     * list of parentFirstClasses.
     */
    public DesktopJava() {
        registerParentFirstClass(SwingAgiConfig.class);
        registerParentFirstClass(DesktopAgiTool.class);
        registerParentFirstClass(AbstractSwingAsiContainer.class);
        registerParentFirstClass(AgiPanel.class);
    }

    /**
     * {@inheritDoc}
     * <p>
     * Injects Desktop java specific execution helpers into the system
     * instructions, enabling the model to use {@code runInEdt} and
     * {@code runInEdtAndWait} for safe UI interactions.
     * </p>
     *
     * @return the list of system instruction blocks.
     * @throws Exception if an error occurs while assembling instructions.
     */
    @Override
    public List<String> getSystemInstructions() throws Exception {
        List<String> instructions = new ArrayList<>(super.getSystemInstructions());

        StringBuilder sb = new StringBuilder();
        sb.append("\n**Swing & EDT Execution Helpers (Context Propagation)**\n");
        sb.append("You have direct access to these Swing execution helpers (inherited from `" + getConcreteClassModelShouldExtend() + "`):\n\n");
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

        String fxVer = AbstractSwingAsiContainer.getJavaFxVersionInfo();
        if (fxVer != null) {
            sb.append("\n**Pure JavaFX Applications (Native Stage)**:\n");
            sb.append("JavaFX runtime is active (v").append(fxVer).append(") and pre-initialized with `Platform.setImplicitExit(false)` so your windows remain active across turns. You can launch a native JavaFX `Stage` directly without wrapping it in a Swing `JFrame` or `JFXPanel`:\n\n");
            sb.append("```java\n");
            sb.append("final ToolContext ctx = getToolContext(); // Capture tool context for the JavaFX thread!\n");
            sb.append("final String modelId = getModelId(); // Resolve on tool execution thread!\n\n");
            sb.append("Platform.runLater(() -> {\n");
            sb.append("    try {\n");
            sb.append("        Stage stage = new Stage();\n");
            sb.append("        stage.setTitle(\"My 3D App - \" + modelId);\n");
            sb.append("        stage.setScene(scene);\n");
            sb.append("        stage.show();\n");
            sb.append("        ctx.log(\"JavaFX Stage launched successfully.\");\n");
            sb.append("    } catch (Throwable t) {\n");
            sb.append("        ctx.error(\"Failed to launch JavaFX Stage: \" + t.getMessage());\n");
            sb.append("    }\n");
            sb.append("});\n");
            sb.append("```\n");
        }

        instructions.add(sb.toString());
        return instructions;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Augments the RAG message with the active JavaFX runtime status if available.
     * </p>
     *
     * @param ragMessage the incoming RAG message to populate.
     * @throws Exception if an error occurs during message population.
     */
    @Override
    public void populateMessage(RagMessage ragMessage) throws Exception {
        super.populateMessage(ragMessage);
        LookAndFeel laf = UIManager.getLookAndFeel();
        if (laf != null) {
            ragMessage.addTextPart("\nActive Swing Look & Feel: " + laf.getClass().getName() + " (" + laf.getName() + ")");
        }
        String fxVer = AbstractSwingAsiContainer.getJavaFxVersionInfo();
        if (fxVer != null) {
            ragMessage.addTextPart("\nJavaFX Runtime Status: Active (" + fxVer + ", Platform.setImplicitExit(false))");
        }
    }

    /**
     * {@inheritDoc}
     * <p>
     * Configures the code generator to produce classes extending
     * {@link DesktopAgiTool}, which provides the necessary plumbing for
     * EDT-aware context propagation.
     * </p>
     */
    @Override
    protected Class<? extends ToolContext> getConcreteClassModelShouldExtend() {
        return DesktopAgiTool.class;
    }

}
