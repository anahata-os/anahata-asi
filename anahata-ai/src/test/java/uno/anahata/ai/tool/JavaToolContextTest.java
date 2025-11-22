/* Licensed under the Anahata Software License, Version 108 - https://github.com/anahata-os/anahata-ai/blob/main/LICENSE */
package uno.anahata.ai.tool;

import java.util.Collections;
import java.util.Map;
import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import uno.anahata.ai.AiConfig;
import uno.anahata.ai.model.core.TextPart;
import uno.anahata.ai.model.tool.AbstractTool;
import uno.anahata.ai.model.tool.ToolExecutionStatus;
import uno.anahata.ai.model.tool.java.JavaMethodToolCall;
import uno.anahata.ai.model.tool.java.JavaMethodToolResponse;

/**
 * Unit test for the JavaTool context injection mechanism.
 *
 * @author anahata-ai
 */
public class JavaToolContextTest {

    private ToolManager toolManager;

    @BeforeEach
    public void setUp() {
        // Use the lightweight constructor for the ToolManager
        toolManager = new ToolManager(new AiConfig("test-app"));
        toolManager.registerClasses(MockToolkit.class);
    }

    @Test
    public void testJavaToolContextInjection() {
        // 1. Find the tool using the public API
        AbstractTool<?, ?> tool = toolManager.getAllTools().stream()
            .filter(t -> t.getName().equals("MockToolkit.testContext"))
            .findFirst()
            .orElseThrow(() -> new AssertionError("Tool 'MockToolkit.testContext' not found"));

        // 2. Create a call
        String testMessage = "Hello from the test!";
        Map<String, Object> args = Collections.singletonMap("logMessage", testMessage);
        JavaMethodToolCall call = (JavaMethodToolCall) tool.createCall("test-call-1", args);

        // 3. Get the response and execute it
        JavaMethodToolResponse response = call.getResponse();
        response.execute();

        // 4. Assert the results
        assertEquals(ToolExecutionStatus.EXECUTED, response.getStatus(), "Tool should have executed successfully.");
        assertEquals("Context test completed successfully.", response.getResult(), "The tool should return the correct success message.");

        // 5. Assert the context-injected actions
        assertFalse(response.getLogs().isEmpty(), "Logs should not be empty.");
        assertEquals("This is a log message from inside the tool: " + testMessage, response.getLogs().get(0), "The log message should match.");

        assertFalse(response.getAttachments().isEmpty(), "Attachments should not be empty.");
        assertTrue(response.getAttachments().get(0) instanceof TextPart, "The attachment should be a TextPart.");
        assertEquals("This is an attachment from inside the tool.", ((TextPart) response.getAttachments().get(0)).getText(), "The attachment content should match.");
        
        System.out.println("JavaTool context test passed successfully.");
    }
}