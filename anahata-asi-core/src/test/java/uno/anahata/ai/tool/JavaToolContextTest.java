/* Licensed under the Anahata Software License, Version 108 - https://github.com/anahata-os/anahata-ai/blob/main/LICENSE */
package uno.anahata.ai.tool;

import uno.anahata.asi.tool.ToolManager;
import java.util.Collections;
import java.util.Map;
import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import uno.anahata.asi.AsiConfig;
import uno.anahata.asi.chat.Chat;
import uno.anahata.asi.chat.ChatConfig;
import uno.anahata.asi.model.core.AbstractModelMessage;
import uno.anahata.asi.model.core.AbstractToolMessage;
import uno.anahata.asi.model.core.TextPart;
import uno.anahata.asi.model.tool.AbstractTool;
import uno.anahata.asi.model.tool.ToolExecutionStatus;
import uno.anahata.asi.model.tool.java.JavaMethodToolCall;
import uno.anahata.asi.model.tool.java.JavaMethodToolResponse;

/**
 * Unit test for the JavaTool context injection mechanism.
 *
 * @author anahata-ai
 */
public class JavaToolContextTest {

    private Chat chat;

    @BeforeEach
    public void setUp() {
        AsiConfig aiConfig = new AsiConfig("test-app");
        ChatConfig chatConfig = new ChatConfig(aiConfig, "test-session");
        chatConfig.getToolClasses().add(MockToolkit.class);
        chat = new Chat(chatConfig);
    }

    @Test
    public void testJavaToolContextInjection() {
        // 1. Get the ToolManager from the Chat
        ToolManager toolManager = chat.getToolManager();
        
        // 2. Find the tool using the public API
        AbstractTool<?, ?> tool = toolManager.getAllTools().stream()
            .filter(t -> t.getName().equals("MockToolkit.testContext"))
            .findFirst()
            .orElseThrow(() -> new AssertionError("Tool 'MockToolkit.testContext' not found"));

        // 3. Create a mock message context for the call
        AbstractModelMessage mockModelMessage = new AbstractModelMessage(chat, "mock-model") {
            @Override
            protected AbstractToolMessage createToolMessage() {
                // A simple anonymous implementation for the test
                return new AbstractToolMessage(this) {};
            }
        };

        // 4. Create a call with the new signature
        String testMessage = "Hello from the test!";
        Map<String, Object> args = Collections.singletonMap("logMessage", testMessage);
        JavaMethodToolCall call = (JavaMethodToolCall) tool.createCall(mockModelMessage, "test-call-1", args);

        // 5. Get the response and execute it
        JavaMethodToolResponse response = call.getResponse();
        response.execute();

        // 6. Assert the results
        assertEquals(ToolExecutionStatus.EXECUTED, response.getStatus(), "Tool should have executed successfully.");
        assertEquals("Context test completed successfully.", response.getResult(), "The tool should return the correct success message.");

        // 7. Assert the context-injected actions
        assertFalse(response.getLogs().isEmpty(), "Logs should not be empty.");
        assertEquals("This is a log message from inside the tool: " + testMessage, response.getLogs().get(0), "The log message should match.");

        // assertFalse(response.getAttachments().isEmpty(), "Attachments should not be empty.");
        // assertTrue(response.getAttachments().get(0) instanceof TextPart, "The attachment should be a TextPart.");
        // assertEquals("This is an attachment from inside the tool.", ((TextPart) response.getAttachments().get(0)).getText(), "The attachment content should match.");
        
        System.out.println("JavaTool context test passed successfully.");
    }
}
