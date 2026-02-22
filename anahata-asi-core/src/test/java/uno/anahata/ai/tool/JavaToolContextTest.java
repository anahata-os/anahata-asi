/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.ai.tool;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import lombok.extern.slf4j.Slf4j;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import uno.anahata.asi.AsiContainer;
import uno.anahata.asi.chat.Chat;
import uno.anahata.asi.chat.ChatConfig;
import uno.anahata.asi.model.core.AbstractModelMessage;
import uno.anahata.asi.model.core.Response;
import uno.anahata.asi.model.core.ResponseUsageMetadata;
import uno.anahata.asi.model.tool.ToolExecutionStatus;
import uno.anahata.asi.model.tool.java.JavaMethodTool;
import uno.anahata.asi.model.tool.java.JavaMethodToolCall;
import uno.anahata.asi.model.tool.java.JavaObjectToolkit;

/**
 * Tests the thread-local context management and tool execution within the Java toolkit.
 * 
 * @author anahata-gemini-pro-2.5
 */
@Slf4j
public class JavaToolContextTest {

    private Chat chat;
    private JavaObjectToolkit toolkit;

    @BeforeEach
    public void setup() {
        AsiContainer container = new AsiContainer("test-app");
        ChatConfig config = new ChatConfig(container);
        config.getToolClasses().add(MockToolkit.class); // Explicitly register for the test
        chat = new Chat(config);
        // Retrieve the JavaObjectToolkit wrapper by name
        toolkit = (JavaObjectToolkit) chat.getToolManager().getToolkits().get("MockToolkit");
    }

    @Test
    public void testToolContextAccess() {
        JavaMethodTool tool = (JavaMethodTool) toolkit.getTools().stream()
                .filter(t -> t.getName().endsWith("testContextAccess"))
                .findFirst().get();

        MockModelMessage message = new MockModelMessage(chat);
        
        JavaMethodToolCall call = tool.createCall(message, "call-1", Collections.emptyMap());
        
        Assertions.assertEquals(ToolExecutionStatus.PENDING, call.getResponse().getStatus());
        
        call.getResponse().execute();
        
        Assertions.assertEquals(ToolExecutionStatus.EXECUTED, call.getResponse().getStatus());
        Assertions.assertEquals("Success", call.getResponse().getResult());
    }

    /**
     * A minimal mock of AbstractModelMessage for testing.
     */
    private static class MockModelMessage extends AbstractModelMessage<MockResponse> {
        public MockModelMessage(Chat chat) {
            super(chat, "mock-model");
        }
    }

    /**
     * A minimal mock of Response for testing.
     */
    private static class MockResponse extends Response<MockModelMessage> {
        @Override public List<MockModelMessage> getCandidates() { return Collections.emptyList(); }
        @Override public ResponseUsageMetadata getUsageMetadata() { return null; }
        @Override public Optional<String> getPromptFeedback() { return Optional.empty(); }
        @Override public int getTotalTokenCount() { return 0; }
        @Override public String getRawJson() { return "{}"; }
        @Override public String getRawRequestConfigJson() { return "{}"; }
        @Override public String getRawHistoryJson() { return "{}"; }
    }
}
