/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.gemini.adapter;

import com.google.genai.types.FunctionDeclaration;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;
import uno.anahata.asi.AsiContainer;
import uno.anahata.asi.model.resource.AbstractResource;
import uno.anahata.asi.chat.Chat;
import uno.anahata.asi.chat.ChatConfig;
import uno.anahata.asi.model.tool.AbstractTool;
import uno.anahata.asi.model.tool.java.JavaObjectToolkit;
import uno.anahata.asi.tool.AiTool;
import uno.anahata.asi.tool.AiToolkit;
import uno.anahata.asi.tool.ToolManager;

public class GeminiFunctionDeclarationAdapterTest {

    @AiToolkit("TestToolkit description")
    public static class TestToolkit {
        @AiTool("A test method")
        public String testMethod(String param1, int param2) {
            return param1 + param2;
        }

        @AiTool("A void method")
        public void voidMethod() {
        }
    }

    @Test
    public void testToGoogleNative() throws Exception {
        AbstractTool<?, ?> tool = getTool("testMethod");
        GeminiFunctionDeclarationAdapter adapter = new GeminiFunctionDeclarationAdapter(tool, true);
        
        FunctionDeclaration func = adapter.toGoogle();
        
        assertNotNull(func);
        assertEquals("TestToolkit.testMethod", func.name().get());
        assertTrue(func.description().get().startsWith("A test method"));
        
        // Verify parameters
        assertTrue(func.parameters().isPresent());
        assertEquals(com.google.genai.types.Type.Known.OBJECT, func.parameters().get().type().get().knownEnum());
        assertTrue(func.parameters().get().properties().get().containsKey("param1"));
        assertTrue(func.parameters().get().properties().get().containsKey("param2"));
        
        // Verify response
        assertTrue(func.response().isPresent());
        assertEquals(com.google.genai.types.Type.Known.STRING, func.response().get().properties().get().get("result").type().get().knownEnum());
    }

    @Test
    public void testToGoogleJson() throws Exception {
        AbstractTool<?, ?> tool = getTool("testMethod");
        GeminiFunctionDeclarationAdapter adapter = new GeminiFunctionDeclarationAdapter(tool, false);
        
        FunctionDeclaration func = adapter.toGoogle();
        
        assertNotNull(func);
        assertEquals("TestToolkit.testMethod", func.name().get());
        
        // In JSON mode, parameters are in parametersJsonSchema
        assertTrue(func.parametersJsonSchema().isPresent());
        assertTrue(func.parametersJsonSchema().get() instanceof Map);
        
        Map<String, Object> params = (Map<String, Object>) func.parametersJsonSchema().get();
        assertEquals("object", params.get("type"));
        
        // Verify response
        assertTrue(func.responseJsonSchema().isPresent());
        Map<String, Object> response = (Map<String, Object>) func.responseJsonSchema().get();
        Map<String, Object> props = (Map<String, Object>) response.get("properties");
        assertTrue(props.containsKey("result"));
    }

    @Test
    public void testVoidMethodOmitResultNative() throws Exception {
        AbstractTool<?, ?> tool = getTool("voidMethod");
        GeminiFunctionDeclarationAdapter adapter = new GeminiFunctionDeclarationAdapter(tool, true);
        
        FunctionDeclaration func = adapter.toGoogle();
        
        assertNotNull(func);
        assertTrue(func.response().isPresent());
        
        // For void methods, the 'result' property should be absent from the response schema
        Map<String, com.google.genai.types.Schema> props = func.response().get().properties().orElse(Map.of());
        assertFalse(props.containsKey("result"), "Response schema should not contain 'result' for void methods");
        
        List<String> required = func.response().get().required().orElse(List.of());
        assertFalse(required.contains("result"), "Response schema should not require 'result' for void methods");
    }

    @Test
    public void testVoidMethodOmitResultJson() throws Exception {
        AbstractTool<?, ?> tool = getTool("voidMethod");
        GeminiFunctionDeclarationAdapter adapter = new GeminiFunctionDeclarationAdapter(tool, false);
        
        FunctionDeclaration func = adapter.toGoogle();
        
        assertNotNull(func);
        assertTrue(func.responseJsonSchema().isPresent());
        
        Map<String, Object> response = (Map<String, Object>) func.responseJsonSchema().get();
        Map<String, Object> props = (Map<String, Object>) response.get("properties");
        if (props != null) {
            assertFalse(props.containsKey("result"), "Response schema should not contain 'result' for void methods");
        }
        
        List<String> required = (List<String>) response.get("required");
        if (required != null) {
            assertFalse(required.contains("result"), "Response schema should not require 'result' for void methods");
        }
    }

    private AbstractTool<?, ?> getTool(String methodName) throws Exception {
        AsiContainer container = new AsiContainer("test") {
            @Override public Chat createNewChat() { return new Chat(new ChatConfig(this)); }
            @Override public void openResource(AbstractResource<?, ?> resource) {}
        };
        ChatConfig config = new ChatConfig(container) {
            @Override public List<Class<?>> getToolClasses() { return List.of(TestToolkit.class); }
        };
        Chat chat = new Chat(config);
        ToolManager toolManager = new ToolManager(chat);
        JavaObjectToolkit toolkit = new JavaObjectToolkit(toolManager, TestToolkit.class);
        return toolkit.getTools().stream()
                .filter(t -> t.getName().endsWith("." + methodName))
                .findFirst()
                .orElseThrow();
    }
}
