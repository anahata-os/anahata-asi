/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.gemini.adapter;

import com.google.genai.types.FunctionDeclaration;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;
import uno.anahata.asi.AbstractAsiContainer;
import uno.anahata.ai.tool.MockAsiContainer;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.agi.AgiConfig;
import uno.anahata.asi.agi.tool.spi.AbstractTool;
import uno.anahata.asi.agi.tool.spi.java.JavaObjectToolkit;
import uno.anahata.asi.agi.tool.ToolManager;
import uno.anahata.asi.agi.tool.AgiToolkit;
import uno.anahata.asi.agi.tool.AgiTool;

/**
 * A verification suite for the {@link GeminiFunctionDeclarationAdapter}, ensuring
 * accurate translation between Anahata tools and Google GenAI function declarations.
 * <p>
 * This class tests both native Google API objects and raw JSON schema output,
 * covering standard parameters, void methods, and response wrapping logic.
 * </p>
 * @author anahata-ai
 */
public class GeminiFunctionDeclarationAdapterTest {

    /**
     * A mock toolkit used to provide varied method signatures for adapter testing.
     */
    @AgiToolkit("TestToolkit description")
    public static class TestToolkit {
        /**
         * A standard method with parameters and a return value.
         * @param param1 A string parameter.
         * @param param2 An integer parameter.
         * @return A concatenated result.
         */
        @AgiTool("A test method")
        public String testMethod(String param1, int param2) {
            return param1 + param2;
        }

        /**
         * A method with no return value, used to verify void-response handling.
         */
        @AgiTool("A void method")
        public void voidMethod() {
        }
    }

    /**
     * Verifies translation to native Google {@link FunctionDeclaration} objects
     * with wrapped response schemas enabled.
     * @throws Exception If tool lookup or adaptation fails.
     */
    @Test
    public void testToGoogleNative() throws Exception {
        AbstractTool<?, ?> tool = getTool("testMethod", true);
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
        
        // Verify response (expects wrapped structure)
        assertTrue(func.response().isPresent());
        assertEquals(com.google.genai.types.Type.Known.STRING, func.response().get().properties().get().get("result").type().get().knownEnum());
    }

    /**
     * Ensures that raw (unwrapped) return types are correctly mapped to
     * primitive schemas in native mode.
     * @throws Exception If tool lookup or adaptation fails.
     */
    @Test
    public void testToGoogleNativeRawSchema() throws Exception {
        // When wrapping is false, the response schema is the raw return type (String)
        AbstractTool<?, ?> tool = getTool("testMethod", false);
        GeminiFunctionDeclarationAdapter adapter = new GeminiFunctionDeclarationAdapter(tool, true);
        
        FunctionDeclaration func = adapter.toGoogle();
        
        assertNotNull(func);
        assertTrue(func.response().isPresent());
        // Should be a simple STRING type, not an OBJECT with properties
        assertEquals(com.google.genai.types.Type.Known.STRING, func.response().get().type().get().knownEnum());
        assertFalse(func.response().get().properties().isPresent(), "Raw schema should not have properties map");
    }

    /**
     * Validates the adaptation to JSON-based function declarations for
     * non-native client integrations.
     * @throws Exception If tool lookup or adaptation fails.
     */
    @Test
    public void testToGoogleJson() throws Exception {
        AbstractTool<?, ?> tool = getTool("testMethod", true);
        GeminiFunctionDeclarationAdapter adapter = new GeminiFunctionDeclarationAdapter(tool, false);
        
        FunctionDeclaration func = adapter.toGoogle();
        
        assertNotNull(func);
        assertEquals("TestToolkit.testMethod", func.name().get());
        
        // In JSON mode, parameters are in parametersJsonSchema
        assertTrue(func.parametersJsonSchema().isPresent());
        assertTrue(func.parametersJsonSchema().get() instanceof Map);
        
        Map<String, Object> params = (Map<String, Object>) func.parametersJsonSchema().get();
        assertEquals("object", params.get("type"));
        
        // Verify response (expects wrapped structure)
        assertTrue(func.responseJsonSchema().isPresent());
        Map<String, Object> response = (Map<String, Object>) func.responseJsonSchema().get();
        Map<String, Object> props = (Map<String, Object>) response.get("properties");
        assertTrue(props.containsKey("result"));
    }

    /**
     * Verifies that void methods correctly omit the 'result' property from
     * native response schemas to prevent model confusion.
     * @throws Exception If tool lookup or adaptation fails.
     */
    @Test
    public void testVoidMethodOmitResultNative() throws Exception {
        AbstractTool<?, ?> tool = getTool("voidMethod", true);
        GeminiFunctionDeclarationAdapter adapter = new GeminiFunctionDeclarationAdapter(tool, true);
        
        FunctionDeclaration func = adapter.toGoogle();
        
        assertNotNull(func);
        assertTrue(func.response().isPresent());
        
        // For void methods, the 'result' property should be absent from the response schema
        Map<String, com.google.genai.types.Schema> props = func.response().get().properties().orElse(Map.of());
        assertFalse(props.containsKey("result"), "Response schema should not contain 'result' for void methods");
        
        List<String> required = func.response().get().required().orElse(List.of());
        assertFalse(required.contains("contains result' for void methods"));
    }

    /**
     * Ensures the deterministic omission of 'result' in void-method JSON
     * schemas for consistent tool calling.
     * @throws Exception If tool lookup or adaptation fails.
     */
    @Test
    public void testVoidMethodOmitResultJson() throws Exception {
        AbstractTool<?, ?> tool = getTool("voidMethod", true);
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

    /**
     * Orchestrates a mock AGI environment to retrieve a specific tool
     * instance for testing.
     * @param methodName The simple name of the method to lookup.
     * @param wrap Whether response wrapping should be enabled in the manager.
     * @return The adapted {@link AbstractTool} instance.
     * @throws Exception If agi initialization or tool lookup fails.
     */
    private AbstractTool<?, ?> getTool(String methodName, boolean wrap) throws Exception {
        AbstractAsiContainer container = new MockAsiContainer("test");
        AgiConfig config = new AgiConfig(container) {
            @Override public List<Class<?>> getToolClasses() { return List.of(TestToolkit.class); }
        };
        Agi agi = new Agi(config);
        ToolManager toolManager = agi.getToolManager();
        toolManager.setWrapResponseSchemas(wrap);
        
        JavaObjectToolkit toolkit = new JavaObjectToolkit(toolManager, TestToolkit.class);
        return toolkit.getTools().stream()
                .filter(t -> t.getName().endsWith("." + methodName))
                .findFirst()
                .orElseThrow();
    }
}
