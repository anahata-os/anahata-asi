/*
 * Copyright 2025 Anahata.
 *
 * Licensed under the Anahata Software License (ASL) V2.0;
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      https://github.com/pablo-anahata/anahata-ai-parent/blob/main/LICENSE
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * Força Barça!
 */
package uno.anahata.ai.tool.schema;

import com.google.gson.Gson;
import com.google.gson.reflect.TypeToken;
import java.util.Map;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import uno.anahata.ai.AiConfig;
import uno.anahata.ai.model.tool.AbstractTool;
import uno.anahata.ai.tool.ToolManager;
import static org.junit.jupiter.api.Assertions.*;

public class SchemaProviderTest {
    private static final Gson GSON = new Gson();
    private static ToolManager toolManager;

    @BeforeAll
    public static void setUp() {
        AiConfig config = new AiConfig("test-app");
        toolManager = new ToolManager(config);
        toolManager.registerClasses(MockToolkit.class);
    }

    private AbstractTool<?, ?> getTool(String methodName) {
        return toolManager.getAllTools().stream()
            .filter(t -> t.getName().endsWith("." + methodName))
            .findFirst()
            .orElseThrow(() -> new AssertionError("Tool not found: " + methodName));
    }

    @Test
    public void testSimpleReturnTypeSchemaIsClean() {
        AbstractTool<?, ?> tool = getTool("sayHello");
        String schema = tool.getResponseJsonSchema();

        assertNotNull(schema, "Schema should not be null for a tool with a return type.");
        assertFalse(schema.contains("\"message\""), "Schema should not contain internal 'message' property.");
        assertFalse(schema.contains("\"call\""), "Schema should not contain internal 'call' property.");
        assertFalse(schema.contains("\"exception\""), "Schema should not contain internal 'exception' property.");
        assertTrue(schema.contains("\"title\": \"java.lang.String\""), "The result schema for String should be correctly injected.");
    }

    @Test
    public void testVoidReturnTypeSchemaIsNull() {
        AbstractTool<?, ?> tool = getTool("doNothing");
        String returnSchema = tool.getReturnTypeJsonSchema();
        String responseSchema = tool.getResponseJsonSchema();

        assertNull(returnSchema, "Return type schema for void method should be null.");
        assertNotNull(responseSchema, "Response schema for void method should still exist.");
        assertFalse(responseSchema.contains("\"result\""), "Response schema for void method should not have a 'result' property.");
    }

    @Test
    public void testRecursiveReturnTypeSchemaIsCleanAndCorrect() {
        AbstractTool<?, ?> tool = getTool("getTree");
        String schema = tool.getResponseJsonSchema();

        assertNotNull(schema, "Schema for recursive type should not be null.");
        assertFalse(schema.contains("\"message\""), "Schema should not contain internal 'message' property.");
        assertTrue(schema.contains("\"title\": \"uno.anahata.ai.tool.schema.Tree\""), "The result schema for Tree should be correctly injected.");
        assertTrue(schema.contains("Recursive reference to uno.anahata.ai.tool.schema.TreeNode"), "Schema should gracefully handle recursion.");
    }
    
    @Test
    public void testSchemaTitlesArePresentForAllTypes() {
        AbstractTool<?, ?> tool = getTool("getComplexObject");
        String schemaJson = tool.getResponseJsonSchema();
        
        assertNotNull(schemaJson, "Schema for complex object should not be null.");
        
        System.out.println("--- Generated Schema for MockComplexObject ---");
        System.out.println(schemaJson);
        System.out.println("--------------------------------------------");
        
        Map<String, Object> schemaMap = GSON.fromJson(schemaJson, new TypeToken<Map<String, Object>>() {}.getType());
        
        // Start the recursive check from the 'result' property within the response schema
        Map<String, Object> properties = (Map<String, Object>) schemaMap.get("properties");
        Map<String, Object> resultSchema = (Map<String, Object>) properties.get("result");
        
        assertTitlesRecursive(resultSchema);
    }

    private void assertTitlesRecursive(Map<String, Object> schemaNode) {
        assertNotNull(schemaNode, "Schema node should not be null.");
        
        // 1. Assert that the current node has a non-empty title
        assertTrue(schemaNode.containsKey("title"), "Every schema node must have a 'title' property. Node: " + schemaNode);
        String title = (String) schemaNode.get("title");
        assertNotNull(title, "Title should not be null.");
        assertFalse(title.trim().isEmpty(), "Title should not be empty.");

        // 2. If it's an object, recurse into its properties
        if (schemaNode.containsKey("properties")) {
            Map<String, Object> properties = (Map<String, Object>) schemaNode.get("properties");
            for (Map.Entry<String, Object> entry : properties.entrySet()) {
                assertTitlesRecursive((Map<String, Object>) entry.getValue());
            }
        }

        // 3. If it's an array, recurse into its items
        if (schemaNode.containsKey("items")) {
            assertTitlesRecursive((Map<String, Object>) schemaNode.get("items"));
        }
    }
}
