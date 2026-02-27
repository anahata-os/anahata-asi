/*
 * Copyright 2025 Anahata.
 *
 * Licensed under the Anahata Software License (ASL) V2.0;
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      https://github.com/anahata-anahata/anahata-ai-parent/blob/main/LICENSE
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

import uno.anahata.asi.tool.schema.SchemaProvider;
import com.fasterxml.jackson.core.type.TypeReference;
import java.util.Map;
import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import uno.anahata.asi.AsiContainer;
import uno.anahata.ai.tool.MockAsiContainer;
import uno.anahata.asi.model.resource.AbstractResource;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.agi.AgiConfig;
import uno.anahata.asi.model.tool.AbstractTool;
import uno.anahata.ai.tool.MockToolkit;
import uno.anahata.asi.tool.ToolManager;
import uno.anahata.asi.model.tool.java.JavaMethodTool;

public class SchemaGenerationTest {
    private static final TypeReference<Map<String, Object>> MAP_TYPE_REF = new TypeReference<>() {};
    private static ToolManager toolManager;

    @BeforeAll
    public static void setUp() {
        AsiContainer container = new MockAsiContainer("test-app");
        AgiConfig config = new AgiConfig(container, "test-session");
        Agi agi = new Agi(config);
        toolManager = agi.getToolManager();
        toolManager.registerClasses(MockToolkit.class);
    }

    @Test
    public void testAllToolSchemasAreCorrectlyWrapped() throws Exception {
        for (AbstractTool<?, ?> tool : toolManager.getAllTools()) {
            String responseSchemaJson = tool.getResponseJsonSchema();
            System.out.println("Verifying schema for tool: " + tool.getName() + "\n" + responseSchemaJson);
            assertNotNull(responseSchemaJson, "Response schema should not be null for tool: " + tool.getName());

            Map<String, Object> responseSchemaMap = SchemaProvider.OBJECT_MAPPER.readValue(responseSchemaJson, MAP_TYPE_REF);
            Map<String, Object> properties = (Map<String, Object>) responseSchemaMap.get("properties");
            assertNotNull(properties, "Response schema must have a 'properties' field.");

            // Assert that standard wrapper fields are present
            assertTrue(properties.containsKey("status"), "Schema must contain 'status' property.");
            assertTrue(properties.containsKey("logs"), "Schema must contain 'logs' property. Schema: " + responseSchemaJson);
            //assertTrue(properties.containsKey("attachments"), "Schema must contain 'attachments' property.");

            // Check for 'result' property based on whether the method is void or not
            boolean isVoid = false;
            if (tool instanceof JavaMethodTool jmt) {
                isVoid = jmt.getMethod().getReturnType().equals(void.class);
            } else {
                isVoid = tool.getName().endsWith("doNothing");
            }

            if (isVoid) {
                // This is a void method, so the 'result' property should be absent.
                assertFalse(properties.containsKey("result"), "Void method schema should not contain 'result' property.");
            } else {
                // This is a non-void method, so the 'result' property should be present.
                assertTrue(properties.containsKey("result"), "Non-void method schema must contain 'result' property. Tool: " + tool.getName());
                assertNotNull(properties.get("result"), "The 'result' property should not be null for a non-void method.");
            }
            
            // Test for recursive reference inlining (specifically for the getTree tool)
            if (tool.getName().endsWith("getTree")) {
                Map<String, Object> resultSchema = (Map<String, Object>) properties.get("result");
                Map<String, Object> treeProps = (Map<String, Object>) resultSchema.get("properties");
                Map<String, Object> rootNodeSchema = (Map<String, Object>) treeProps.get("root");
                Map<String, Object> rootNodeProps = (Map<String, Object>) rootNodeSchema.get("properties");
                Map<String, Object> childrenSchema = (Map<String, Object>) rootNodeProps.get("children");
                Map<String, Object> itemsSchema = (Map<String, Object>) childrenSchema.get("items");
                
                // The 'items' of the children list should be a recursive reference to TreeNode
                assertEquals("uno.anahata.ai.model.tool.TreeNode", itemsSchema.get("title"));
                String description = (String) itemsSchema.get("description");
                assertTrue(description.contains("Recursive reference to uno.anahata.ai.model.tool.TreeNode"), 
                        "Recursive reference description missing or incorrect: " + description);
            }
        }
    }
}
