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

import com.fasterxml.jackson.core.type.TypeReference;
import java.util.Map;
import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import uno.anahata.ai.AiConfig;
import uno.anahata.ai.model.tool.AbstractTool;
import uno.anahata.ai.tool.MockToolkit;
import uno.anahata.ai.tool.ToolManager;

public class SchemaGenerationTest {
    private static final TypeReference<Map<String, Object>> MAP_TYPE_REF = new TypeReference<>() {};
    private static ToolManager toolManager;

    @BeforeAll
    public static void setUp() {
        AiConfig config = new AiConfig("test-app");
        toolManager = new ToolManager(config);
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
            assertTrue(properties.containsKey("attachments"), "Schema must contain 'attachments' property.");

            // Check for 'result' property based on whether the method is void or not
            // A bit of a hack, but effective for testing without exposing return types directly.
            boolean isVoid = tool.getName().endsWith("doNothing");

            if (isVoid) {
                // This is a void method, so the 'result' property should be absent.
                assertFalse(properties.containsKey("result"), "Void method schema should not contain 'result' property.");
            } else {
                // This is a non-void method, so the 'result' property should be present.
                assertTrue(properties.containsKey("result"), "Non-void method schema must contain 'result' property.");
                assertNotNull(properties.get("result"), "The 'result' property should not be null for a non-void method.");
            }
        }
    }
}
