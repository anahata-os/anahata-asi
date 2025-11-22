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
import java.lang.reflect.Type;
import java.util.Map;
import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.api.Test;
import uno.anahata.ai.model.tool.MockComplexObject;
import uno.anahata.ai.model.tool.Tree;
import uno.anahata.ai.model.tool.TreeNode;
import uno.anahata.ai.model.tool.java.JavaMethodToolResponse;

/**
 * A robust, modern test suite for the SchemaProvider, built from verified outputs.
 * This class avoids brittle string comparisons in favor of structural JSON assertions.
 *
 * @author anahata-ai
 */
public class SchemaProviderTest {
    private static final Gson GSON = new Gson();
    private static final Type MAP_TYPE = new TypeToken<Map<String, Object>>() {}.getType();

    @Test
    public void testSimpleTypeSchema() throws Exception {
        String schemaJson = SchemaProvider.generateInlinedSchemaString(String.class);
        assertNotNull(schemaJson);
        Map<String, Object> schema = GSON.fromJson(schemaJson, MAP_TYPE);
        assertEquals(String.class.getName(), schema.get("title"));
        assertEquals("string", schema.get("type"));
    }

    @Test
    public void testComplexObjectSchema() throws Exception {
        String schemaJson = SchemaProvider.generateInlinedSchemaString(MockComplexObject.class);
        assertNotNull(schemaJson);
        Map<String, Object> schema = GSON.fromJson(schemaJson, MAP_TYPE);
        assertEquals(MockComplexObject.class.getName(), schema.get("title"));
        Map<String, Object> properties = (Map<String, Object>) schema.get("properties");
        assertNotNull(properties);
        assertTrue(properties.containsKey("primitiveField"));
        assertTrue(properties.containsKey("stringField"));
        assertTrue(properties.containsKey("listField"));
        assertTrue(properties.containsKey("nestedObject"));
        assertEquals(String.class.getName(), ((Map<String, Object>) properties.get("stringField")).get("title"));
    }

    @Test
    public void testRecursiveObjectSchema() throws Exception {
        String schemaJson = SchemaProvider.generateInlinedSchemaString(Tree.class);
        assertNotNull(schemaJson);
        Map<String, Object> schema = GSON.fromJson(schemaJson, MAP_TYPE);
        assertEquals(Tree.class.getName(), schema.get("title"));
        Map<String, Object> root = (Map<String, Object>) ((Map<String, Object>) schema.get("properties")).get("root");
        Map<String, Object> children = (Map<String, Object>) ((Map<String, Object>) root.get("properties")).get("children");
        Map<String, Object> items = (Map<String, Object>) children.get("items");
        assertTrue(((String) items.get("description")).startsWith("Recursive reference to " + TreeNode.class.getName()));
    }

    @Test
    public void testWrappedRecursiveObjectSchema() throws Exception {
        String schemaJson = SchemaProvider.generateInlinedSchemaString(JavaMethodToolResponse.class, "result", Tree.class);
        assertNotNull(schemaJson);
        Map<String, Object> schema = GSON.fromJson(schemaJson, MAP_TYPE);
        assertEquals(JavaMethodToolResponse.class.getName(), schema.get("title"));
        Map<String, Object> result = (Map<String, Object>) ((Map<String, Object>) schema.get("properties")).get("result");
        assertEquals(Tree.class.getName(), result.get("title"));
        Map<String, Object> root = (Map<String, Object>) ((Map<String, Object>) result.get("properties")).get("root");
        Map<String, Object> children = (Map<String, Object>) ((Map<String, Object>) root.get("properties")).get("children");
        Map<String, Object> items = (Map<String, Object>) children.get("items");
        assertTrue(((String) items.get("description")).startsWith("Recursive reference to " + TreeNode.class.getName()));
    }
}
