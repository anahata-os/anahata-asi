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
import java.lang.reflect.Type;
import java.util.Map;
import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.api.Test;
import uno.anahata.ai.model.tool.MockComplexObject;
import uno.anahata.ai.model.tool.Tree;
import uno.anahata.ai.model.tool.TreeNode;
import uno.anahata.asi.model.tool.java.JavaMethodToolResponse;

/**
 * A robust, modern test suite for the SchemaProvider, built from verified outputs.
 * This class avoids brittle string comparisons in favor of structural JSON assertions.
 *
 * @author anahata-ai
 */
public class SchemaProviderTest {
    private static final TypeReference<Map<String, Object>> MAP_TYPE_REF = new TypeReference<>() {};

    @Test
    public void testSimpleTypeSchema() throws Exception {
        String schemaJson = SchemaProvider.generateInlinedSchemaString(String.class);
        assertNotNull(schemaJson);
        Map<String, Object> schema = SchemaProvider.OBJECT_MAPPER.readValue(schemaJson, MAP_TYPE_REF);
        assertEquals(String.class.getName(), schema.get("title"));
        assertEquals("string", schema.get("type"));
    }

    @Test
    public void testMapSchema() throws Exception {
        Type mapType = new TypeReference<Map<String, String>>() {}.getType();
        String schemaJson = SchemaProvider.generateInlinedSchemaString(mapType);
        assertNotNull(schemaJson);
        Map<String, Object> schema = SchemaProvider.OBJECT_MAPPER.readValue(schemaJson, MAP_TYPE_REF);
        assertEquals("java.util.Map<java.lang.String, java.lang.String>", schema.get("title"));
        assertEquals("object", schema.get("type"));
        Map<String, Object> additionalProperties = (Map<String, Object>) schema.get("additionalProperties");
        assertNotNull(additionalProperties);
        assertEquals("string", additionalProperties.get("type"));
        assertEquals("java.lang.String", additionalProperties.get("title"));
    }

    @Test
    public void testComplexObjectSchema() throws Exception {
        String schemaJson = SchemaProvider.generateInlinedSchemaString(MockComplexObject.class);
        System.out.println("Schema for MockComplexObject.class " + schemaJson);
        assertNotNull(schemaJson);
        Map<String, Object> schema = SchemaProvider.OBJECT_MAPPER.readValue(schemaJson, MAP_TYPE_REF);
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
        Map<String, Object> schema = SchemaProvider.OBJECT_MAPPER.readValue(schemaJson, MAP_TYPE_REF);
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
        Map<String, Object> schema = SchemaProvider.OBJECT_MAPPER.readValue(schemaJson, MAP_TYPE_REF);
        assertEquals(JavaMethodToolResponse.class.getName(), schema.get("title"));
        Map<String, Object> result = (Map<String, Object>) ((Map<String, Object>) schema.get("properties")).get("result");
        assertEquals(Tree.class.getName(), result.get("title"));
        Map<String, Object> root = (Map<String, Object>) ((Map<String, Object>) result.get("properties")).get("root");
        Map<String, Object> children = (Map<String, Object>) ((Map<String, Object>) root.get("properties")).get("children");
        Map<String, Object> items = (Map<String, Object>) children.get("items");
        assertTrue(((String) items.get("description")).startsWith("Recursive reference to " + TreeNode.class.getName()));
    }
}
