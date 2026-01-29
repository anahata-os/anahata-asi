/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.gemini.schema;

import com.google.genai.types.Schema;
import com.google.genai.types.Type;
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

public class GeminiSchemaAdapterTest {

    @Test
    public void testSimpleType() throws Exception {
        Schema schema = GeminiSchemaAdapter.getGeminiSchema(String.class);
        assertNotNull(schema);
        assertEquals(Type.Known.STRING, schema.type().get().knownEnum());
        assertEquals("java.lang.String", schema.title().get());
    }

    @Test
    public void testComplexType() throws Exception {
        Schema schema = GeminiSchemaAdapter.getGeminiSchema(TestObject.class);
        assertNotNull(schema);
        assertEquals(Type.Known.OBJECT, schema.type().get().knownEnum());
        assertEquals("uno.anahata.asi.gemini.schema.GeminiSchemaAdapterTest.TestObject", schema.title().get());
        
        assertTrue(schema.properties().isPresent());
        assertTrue(schema.properties().get().containsKey("name"));
        assertEquals(Type.Known.STRING, schema.properties().get().get("name").type().get().knownEnum());
        
        assertTrue(schema.properties().get().containsKey("age"));
        assertEquals(Type.Known.INTEGER, schema.properties().get().get("age").type().get().knownEnum());
    }

    public static class TestObject {
        private String name;
        private int age;
    }
}
