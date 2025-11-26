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
 * Fora Bara!
 */
package uno.anahata.ai.tool;

import com.google.gson.Gson;
import com.google.gson.reflect.TypeToken;
import java.lang.reflect.Type;
import java.util.Map;
import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import uno.anahata.ai.AiConfig;
import uno.anahata.ai.model.tool.java.JavaMethodTool;
import uno.anahata.ai.model.tool.java.JavaMethodToolParameter;

/**
 * Unit tests for the JavaMethodTool class, verifying correct parsing of annotations.
 *
 * @author anahata-ai
 */
public class JavaMethodToolTest {
    private static final Gson GSON = new Gson();
    private static final Type MAP_TYPE = new TypeToken<Map<String, Object>>() {}.getType();
    private static ToolManager toolManager;

    @BeforeAll
    public static void setUp() {
        AiConfig config = new AiConfig("test-app");
        toolManager = new ToolManager(config);
        toolManager.registerClasses(MockToolkit.class);
    }

    @Test
    public void testParameterAnnotationsAreParsedCorrectly() {
        JavaMethodTool sayHelloTool = (JavaMethodTool) toolManager.getAllTools().stream()
            .filter(t -> t.getName().equals("MockToolkit.sayHello"))
            .findFirst()
            .orElse(null);

        assertNotNull(sayHelloTool, "Could not find the sayHello tool.");
        assertEquals(1, sayHelloTool.getParameters().size());

        JavaMethodToolParameter nameParam = sayHelloTool.getParameters().get(0);
        assertEquals("The name to greet.", nameParam.getDescription());

        Map<String, Object> schema = GSON.fromJson(nameParam.getJsonSchema(), MAP_TYPE);
        assertEquals(String.class.getName(), schema.get("title"));
    }

    @Test
    public void testRetentionPolicyIsInheritedFromToolkit() {
        JavaMethodTool doNothingTool = (JavaMethodTool) toolManager.getAllTools().stream()
            .filter(t -> t.getName().equals("MockToolkit.doNothing"))
            .findFirst()
            .orElse(null);

        assertNotNull(doNothingTool, "Could not find the doNothing tool.");
        // MockToolkit has a retention of 10, which should be inherited.
        assertEquals(10, doNothingTool.getRetentionTurns());
    }
}
