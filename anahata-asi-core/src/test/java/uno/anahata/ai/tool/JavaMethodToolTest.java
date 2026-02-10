/*
 * Copyright 2025 Anahata.
 *
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.ai.tool;

import uno.anahata.asi.tool.ToolManager;
import com.fasterxml.jackson.core.type.TypeReference;
import java.util.Map;
import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import uno.anahata.asi.AsiContainer;
import uno.anahata.asi.chat.Chat;
import uno.anahata.asi.chat.ChatConfig;
import uno.anahata.asi.model.tool.java.JavaMethodTool;
import uno.anahata.asi.model.tool.java.JavaMethodToolParameter;
import uno.anahata.asi.tool.schema.SchemaProvider;

/**
 * Unit tests for the JavaMethodTool class, verifying correct parsing of annotations.
 *
 * @author anahata-ai
 */
public class JavaMethodToolTest {
    private static final TypeReference<Map<String, Object>> MAP_TYPE_REF = new TypeReference<>() {};
    private static Chat chat;
    private static ToolManager toolManager;

    @BeforeAll
    public static void setUp() {
        AsiContainer container = new AsiContainer("test-app");
        ChatConfig config = new ChatConfig(container, "test-session");
        config.getToolClasses().add(MockToolkit.class);
        chat = new Chat(config);
        toolManager = chat.getToolManager();
    }

    @Test
    public void testParameterAnnotationsAreParsedCorrectly() throws Exception {
        JavaMethodTool sayHelloTool = (JavaMethodTool) toolManager.getAllTools().stream()
            .filter(t -> t.getName().equals("MockToolkit.sayHello"))
            .findFirst()
            .orElse(null);

        assertNotNull(sayHelloTool, "Could not find the sayHello tool.");
        assertEquals(1, sayHelloTool.getParameters().size());

        JavaMethodToolParameter nameParam = sayHelloTool.getParameters().get(0);
        assertEquals("The name to greet.", nameParam.getDescription());

        Map<String, Object> schema = SchemaProvider.OBJECT_MAPPER.readValue(nameParam.getJsonSchema(), MAP_TYPE_REF);
        assertEquals(String.class.getName(), schema.get("title"));
    }

    @Test
    public void testMaxDepthPolicyIsInheritedFromToolkit() {
        JavaMethodTool doNothingTool = (JavaMethodTool) toolManager.getAllTools().stream()
            .filter(t -> t.getName().equals("MockToolkit.doNothing"))
            .findFirst()
            .orElse(null);

        assertNotNull(doNothingTool, "Could not find the doNothing tool.");
        // MockToolkit has a maxDepth of 10, which should be inherited.
        assertEquals(10, doNothingTool.getEffectiveMaxDepth());
    }
}
