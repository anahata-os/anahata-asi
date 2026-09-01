/*
 * Copyright 2025 Anahata.
 *
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.ai.tool;

import uno.anahata.asi.agi.tool.ToolManager;
import com.fasterxml.jackson.core.type.TypeReference;
import java.util.Map;
import lombok.SneakyThrows;
import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import uno.anahata.asi.AbstractAsiContainer;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.agi.AgiConfig;
import uno.anahata.asi.agi.tool.spi.java.JavaMethodTool;
import uno.anahata.asi.agi.tool.spi.java.JavaMethodToolParameter;
import uno.anahata.asi.agi.tool.schema.SchemaProvider;

/**
 * Unit tests for the JavaMethodTool class, verifying correct parsing of annotations.
 *
 * @author anahata-ai
 */
public class JavaMethodToolTest {
    /**
     * A reference to the Jackson type for a standard map, used for
     * schema deserialization verification.
     */
    private static final TypeReference<Map<String, Object>> MAP_TYPE_REF = new TypeReference<>() {};
    /**
     * The active AGI session used for tool lookup.
     */
    private static Agi agi;
    /**
     * The manager instance being tested.
     */
    private static ToolManager toolManager;

    /**
     * Initializes the test environment with a mock container and toolkit.
     */
    @BeforeAll
    @SneakyThrows
    public static void setUp() {
        AbstractAsiContainer container = new MockAsiContainer("test-app");
        AgiConfig config = new AgiConfig(container, "test-session");
        config.getToolClasses().add(MockToolkit.class);
        agi = new Agi(config);
        toolManager = agi.getToolManager();
    }

    /**
     * Verifies that @AgiToolParam descriptions and schemas are correctly
     * extracted from Java methods.
     * @throws Exception If schema parsing fails.
     */
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

    /**
     * Ensures that tools inherit the maximum execution depth from their
     * parent toolkit.
     */
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
