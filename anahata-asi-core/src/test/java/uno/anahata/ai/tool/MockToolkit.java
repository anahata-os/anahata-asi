/* Licensed under the Anahata Software License, Version 108 - https://github.com/anahata-os/anahata-ai/blob/main/LICENSE */
package uno.anahata.ai.tool;

import uno.anahata.asi.agi.tool.AnahataToolkit;
import java.util.List;
import uno.anahata.ai.model.tool.MockComplexObject;
import uno.anahata.ai.model.tool.Tree;
import uno.anahata.asi.agi.tool.spi.java.JavaMethodToolResponse;
import uno.anahata.asi.agi.tool.AgiToolkit;
import uno.anahata.asi.agi.tool.AgiToolParam;
import uno.anahata.asi.agi.tool.AgiTool;
import uno.anahata.asi.agi.tool.ToolPermission;

/**
 * A mock toolkit used to verify schema generation, context injection, and
 * permission handling within the core tooling engine.
 * <p>
 * This toolkit serves as a baseline for unit tests, providing varied method
 * signatures and return types to ensure the SPI layer correctly translates
 * Java members into the AI-consumable model.
 * </p>
 */
@AgiToolkit(value = "A mock toolkit for testing schema generation and context injection.", maxDepth = 10)
public class MockToolkit extends AnahataToolkit {

    /**
     * Returns a greeting for the given name. Used to verify single-parameter
     * mapping and string return types.
     * @param name The name to greet.
     * @return A formatted greeting string.
     */
    @AgiTool("Returns a greeting for the given name.")
    public String sayHello(@AgiToolParam("The name to greet.") String name) {
        return "Hello, " + name;
    }   

    /**
     * Retrieves a recursive tree structure. Used to verify object-graph
     * traversal and recursive schema generation.
     * @return A new {@link Tree} instance.
     */
    @AgiTool("A method that returns a recursive Tree object.")
    public Tree getTree() {
        return new Tree();
    }

    /**
     * A tool with no return value. Used to verify void-handling in tool
     * execution and response generation.
     */
    @AgiTool("A method with no return value.")
    public void doNothing() {
        // This method does nothing and returns void.
    }
    
    /**
     * Returns a list of strings. Used to verify collection/array mapping
     * in the tool schema.
     * @return A list of strings (always returns {@code null} for testing).
     */
    @AgiTool("Returns a List<String>")
    public List<String> getStringList() {
        return null;
        // This method does nothing and returns null.
    }
    
    /**
     * Retrieves a complex mock object with varied field types.
     * Used to verify multi-level object mapping and type coercion.
     * @return A new {@link MockComplexObject} instance.
     */
    @AgiTool("A method that returns a complex object with various field types.")
    public MockComplexObject getComplexObject() {
        return new MockComplexObject();
    }
    
    /**
     * A tool specifically for testing the {@code ToolContext} access and
     * thread-local logging within the toolkit.
     * @param logMessage A message to add to the tool logs.
     * @return A status string indicating successful context access.
     */
    @AgiTool(value = "A tool specifically for testing the JavaTool context.", permission = ToolPermission.APPROVE_ALWAYS)
    public String testContext(@AgiToolParam("A message to add to the logs.") String logMessage) {
        log("This is a log message from inside the tool: " + logMessage);
        return "Context test completed successfully.";
    }

    /**
     * Directly verifies that the current {@code JavaMethodToolResponse} is
     * accessible via the thread-local proxy.
     * @return {@code "Success"} if the context is present, otherwise {@code "Failure"}.
     */
    @AgiTool(value = "A tool for testing context access.", permission = ToolPermission.APPROVE_ALWAYS)
    public String testContextAccess() {
        if (JavaMethodToolResponse.getCurrent() != null) {
            return "Success";
        }
        return "Failure";
    }
}
