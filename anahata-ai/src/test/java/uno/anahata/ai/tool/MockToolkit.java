/* Licensed under the Anahata Software License, Version 108 - https://github.com/anahata-os/anahata-ai/blob/main/LICENSE */
package uno.anahata.ai.tool;

import uno.anahata.ai.model.core.TextPart;
import uno.anahata.ai.model.tool.MockComplexObject;
import uno.anahata.ai.model.tool.Tree;

@AiToolkit("A mock toolkit for testing schema generation and context injection.")
public class MockToolkit extends JavaTool {

    @AiTool("Returns a greeting for the given name.")
    public String sayHello(@AIToolParam("The name to greet.") String name) {
        return "Hello, " + name;
    }

    @AiTool("A method that returns a recursive Tree object.")
    public Tree getTree() {
        return new Tree();
    }

    @AiTool("A method with no return value.")
    public void doNothing() {
        // This method does nothing and returns void.
    }
    
    @AiTool("A method that returns a complex object with various field types.")
    public MockComplexObject getComplexObject() {
        return new MockComplexObject();
    }
    
    @AiTool(value = "A tool specifically for testing the JavaTool context.", requiresApproval = false)
    public String testContext(@AIToolParam("A message to add to the logs.") String logMessage) {
        log("This is a log message from inside the tool: " + logMessage);
        attach(new TextPart("This is an attachment from inside the tool."));
        return "Context test completed successfully.";
    }
}