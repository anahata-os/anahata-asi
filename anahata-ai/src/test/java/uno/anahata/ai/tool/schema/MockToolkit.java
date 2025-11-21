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

import uno.anahata.ai.tool.AIToolParam;
import uno.anahata.ai.tool.AiTool;
import uno.anahata.ai.tool.AiToolkit;

@AiToolkit("A mock toolkit for testing schema generation.")
public class MockToolkit {

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
}
