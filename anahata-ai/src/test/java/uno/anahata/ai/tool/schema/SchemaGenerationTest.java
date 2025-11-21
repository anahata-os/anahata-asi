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

import uno.anahata.ai.AiConfig;
import uno.anahata.ai.model.tool.AbstractTool;
import uno.anahata.ai.tool.ToolManager;

public class SchemaGenerationTest {

    public static void main(String[] args) throws Exception {
        System.out.println("--- Running Schema Generation Test ---");

        AiConfig config = new AiConfig("test-app");
        ToolManager toolManager = new ToolManager(config);
        
        // Register the mock toolkit
        toolManager.registerClasses(MockToolkit.class);

        System.out.println("\nFound " + toolManager.getAllTools().size() + " tools.");

        for (AbstractTool<?, ?> tool : toolManager.getAllTools()) {
            System.out.println("\n========================================");
            System.out.println("Testing Tool: " + tool.getName());
            System.out.println("----------------------------------------");

            System.out.println("\n1. Returned Type JSON Schema:");
            String returnTypeSchema = tool.getReturnTypeJsonSchema();
            System.out.println(returnTypeSchema != null ? returnTypeSchema : "[null - Correct for void]");

            System.out.println("\n2. Full Response JSON Schema:");
            String responseSchema = tool.getResponseJsonSchema();
            System.out.println(responseSchema != null ? responseSchema : "[null - Should not happen for non-void]");
            System.out.println("========================================");
        }
        
        System.out.println("\n--- Test Complete ---");
    }
}
