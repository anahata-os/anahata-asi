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
package uno.anahata.ai.model.tool;

import java.util.List;
import lombok.Data;

/**
 * A mock object with a variety of field types, designed to test the richness
 * of the generated JSON schema.
 *
 * @author anahata-gemini-pro-2.5
 */
@Data
public class MockComplexObject {
    /**
     * A primitive integer field used to verify numeric type mapping.
     */
    private int primitiveField;
    /**
     * A standard string field used to verify basic text property mapping.
     */
    private String stringField;
    /**
     * A list of strings used to verify array/collection mapping in the schema.
     */
    private List<String> listField;
    /**
     * A nested object reference used to verify recursive traversal and
     * component definitions.
     */
    private MockNestedObject nestedObject;
}
