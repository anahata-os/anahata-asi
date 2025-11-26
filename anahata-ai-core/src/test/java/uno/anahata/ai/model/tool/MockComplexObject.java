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
    private int primitiveField;
    private String stringField;
    private List<String> listField;
    private MockNestedObject nestedObject;
}
