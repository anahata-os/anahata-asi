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
package uno.anahata.ai.model.context;

/**
 * Defines the refresh policy for a {@link StatefulResource} when its content
 * is provided to the model via the Augmented Workspace.
 *
 * @author anahata-gemini-pro-2.5
 */
public enum RefreshPolicy {
    /**
     * The resource's content is captured once when it is first loaded.
     * This snapshot is always provided to the model, even if the underlying
     * resource on disk changes. Ideal for historical data like a git diff.
     */
    SNAPSHOT,

    /**
     * The resource is checked for modifications on every turn. If the on-disk
     * version is newer than the version in context, it is automatically
     * reloaded before being provided to the model. Ideal for source files
     * under active development.
     */
    LIVE;
}
