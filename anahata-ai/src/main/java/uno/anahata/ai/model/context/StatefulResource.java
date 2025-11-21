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
 * A marker interface for objects that represent a resource with a persistent,
 * trackable state (e.g., a file on disk). This is the lynchpin of the V2
 * context management system.
 *
 * @author anahata-gemini-pro-2.5
 */
public interface StatefulResource {

    /**
     * Gets the unique, stable identifier for this resource (e.g., the absolute file path).
     * This is used as the key for tracking the resource.
     * @return The unique resource ID.
     */
    String getResourceId();

    /**
     * Gets the timestamp of the last modification of the resource's state
     * that is currently loaded in memory.
     * @return The last modified timestamp.
     */
    long getLastModified();

    /**
     * Gets the size of the resource's state currently loaded in memory.
     * @return The size in bytes.
     */
    long getSize();

    /**
     * Gets the refresh policy for this resource, determining how its content
     * is updated in the Augmented Workspace.
     * @return The refresh policy.
     */
    RefreshPolicy getRefreshPolicy();
}
