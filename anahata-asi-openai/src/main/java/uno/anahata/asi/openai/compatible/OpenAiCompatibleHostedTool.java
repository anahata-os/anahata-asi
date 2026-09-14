/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.openai.compatible;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

/**
 * Enumeration of native server-side tools provided by OpenAI via the Responses API.
 */
@Getter
@RequiredArgsConstructor
public enum OpenAiCompatibleHostedTool {
    /** Searches the web using OpenAI's native tool. */
    WEB_SEARCH("web_search", "Search the web using OpenAI's native tool."),
    /** Executes Python code in a sandboxed environment. */
    CODE_INTERPRETER("code_interpreter", "Execute Python code in a sandboxed environment."),
    /** Interacts with a virtual computer environment. */
    COMPUTER_USE("computer_use", "Interact with a virtual computer environment."),
    /** Generates images using DALL-E. */
    IMAGE_GENERATION("image_generation", "Generate images using DALL-E.");

    /** The unique hosted tool identifier. */
    private final String id;
    /** The human-readable description. */
    private final String description;
}
