/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.openai.compatible;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

/**
 * Enumeration of native server-side tools provided by OpenAI via the Responses API.
 */
@Getter
@RequiredArgsConstructor
public enum OpenAiHostedTool {
    WEB_SEARCH("web_search", "Search the web using OpenAI's native tool."),
    CODE_INTERPRETER("code_interpreter", "Execute Python code in a sandboxed environment."),
    COMPUTER_USE("computer_use", "Interact with a virtual computer environment."),
    IMAGE_GENERATION("image_generation", "Generate images using DALL-E.");

    private final String id;
    private final String description;
}
