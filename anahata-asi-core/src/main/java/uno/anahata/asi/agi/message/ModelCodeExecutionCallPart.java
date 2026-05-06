/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.agi.message;

import lombok.Getter;
import lombok.Setter;

/**
 * A specialized part representing a request from the model to execute code 
 * on the server (e.g., Python in OpenAI's Code Interpreter or Gemini's Code Execution).
 * <p>
 * This part captures the source code and the language intended for execution. 
 * It is used to maintain the narrative of the model's actions in the history, 
 * allowing for accurate replay across different AI providers.
 * </p>
 * 
 * @author anahata
 */
@Getter
@Setter
public class ModelCodeExecutionCallPart extends ModelTextPart {
    
    /**
     * The programming language of the code to be executed (e.g., "python").
     */
    private String language;

    /**
     * Constructs a new ModelCodeCallPart.
     * 
     * @param message The model message that initiated the code execution request.
     * @param code The source code to be executed.
     * @param language The programming language of the code.
     * @param thoughtSignature The signature of the thought process associated with this call, if any.
     */
    public ModelCodeExecutionCallPart(AbstractModelMessage message, String code, String language, byte[] thoughtSignature) {
        super(message, code, thoughtSignature, false);
        this.language = language;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Returns a formatted string indicating a hosted code execution request.
     * </p>
     */
    @Override
    public String asText() {
        return String.format("[Hosted Code Call (%s)]\n%s", language != null ? language : "unknown", getText());
    }
}
