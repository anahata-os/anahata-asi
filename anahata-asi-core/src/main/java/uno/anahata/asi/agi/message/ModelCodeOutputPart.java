/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.agi.message;

import lombok.Getter;
import lombok.Setter;

/**
 * A specialized part representing the output or logs from a server-side code execution.
 * <p>
 * This part stores the textual results (stdout/stderr) returned by the execution 
 * environment. Visual outputs (like images) should be captured as {@link ModelBlobPart}s 
 * associated with the same message turn.
 * </p>
 * 
 * @author anahata
 */
@Getter
@Setter
public class ModelCodeOutputPart extends ModelTextPart {

    /**
     * Constructs a new ModelCodeOutputPart.
     * 
     * @param message The model message containing the execution results.
     * @param logs The textual output or logs from the execution.
     * @param thoughtSignature The signature of the thought process associated with this output, if any.
     */
    public ModelCodeOutputPart(AbstractModelMessage message, String logs, byte[] thoughtSignature) {
        super(message, logs, thoughtSignature, false);
    }

    /**
     * {@inheritDoc}
     * <p>
     * Returns a formatted string indicating the output of a hosted code execution.
     * </p>
     */
    @Override
    public String asText() {
        return String.format("[Hosted Code Output]\n%s", getText());
    }
}
