/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.agi.message.code;

import lombok.Getter;
import lombok.Setter;
import uno.anahata.asi.agi.message.AbstractModelMessage;
import uno.anahata.asi.agi.message.ModelTextPart;

/**
 * A specialized part representing the output or logs from a server-side code execution.
 * <p>
 * This part stores the textual results (stdout/stderr) returned by the execution 
 * environment. Visual outputs (like images) should be captured as {@link uno.anahata.asi.agi.message.ModelBlobPart}s 
 * associated with the same message turn.
 * </p>
 * 
 * @author anahata
 */
@Getter
@Setter
public class HostedCodeExecutionResultPart extends ModelTextPart {
    
    /**
     * The code execution outcomes.
     */
    /**
     * The possible outcomes of a server-side code execution.
     */
    public static enum ModelCodeExectionOutcome {
        /** The execution outcome is unknown or could not be determined. */
        UNKNOWN,
        /** The execution completed successfully with an exit code of 0. */
        OK,
        /** The execution failed or exited with a non-zero status. */
        FAILED,
        /** The execution exceeded the maximum allotted time. */
        TIMEOUT
    }
    
    /**
     * The call part that initiated this execution result, used to reconstruct 
     * parent-child relationships in the conversation tree.
     */
    private HostedCodeExecutionCallPart parentCall;
    
    /**
     * The final state of the execution (OK, FAILED, etc.) as reported by the provider.
     */
    private ModelCodeExectionOutcome outcome;
    
    /**
     * Constructs a new ModelCodeOutputPart.
     * 
     * @param message The model message containing the execution results.
     * @param logs The textual output or logs from the execution.
     * @param thoughtSignature The signature of the thought process associated with this output, if any.
     */
    public HostedCodeExecutionResultPart(AbstractModelMessage message, String logs, byte[] thoughtSignature) {
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

    /**
     * {@inheritDoc}
     * <p>
     * Returns the default maximum depth to keep a hosted code execution result part in context.
     * </p>
     */
    @Override
    protected int getDefaultMaxDepth() {
        return getAgiConfig().getDefaultModelCodeExecutionMaxDepth();
    }
}
