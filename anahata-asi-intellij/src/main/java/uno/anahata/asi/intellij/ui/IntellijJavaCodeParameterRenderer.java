/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.intellij.ui;

import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.resource.handle.StringHandle;
import uno.anahata.asi.agi.tool.spi.AbstractToolCall;
import uno.anahata.asi.swing.agi.message.part.tool.param.ObjectToStringParameterRenderer;

/**
 * Specialized parameter renderer for dynamic Java code execution in IntelliJ IDEA.
 * <p>
 * Binds the parameter to the "java" language mode and names the ephemeral handle
 * "Anahata.java" so the high-fidelity {@link uno.anahata.asi.intellij.ui.resources.IntellijTextResourceViewer}
 * binds it to the IntelliJ Java file type and editor highlighter.
 * </p>
 *
 * @author anahata
 */
@Slf4j
public class IntellijJavaCodeParameterRenderer extends ObjectToStringParameterRenderer {

    /**
     * Constructs the renderer, setting the language to "java" and enabling user editability.
     */
    public IntellijJavaCodeParameterRenderer() {
        super();
        setLanguage("java");
        setEditable(true);
    }

    /**
     * {@inheritDoc}
     * <p>
     * Ensures the handle name is always "Anahata.java" for Java parameter snippets.
     * </p>
     */
    @Override
    protected StringHandle createHandle(String content, AbstractToolCall<?, ?> call, String paramName) {
        return new StringHandle("Anahata.java", content);
    }
}
