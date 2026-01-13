/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.model.tool.bad;

import uno.anahata.asi.model.tool.AbstractToolParameter;
import lombok.Getter;

/**
 * A placeholder parameter for a {@link BadTool}. It contains no meaningful
 * reflection data and is used only to satisfy the generic constraints of the
 * {@link AbstractToolParameter} hierarchy.
 *
 * @author anahata-gemini-pro-2.5
 */
@Getter
public class BadToolParam extends AbstractToolParameter<BadTool> {

    public BadToolParam(BadTool tool, String name) {
        // The empty strings satisfy the @NonNull constraints in the superclass.
        super(tool, name, "", "", false, null);
    }
}
