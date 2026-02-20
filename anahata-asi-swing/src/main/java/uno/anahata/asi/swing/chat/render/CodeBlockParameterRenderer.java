/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.chat.render;

import javax.swing.JComponent;
import uno.anahata.asi.model.tool.AbstractToolCall;
import uno.anahata.asi.swing.chat.ChatPanel;

/**
 * A parameter renderer that displays string values as code blocks with syntax highlighting.
 * It wraps an {@link AbstractCodeBlockSegmentRenderer} and supports editing by syncing
 * changes back to the {@link AbstractToolCall}.
 * 
 * @author anahata
 */
public class CodeBlockParameterRenderer implements ParameterRenderer<String> {
    
    private AbstractCodeBlockSegmentRenderer renderer;
    private String language;

    /** No-arg constructor for factory instantiation. */
    public CodeBlockParameterRenderer() {}

    @Override
    public void init(ChatPanel chatPanel, AbstractToolCall<?, ?> call, String paramName, String value) {
        init(chatPanel, call, paramName, value, "code");
    }

    /**
     * Specialized initialization for code blocks with a specific language.
     * 
     * @param chatPanel The chat panel.
     * @param call The tool call.
     * @param paramName The parameter name.
     * @param value The initial value.
     * @param language The language for syntax highlighting.
     */
    public void init(ChatPanel chatPanel, AbstractToolCall<?, ?> call, String paramName, String value, String language) {
        this.language = language;
        this.renderer = AbstractCodeBlockSegmentRenderer.create(chatPanel, value, language);
        this.renderer.setEditable(true);
        this.renderer.setOnSave(newContent -> call.setModifiedArgument(paramName, newContent));
    }

    @Override
    public JComponent getComponent() {
        return renderer.getComponent();
    }

    @Override
    public void updateContent(String value) {
        renderer.updateContent(value);
    }

    @Override
    public boolean render() {
        return renderer.render();
    }
}
