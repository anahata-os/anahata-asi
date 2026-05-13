/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.anthropic;

import java.util.Collections;
import java.util.List;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.provider.AbstractModel;
import uno.anahata.asi.agi.provider.GenerationRequest;
import uno.anahata.asi.agi.provider.RequestConfig;
import uno.anahata.asi.agi.provider.Response;
import uno.anahata.asi.agi.provider.ServerTool;
import uno.anahata.asi.agi.provider.StreamObserver;
import uno.anahata.asi.agi.tool.spi.AbstractTool;

/**
 * Model implementation for Anthropic's Claude.
 * 
 * @author anahata
 */
@Slf4j
public class AnthropicModel extends AbstractModel {

    private final AnthropicProvider provider;
    private final String modelId;
    private final String displayName;

    public AnthropicModel(AnthropicProvider provider, String modelId, String displayName) {
        this.provider = provider;
        this.modelId = modelId;
        this.displayName = displayName;
    }

    @Override
    public AnthropicProvider getProvider() { return provider; }

    @Override
    public String getModelId() { return modelId; }

    @Override
    public String getDisplayName() { return displayName; }

    @Override
    public String getDescription() { return "Anthropic Claude Model"; }

    @Override
    public String getVersion() { return null; }

    @Override
    public int getMaxInputTokens() { return 200000; }

    @Override
    public int getMaxOutputTokens() { return 8192; }

    @Override
    public List<String> getSupportedActions() { return List.of("messages"); }

    @Override
    public String getRawDescription() { return "<html><b>Model ID:</b> " + modelId + "</html>"; }

    @Override
    public boolean isSupportsFunctionCalling() { return true; }

    @Override
    public boolean isSupportsContentGeneration() { return true; }

    @Override
    public boolean isSupportsBatchEmbeddings() { return false; }

    @Override
    public boolean isSupportsEmbeddings() { return false; }

    @Override
    public boolean isSupportsCachedContent() { return true; }

    @Override
    public List<String> getSupportedResponseModalities() { return List.of("TEXT", "IMAGE"); }

    @Override
    public List<ServerTool> getAvailableServerTools() { return Collections.emptyList(); }

    @Override
    public List<ServerTool> getDefaultServerTools() { return Collections.emptyList(); }

    @Override
    public Float getDefaultTemperature() { return 0.7f; }

    @Override
    public Integer getDefaultTopK() { return null; }

    @Override
    public Float getDefaultTopP() { return null; }

    @Override
    public Response generateContent(GenerationRequest request) {
        throw new UnsupportedOperationException("Anthropic generateContent not yet implemented.");
    }

    @Override
    public void generateContentStream(GenerationRequest request, StreamObserver observer) {
        throw new UnsupportedOperationException("Anthropic generateContentStream not yet implemented.");
    }

    @Override
    public String getToolDeclarationJson(AbstractTool tool, RequestConfig config) {
        return "{}"; // TODO: Implement Anthropic Tool Schema
    }
}
