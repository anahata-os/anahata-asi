package uno.anahata.ai.gemini;

import com.google.genai.Client;
import com.google.genai.types.Content;
import com.google.genai.types.GenerateContentConfig;
import com.google.genai.types.GenerateContentResponse;
import com.google.genai.types.ListModelsConfig;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.ai.AiConfig;
import uno.anahata.ai.gemini.adapter.ContentAdapter;
import uno.anahata.ai.gemini.adapter.RequestConfigAdapter;
import uno.anahata.ai.gemini.adapter.ResponseAdapter;
import uno.anahata.ai.model.core.Request;
import uno.anahata.ai.model.provider.AbstractAiProvider;
import uno.anahata.ai.model.provider.AbstractModel;
import uno.anahata.ai.tool.ToolManager;

@Slf4j
public class GeminiAiProvider extends AbstractAiProvider {

    private Client client;
    private final ToolManager toolManager;

    public GeminiAiProvider(AiConfig config, ToolManager toolManager) {
        super(config, "gemini");
        this.toolManager = toolManager;
    }

    private synchronized Client getClient() {
        if (client == null) {
            client = Client.builder()
                    .apiKey(getApiKey())
                    .build();
        }
        return client;
    }

    @Override
    public List<? extends AbstractModel> listModels() {

        var pager = getClient().models.list(ListModelsConfig.builder().build());
        return StreamSupport.stream(pager.spliterator(), false)
                .map(model -> new GeminiModel(this, model))
                .collect(Collectors.toList());

    }

    @Override
    public uno.anahata.ai.model.core.Response generateContent(Request request) {
        try {
            List<Content> history = request.getHistory().stream()
                    .map(ContentAdapter::toGoogle)
                    .collect(Collectors.toList());

            GenerateContentConfig config = RequestConfigAdapter.toGoogle(request.getConfig());

            GenerateContentResponse response = getClient().models.generateContent(
                    request.getModel().getModelId(),
                    history,
                    config
            );

            return ResponseAdapter.toAnahata(response, toolManager, request.getModel().getModelId());

        } catch (Exception e) {
            log.error("Failed to generate content with Gemini", e);
            throw new RuntimeException(e);
        }
    }
}