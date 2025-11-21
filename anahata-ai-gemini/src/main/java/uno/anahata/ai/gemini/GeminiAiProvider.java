package uno.anahata.ai.gemini;

import com.google.genai.Client;
import com.google.genai.types.ListModelsConfig;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;
import lombok.Getter;
import lombok.NonNull;
import uno.anahata.ai.Chat;
import uno.anahata.ai.model.provider.AbstractAiProvider;
import uno.anahata.ai.model.provider.AbstractModel;

@Getter
public class GeminiAiProvider extends AbstractAiProvider {

    private Client client;

    public GeminiAiProvider(@NonNull Chat chat) {
        super(chat, "gemini");
        // Fail fast if no API key is configured.
        if (getApiKey() == null) {
            throw new IllegalStateException("GEMINI_API_KEY is not set or the api_keys.txt file is missing or empty.");
        }
    }

    public synchronized Client getClient() {
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
}
