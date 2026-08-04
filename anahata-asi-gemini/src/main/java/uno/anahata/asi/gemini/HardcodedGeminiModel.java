/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.gemini;

import com.google.genai.types.Model;
import java.util.ArrayList;
import java.util.List;
import lombok.Getter;
import lombok.Setter;

/**
 * A specialized {@link GeminiModel} that uses hardcoded metadata instead of
 * fetching it from the Gemini API. This is useful for restricted environments
 * like "Google Vertex Express" which do not support model discovery (listing).
 *
 * @author anahata
 */
@Getter
@Setter
public class HardcodedGeminiModel extends GeminiModel {

    /**
     * The human-readable name for this model. Used when the API cannot 
     * provide metadata (e.g. Vertex Express).
     */
    private String displayName;
    /**
     * The version string for the hardcoded model manifest.
     */
    private String version;
    /**
     * The maximum number of input tokens supported by this specific 
     * hardcoded manifest.
     */
    private Integer maxInputTokens;
    /**
     * The maximum number of output tokens supported by this specific 
     * hardcoded manifest.
     */
    private Integer maxOutputTokens;
    /**
     * The list of API actions (e.g. 'generateContent') supported by 
     * this specific hardcoded manifest.
     */
    private List<String> supportedActions = new ArrayList<>();

    /**
     * Constructs a new hardcoded model instance.
     * @param provider The owning Gemini provider.
     * @param modelId  The unique model identifier.
     */
    public HardcodedGeminiModel(GeminiAiProvider provider, String modelId) {
        super(provider, Model.builder().name(modelId).build());
    }

    /**
     * {@inheritDoc}
     * <p>
     * Returns the hardcoded display name.
     * </p>
     */
    @Override
    public String getDisplayName() {
        return displayName != null ? displayName : getModelId();
    }

    /**
     * {@inheritDoc}
     * <p>
     * Returns the hardcoded version string.
     * </p>
     */
    @Override
    public String getVersion() {
        return version != null ? version : "";
    }

    /**
     * {@inheritDoc}
     * <p>
     * Returns the hardcoded input token limit.
     * </p>
     */
    @Override
    public Integer getMaxInputTokens() {
        return maxInputTokens;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Returns the hardcoded output token limit.
     * </p>
     */
    @Override
    public Integer getMaxOutputTokens() {
        return maxOutputTokens;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Returns the hardcoded list of supported actions.
     * </p>
     */
    @Override
    public List<String> getSupportedActions() {
        return supportedActions;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Returns null as the default temperature.
     * </p>
     */
    @Override
    public Float getDefaultTemperature() {
        return null;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Returns null as the default topK.
     * </p>
     */
    @Override
    public Integer getDefaultTopK() {
        return null;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Returns null as the default topP.
     * </p>
     */
    @Override
    public Float getDefaultTopP() {
        return null;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Returns a simple description for hardcoded models.
     * </p>
     */
    @Override
    public String getDescription() {
        return "Hardcoded model manifest for restricted API environments.";
    }

    /**
     * {@inheritDoc}
     * <p>
     * Returns a basic metadata summary for the model.
     * </p>
     */
    @Override
    public String getRawDescription() {
        return "<html><b>ID: </b>" + getModelId() + "<br>"
                + "<b>Display Name: </b>" + getDisplayName() + "<br>"
                + "<b>Version: </b>" + getVersion() + "<br>"
                + "<b>Input Tokens: </b>" + getMaxInputTokens() + "<br>"
                + "<b>Output Tokens: </b>" + getMaxOutputTokens() + "<br>"
                + "<b>Supported Actions: </b>" + getSupportedActions() + "</html>";
    }
}
