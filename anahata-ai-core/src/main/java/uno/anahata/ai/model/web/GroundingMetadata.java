/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.ai.model.web;

import java.util.List;
import lombok.AllArgsConstructor;
import lombok.Value;
import lombok.experimental.SuperBuilder;

/**
 * Metadata about the grounding sources used by the model.
 *
 * @author anahata
 */
@Value
@AllArgsConstructor
public class GroundingMetadata {

    /**
     * A list of search queries that the model suggests for further exploration.
     */
    List<String> webSearchQueries;

    /**
     * A list of text segments from the sources that directly support the model's response.
     */
    List<String> supportingTexts;

    /**
     * A list of web sources (citations) used for grounding.
     */
    List<GroundingSource> sources;
    
    /**
     * The rendered HTML for the Google Search entry point (the "Google Search" button/chip).
     */
    String searchEntryPointHtml;

    /**
     * The raw JSON representation of the native grounding metadata object.
     */
    String rawJson;
}
