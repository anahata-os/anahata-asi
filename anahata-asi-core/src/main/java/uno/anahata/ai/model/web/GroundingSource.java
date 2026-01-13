/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Fora Bara!
 */
package uno.anahata.ai.model.web;

import lombok.Value;
import lombok.experimental.SuperBuilder;

/**
 * Represents a single web source used for grounding, including its URI and title.
 *
 * @author anahata
 */
@Value
@SuperBuilder
public class GroundingSource {
    /** The URI of the source. */
    String uri;
    /** The title of the source. */
    String title;
}
