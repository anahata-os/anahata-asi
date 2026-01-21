package uno.anahata.asi.model.resource;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

/**
 * Encapsulates adjustable viewport configuration.
 * @author anahata-ai
 */
@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
@Schema(description = "Adjustable settings for the text viewport")
public class TextViewportSettings {
    @Builder.Default
    private long startChar = 0;
    @Builder.Default
    private int pageSizeInChars = 64 * 1024;
    @Builder.Default
    private int columnWidth = 1024;
    private String grepPattern;
    @Builder.Default
    private boolean showLineNumbers = false;
}
