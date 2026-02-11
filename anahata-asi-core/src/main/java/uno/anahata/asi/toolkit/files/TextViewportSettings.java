package uno.anahata.asi.toolkit.files;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;

/**
 * Encapsulates adjustable viewport configuration.
 * @author anahata-ai
 */
@Getter
@Setter
@ToString
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
    @Builder.Default
    private boolean tail = false;
    @Builder.Default
    private int tailLines = 100;
}
