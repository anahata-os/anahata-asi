/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.agi.resource.view;

import com.fasterxml.jackson.annotation.JsonIgnore;
import io.swagger.v3.oas.annotations.media.Schema;
import java.util.Objects;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

/**
 * Encapsulates adjustable viewport configuration for V2 text resources.
 * <p>
 * Controls pagination, tailing, grep filtering, and visual markers for 
 * large resource files.
 * </p>
 * 
 * @author anahata
 */
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Schema(description = "Adjustable settings for the text viewport")
public class TextViewportSettings {

    /**
     * Back-reference to the parent viewport engine.
     */
    @JsonIgnore
    @Schema(hidden = true)
    private TextViewport viewport;

    /**
     * Constructs settings bound to a parent viewport engine.
     * @param viewport The parent viewport.
     */
    public TextViewportSettings(TextViewport viewport) {
        this();
        this.viewport = viewport;
    }

    /**
     * Synchronously notifies the parent viewport and view that settings have changed.
     */
    public void markDirty() {
        if (viewport != null) {
            viewport.markDirty();
        }
    }

    /** Whether full view is enabled (loading entire resource up to EOF). */
    private boolean fullView = false;

    /** The starting character offset for pagination. */
    private int startChar = 0;

    /** The maximum number of characters to load in a single page. */
    private int pageSizeInChars = 64 * 1024;

    /** The maximum line width before horizontal truncation. */
    private int columnWidth = 1024;

    /** A regular expression pattern for filtering lines (grep). */
    private String grepPattern;

    /** Whether to include line numbers in the processed output. */
    private boolean includeLineNumbers = true;

    /** Whether to enable tailing mode. */
    private boolean tail = false;

    /** The number of lines to capture from the end of the source. */
    private int tailLines = 100;

    /** 
     * Sets whether full view mode is enabled and triggers dirty chain.
     * @param fullView True to load full resource.
     */
    public void setFullView(boolean fullView) {
        if (this.fullView != fullView) {
            this.fullView = fullView;
            markDirty();
        }
    }

    /** 
     * Sets the start character and triggers dirty chain.
     * @param startChar The new character offset.
     */
    public void setStartChar(int startChar) {
        if (this.startChar != startChar) {
            this.startChar = startChar;
            markDirty();
        }
    }

    /** 
     * Sets the page size and triggers dirty chain.
     * @param pageSizeInChars The maximum characters per page.
     */
    public void setPageSizeInChars(int pageSizeInChars) {
        if (this.pageSizeInChars != pageSizeInChars) {
            this.pageSizeInChars = pageSizeInChars;
            markDirty();
        }
    }

    /** 
     * Sets the column width and triggers dirty chain.
     * @param columnWidth The maximum characters per line.
     */
    public void setColumnWidth(int columnWidth) {
        if (this.columnWidth != columnWidth) {
            this.columnWidth = columnWidth;
            markDirty();
        }
    }

    /** 
     * Sets the grep pattern and triggers dirty chain.
     * @param grepPattern The regex pattern.
     */
    public void setGrepPattern(String grepPattern) {
        if (!Objects.equals(this.grepPattern, grepPattern)) {
            this.grepPattern = grepPattern;
            markDirty();
        }
    }

    /** 
     * Toggles line numbers and triggers dirty chain.
     * @param includeLineNumbers True to show gutters.
     */
    public void setIncludeLineNumbers(boolean includeLineNumbers) {
        if (this.includeLineNumbers != includeLineNumbers) {
            this.includeLineNumbers = includeLineNumbers;
            markDirty();
        }
    }

    /** 
     * Toggles tailing mode and triggers dirty chain.
     * @param tail True to enable tailing.
     */
    public void setTail(boolean tail) {
        if (this.tail != tail) {
            this.tail = tail;
            markDirty();
        }
    }

    /** 
     * Sets the number of tail lines and triggers dirty chain.
     * @param tailLines The number of lines to capture.
     */
    public void setTailLines(int tailLines) {
        if (this.tailLines != tailLines) {
            this.tailLines = tailLines;
            markDirty();
        }
    }

    /**
     * Returns a summary of the settings for use in resource headers.
     * @return A summary string.
     */
    @Override
    public String toString() {
        if (fullView) {
            return "Full View " + (includeLineNumbers ? "(+Lines)" : "(-Lines)");
        }
        StringBuilder sb = new StringBuilder();
        if (grepPattern != null && !grepPattern.isBlank()) {
            sb.append("Grep: '").append(grepPattern).append("' ");
        }
        if (tail) {
            sb.append("Tail: ").append(tailLines).append(" lines ");
        } else {
            sb.append(String.format("Range: %d-%d ", startChar, startChar + pageSizeInChars));
        }
        
        sb.append(includeLineNumbers ? "(+Lines) " : "(-Lines) ");
        
        if (columnWidth != 1024) {
            sb.append("Cols: ").append(columnWidth).append(" ");
        }
        return sb.length() > 0 ? sb.toString().trim() : "Full View";
    }
}
