/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.resource.v2;

import io.swagger.v3.oas.annotations.media.Schema;
import java.util.Objects;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import uno.anahata.asi.model.core.BasicPropertyChangeSource;

/**
 * Encapsulates adjustable viewport configuration for V2 text resources.
 * <p>
 * Controls pagination, tailing, grep filtering, and visual markers for 
 * large resource files.
 * </p>
 * <p>
 * <b>Reactivity:</b> Extends {@link BasicPropertyChangeSource} to provide 
 * fine-grained change notifications to the UI.
 * </p>
 * 
 * @author anahata
 */
@Getter
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Schema(description = "Adjustable settings for the text viewport")
public class TextViewportSettings extends BasicPropertyChangeSource {

    /** The starting character offset for pagination. */
    @Builder.Default
    private long startChar = 0;

    /** The maximum number of characters to load in a single page. */
    @Builder.Default
    private int pageSizeInChars = 64 * 1024;

    /** The maximum line width before horizontal truncation. */
    @Builder.Default
    private int columnWidth = 1024;

    /** A regular expression pattern for filtering lines (grep). */
    private String grepPattern;

    /** Whether to include line numbers in the processed output. */
    @Builder.Default
    private boolean includeLineNumbers = true;

    /** Whether to enable tailing mode. */
    @Builder.Default
    private boolean tail = false;

    /** The number of lines to capture from the end of the source. */
    @Builder.Default
    private int tailLines = 100;

    /** 
     * Sets the start character and fires a change event if the value is different.
     * @param startChar The new character offset.
     */
    public void setStartChar(long startChar) {
        if (this.startChar != startChar) {
            long old = this.startChar;
            this.startChar = startChar;
            propertyChangeSupport.firePropertyChange("startChar", old, startChar);
        }
    }

    /** 
     * Sets the page size and fires a change event if the value is different.
     * @param pageSizeInChars The maximum characters per page.
     */
    public void setPageSizeInChars(int pageSizeInChars) {
        if (this.pageSizeInChars != pageSizeInChars) {
            int old = this.pageSizeInChars;
            this.pageSizeInChars = pageSizeInChars;
            propertyChangeSupport.firePropertyChange("pageSizeInChars", old, pageSizeInChars);
        }
    }

    /** 
     * Sets the column width and fires a change event if the value is different.
     * @param columnWidth The maximum characters per line.
     */
    public void setColumnWidth(int columnWidth) {
        if (this.columnWidth != columnWidth) {
            int old = this.columnWidth;
            this.columnWidth = columnWidth;
            propertyChangeSupport.firePropertyChange("columnWidth", old, columnWidth);
        }
    }

    /** 
     * Sets the grep pattern and fires a change event if the value is different.
     * @param grepPattern The regex pattern.
     */
    public void setGrepPattern(String grepPattern) {
        if (!Objects.equals(this.grepPattern, grepPattern)) {
            String old = this.grepPattern;
            this.grepPattern = grepPattern;
            propertyChangeSupport.firePropertyChange("grepPattern", old, grepPattern);
        }
    }

    /** 
     * Toggles line numbers and fires a change event if the value is different.
     * @param includeLineNumbers True to show gutters.
     */
    public void setIncludeLineNumbers(boolean includeLineNumbers) {
        if (this.includeLineNumbers != includeLineNumbers) {
            boolean old = this.includeLineNumbers;
            this.includeLineNumbers = includeLineNumbers;
            propertyChangeSupport.firePropertyChange("includeLineNumbers", old, includeLineNumbers);
        }
    }

    /** 
     * Toggles tailing mode and fires a change event if the value is different.
     * @param tail True to enable tailing.
     */
    public void setTail(boolean tail) {
        if (this.tail != tail) {
            boolean old = this.tail;
            this.tail = tail;
            propertyChangeSupport.firePropertyChange("tail", old, tail);
        }
    }

    /** 
     * Sets the number of tail lines and fires a change event if the value is different.
     * @param tailLines The number of lines to capture.
     */
    public void setTailLines(int tailLines) {
        if (this.tailLines != tailLines) {
            int old = this.tailLines;
            this.tailLines = tailLines;
            propertyChangeSupport.firePropertyChange("tailLines", old, tailLines);
        }
    }

    /**
     * Returns a summary of the settings for use in resource headers.
     * @return A summary string.
     */
    @Override
    public String toString() {
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
