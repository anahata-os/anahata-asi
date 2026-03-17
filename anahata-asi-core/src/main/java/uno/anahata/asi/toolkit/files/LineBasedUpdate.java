/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.toolkit.files;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Represents a single line-based replacement operation.
 * <p>
 * This DTO is optimized for agentic workflows, using semantic naming 
 * that aligns with natural model reasoning.
 * </p>
 * 
 * @author anahata
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Schema(description = "Represents a token-efficient line-based update operation that can insert, delete or replace a single line or a range of lines without surrounding anchors/context.")
public class LineBasedUpdate {
    /**
     * The 1-based start line number.
     */
    @Schema(description = "The 1-based line number of the text resource in the RAG message where this update starts.", required = true)
    private int startLine;

    /**
     * The number of lines from `startLine` (including the `startLine` line) in the text resource that will be deleted or replaced."
     * Use 0 for pure insertion.
     */
    
/*
    @Schema(description = "The number of lines from `startLine` (including the `startLine` line) in the text resource that will be deleted or replaced."
            + " For example, if you want to replace lines 108 and 109 for 4 new lines, `startLine` should be 108, `lineCount` should be 2 and `newContent` should contain the new 4 lines. "
            + " If you want to insert 4 new lines between 108 and 109, `startLine` should be 109 (the line after the insertion point). `lineCount` should be 0 (as it is a pure insert) and `newContent` should contain the new 4 lines. "
            + " If you want to delete lines 108 and 109, `startLine` should be 108, `lineCount` should be 2 and `newContent` should be an empty string. "
            + " ", required = true)
    */
    @Schema(description = "The exact number of lines from `startLine` to be removed or replaced. "
        + "This is a surgical count: 0 = Pure Insertion, 1 = Replace single line, N = Replace range. "
        + "Use the line numbers in the RAG message as your absolute source of truth.", required = true)
    private int lineCount;

    /**
     * The replacement text. Can be multiple lines. Use empty string for pure removal.
     */
    //@Schema(description = "The new lines for that range [startLine, startLine + lineCount). Use standard line breaks between lines. Empty if you just want to delete lines. A trailing new-line character (i.e. ending this newContent with a \\n) will cause an additional extra blank line to be inserted")
    @Schema(description = "The new lines for the range. Do NOT include surrounding context/anchors from the source file as it would cause the tool to fail; "
        + "only include the content that should exist between [startLine] and [startLine + lineCount]. "
        + "Structural Newline Absorption is active: a single trailing \\n will NOT create a blank line.")
    private String newContent;

    /**
     * The reason for this change.
     */
    @Schema(description = "The reason for this change.", required = true)
    private String reason;
}
