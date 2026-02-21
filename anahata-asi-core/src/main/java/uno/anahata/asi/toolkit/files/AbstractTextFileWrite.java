/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package uno.anahata.asi.toolkit.files;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 *
 * @author anahata
 */
@NoArgsConstructor
@AllArgsConstructor
@Getter
public abstract class AbstractTextFileWrite {
    
    /**
     * The absolute path to the file to be updated.
     */
    @Schema(description = "The absolute path to the file.", required = true)
    protected String path;
    /**
     * Optimistic locking: the expected last modified timestamp of the file on disk.
     */
    @Schema(description = "Optimistic locking: the expected last modified timestamp of the file on disk.", required = true)
    protected long lastModified;
    
}
