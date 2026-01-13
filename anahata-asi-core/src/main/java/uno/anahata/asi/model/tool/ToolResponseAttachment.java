/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package uno.anahata.asi.model.tool;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

/**
 *
 * @author anahata
 */
@RequiredArgsConstructor
@Getter
public final class ToolResponseAttachment {
    private final byte[] data;
    private final String mimeType;
}