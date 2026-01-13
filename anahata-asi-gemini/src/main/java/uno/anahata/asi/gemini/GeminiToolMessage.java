/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package uno.anahata.asi.gemini;

import com.google.genai.types.Content;
import com.google.genai.types.Part;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;
import uno.anahata.asi.gemini.adapter.GeminiPartAdapter;
import uno.anahata.asi.model.core.AbstractModelMessage;
import uno.anahata.asi.model.core.AbstractToolMessage;

/**
 *
 * @author anahata
 */
public class GeminiToolMessage extends AbstractToolMessage {

    public GeminiToolMessage(AbstractModelMessage modelMessage) {
        super(modelMessage);
    }

}
