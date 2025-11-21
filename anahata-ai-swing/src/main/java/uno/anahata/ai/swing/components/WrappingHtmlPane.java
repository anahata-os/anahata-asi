/*
 * Copyright 2025 Anahata.
 *
 * Licensed under the Anahata Software License (ASL) V2.0;
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      https://github.com/pablo-anahata/anahata-ai-parent/blob/main/LICENSE
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * Força Barça!
 */
package uno.anahata.ai.swing.components;

import java.awt.BorderLayout;
import java.awt.Dimension;
import javax.swing.JEditorPane;
import javax.swing.JPanel;
import javax.swing.text.html.HTMLEditorKit;
import javax.swing.text.html.StyleSheet;
import lombok.Getter;

/**
 * A reusable JPanel component that wraps a {@link WrappingEditorPane} in a
 * BorderLayout to correctly force HTML content to wrap based on the container's
 * width. This is a standard Swing trick to ensure correct layout for long,
 * unbroken strings.
 *
 * @author anahata-gemini-pro-2.5
 */
@Getter
public class WrappingHtmlPane extends JPanel {

    private final WrappingEditorPane editorPane;

    public WrappingHtmlPane() {
        super(new BorderLayout());
        this.editorPane = new WrappingEditorPane();
        
        // Configuration copied from TextPartRenderer.createHtmlPane
        editorPane.setEditable(false);
        editorPane.setContentType("text/html");
        editorPane.setOpaque(false); 
        
        HTMLEditorKit kit = new HTMLEditorKit();
        editorPane.setEditorKit(kit);
        StyleSheet sheet = kit.getStyleSheet();
        
        // Styles copied from TextPartRenderer.createHtmlPane (excluding font-size)
        sheet.addRule("body { word-wrap: break-word; font-family: sans-serif; background-color: transparent; }");
        sheet.addRule("table { border-collapse: collapse; width: 100%; }");
        sheet.addRule("th, td { border: 1px solid #dddddd; text-align: left; padding: 8px; }");
        sheet.addRule("th { background-color: #f2f2f2; }");
        
        // Add the editor pane to the center of this panel
        setOpaque(false);
        add(editorPane, BorderLayout.CENTER);
    }

    /**
     * Sets the HTML content to be displayed.
     * The input HTML is expected to be the inner content of the <body> tag.
     * @param html The HTML string.
     */
    public void setHtml(String html) {
        // Removed explicit width constraint from the body tag.
        // The JPanel wrapper with BorderLayout is responsible for constraining the width.
        editorPane.setText("<html><body>" + html + "</body></html>");
    }
}
