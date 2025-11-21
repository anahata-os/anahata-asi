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
package uno.anahata.ai.swing.provider;

import java.awt.BorderLayout;
import java.awt.Dimension;
import javax.swing.JToolTip;
import uno.anahata.ai.swing.components.WrappingHtmlPane;

/**
 * A custom JToolTip that uses a WrappingHtmlPane configured for HTML.
 * This allows for rich formatting while ensuring the content wraps correctly.
 *
 * @author anahata-gemini-pro-2.5
 */
public class HtmlWrappingToolTip extends JToolTip {

    // Use the new reusable component that correctly handles HTML wrapping
    private final WrappingHtmlPane htmlPane = new WrappingHtmlPane();

    public HtmlWrappingToolTip() {
        // The WrappingHtmlPane is a JPanel with BorderLayout, which is the correct wrapper.
        htmlPane.getEditorPane().setFont(getFont()); // Inherit font from the tooltip
        
        setLayout(new BorderLayout());
        add(htmlPane, BorderLayout.CENTER);
    }

    @Override
    public void setTipText(String text) {
        // The text is expected to be HTML
        htmlPane.setHtml(text);
        
        // The WrappingHtmlPane's layout ensures the preferred size calculation is correct.
        Dimension preferredSize = htmlPane.getPreferredSize();
        setPreferredSize(preferredSize);
        
        // Call super to ensure the tooltip manager handles the text change
        super.setTipText(text);
    }
    
    @Override
    public Dimension getPreferredSize() {
        // Delegate preferred size calculation to the internal pane
        return htmlPane.getPreferredSize();
    }
}
