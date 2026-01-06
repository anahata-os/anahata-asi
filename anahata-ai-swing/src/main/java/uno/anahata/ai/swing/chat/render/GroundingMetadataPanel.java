/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Fora Bara!
 */
package uno.anahata.ai.swing.chat.render;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Cursor;
import java.awt.Desktop;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.Rectangle;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.border.EmptyBorder;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.ai.model.web.GroundingMetadata;
import uno.anahata.ai.model.web.GroundingSource;
import uno.anahata.ai.swing.chat.ChatPanel;
import uno.anahata.ai.swing.chat.SwingChatConfig.UITheme;
import uno.anahata.ai.swing.components.CodeHyperlink;
import uno.anahata.ai.swing.components.WrappingEditorPane;
import uno.anahata.ai.swing.icons.CopyIcon;
import uno.anahata.ai.swing.icons.IconUtils;
import uno.anahata.ai.swing.icons.SearchIcon;
import uno.anahata.ai.swing.internal.SwingUtils;
import uno.anahata.ai.swing.internal.WrapLayout;

/**
 * A specialized Swing panel for rendering {@link GroundingMetadata} in a rich,
 * interactive format. It displays supporting text segments, web sources with
 * clickable links, and search suggestions as chips.
 * 
 * @author gemini-3-flash-preview
 */
@Slf4j
@Getter
public class GroundingMetadataPanel extends JPanel {
    private static final int V_GAP = 10;
    private final ChatPanel chatPanel;
    private final UITheme theme;
    private final GroundingMetadata metadata;

    public GroundingMetadataPanel(ChatPanel chatPanel, GroundingMetadata metadata) {
        this.chatPanel = chatPanel;
        this.metadata = metadata;
        this.theme = chatPanel.getChatConfig().getTheme();
        setLayout(new BorderLayout());
        setOpaque(false);
        
        // Outer Border
        setBorder(BorderFactory.createCompoundBorder(
            BorderFactory.createLineBorder(theme.getModelBorder(), 1),
            new EmptyBorder(0, 0, 0, 0)
        )); 

        // 1. Main Header
        add(renderHeader(), BorderLayout.NORTH);

        // 2. Content Panel
        JPanel contentPanel = new JPanel();
        contentPanel.setLayout(new BoxLayout(contentPanel, BoxLayout.Y_AXIS));
        contentPanel.setBackground(theme.getGroundingContentBg());
        contentPanel.setBorder(new EmptyBorder(V_GAP, V_GAP, 0, V_GAP));

        // Supporting Text
        if (metadata.getSupportingTexts() != null && !metadata.getSupportingTexts().isEmpty()) {
            contentPanel.add(renderSupportingTexts(metadata.getSupportingTexts()));
            contentPanel.add(Box.createVerticalStrut(V_GAP));
        }

        // Sources
        if (metadata.getSources() != null && !metadata.getSources().isEmpty()) {
            contentPanel.add(renderSources(metadata.getSources()));
            contentPanel.add(Box.createVerticalStrut(V_GAP));
        }

        // Search Suggestions (Fallback/Additional)
        if (metadata.getWebSearchQueries() != null && !metadata.getWebSearchQueries().isEmpty()) {
            contentPanel.add(renderSearchSuggestions(metadata.getWebSearchQueries()));
        }
        
        contentPanel.add(Box.createVerticalGlue());
        add(contentPanel, BorderLayout.CENTER);
    }
    
    private JPanel renderHeader() {
        JPanel headerPanel = new JPanel(new BorderLayout());
        headerPanel.setBackground(theme.getGroundingHeaderBg());
        headerPanel.setBorder(BorderFactory.createEmptyBorder(6, 10, 6, 10));
        
        JLabel titleLabel = new JLabel("Grounding Metadata");
        titleLabel.setFont(titleLabel.getFont().deriveFont(Font.BOLD));
        titleLabel.setForeground(theme.getFontColor());
        
        try {
            titleLabel.setIcon(IconUtils.getIcon("anahata.png", 24, 24));
            titleLabel.setIconTextGap(8);
        } catch (Exception e) {
            log.warn("Could not load icon for header.", e);
        }
        
        headerPanel.add(titleLabel, BorderLayout.WEST);
        
        // Add a JSON link for debugging
        CodeHyperlink jsonLink = new CodeHyperlink("Json", 
                () -> "Grounding Metadata", 
                () -> metadata.getRawJson(), 
                "json");
        headerPanel.add(jsonLink, BorderLayout.EAST);
        
        return headerPanel;
    }

    private JPanel createDetailSection(String title) {
        JPanel mainPanel = new JPanel(new BorderLayout());
        mainPanel.setAlignmentX(Component.LEFT_ALIGNMENT);
        
        JPanel headerPanel = new JPanel(new BorderLayout());
        headerPanel.setBackground(theme.getGroundingDetailsHeaderBg());
        headerPanel.setBorder(BorderFactory.createEmptyBorder(4, 8, 4, 8));
        
        JLabel titleLabel = new JLabel(title);
        titleLabel.setFont(titleLabel.getFont().deriveFont(Font.BOLD, 12f));
        titleLabel.setForeground(theme.getGroundingDetailsHeaderColor());
        headerPanel.add(titleLabel, BorderLayout.WEST);
        
        mainPanel.add(headerPanel, BorderLayout.NORTH);
        
        JPanel contentPanel = new JPanel(new BorderLayout());
        contentPanel.setBackground(theme.getGroundingDetailsContentBg());
        contentPanel.setBorder(new EmptyBorder(8, 10, 8, 10));
        
        JPanel innerContentHolder = new JPanel();
        innerContentHolder.setLayout(new BoxLayout(innerContentHolder, BoxLayout.Y_AXIS));
        innerContentHolder.setOpaque(false);
        
        contentPanel.add(innerContentHolder, BorderLayout.NORTH);
        mainPanel.add(contentPanel, BorderLayout.CENTER);
        mainPanel.setBorder(BorderFactory.createLineBorder(theme.getModelBorder(), 1));
        
        return mainPanel;
    }

    private JPanel renderSupportingTexts(List<String> texts) {
        JPanel mainPanel = createDetailSection("Supporting Text");
        JPanel contentPanel = (JPanel) mainPanel.getComponent(1);
        JPanel innerContentHolder = (JPanel) contentPanel.getComponent(0);
        
        for (String text : texts) {
            JLabel textLabel = new JLabel("<html>" + text + "</html>");
            textLabel.setAlignmentX(Component.LEFT_ALIGNMENT);
            innerContentHolder.add(textLabel);
        }
        return mainPanel;
    }

    private JPanel renderSources(List<GroundingSource> sources) {
        JPanel mainPanel = createDetailSection("Sources");
        JPanel contentPanel = (JPanel) mainPanel.getComponent(1);
        JPanel innerContentHolder = (JPanel) contentPanel.getComponent(0);

        AtomicInteger count = new AtomicInteger(1);
        for (GroundingSource source : sources) {
            String uri = source.getUri();
            String title = source.getTitle() != null ? source.getTitle() : uri;
            String labelText = String.format("<html>%d. <a href='%s'>%s</a></html>", count.getAndIncrement(), uri, title);
            JLabel label = createClickableLabel(labelText, uri);
            label.setAlignmentX(Component.LEFT_ALIGNMENT);
            innerContentHolder.add(label);
        }
        return mainPanel;
    }

    private JPanel renderSearchSuggestions(List<String> queries) {
        JPanel mainPanel = createDetailSection("Search Suggestions");
        JPanel contentPanel = (JPanel) mainPanel.getComponent(1);
        JPanel innerContentHolder = (JPanel) contentPanel.getComponent(0);

        JPanel chipsContainer = new JPanel(new WrapLayout(FlowLayout.LEFT, 8, 4));
        chipsContainer.setOpaque(false);

        for (String query : queries) {
            String searchUri = "https://www.google.com/search?q=" + query.replace(" ", "+");
            chipsContainer.add(createClickableLabel("<html><u>" + query + "</u></html>", searchUri));
        }
        
        innerContentHolder.add(chipsContainer);
        return mainPanel;
    }

    private JLabel createClickableLabel(String text, String uri) {
        JLabel label = new JLabel(text);
        label.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
        label.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                if (uri != null) {
                    try {
                        Desktop.getDesktop().browse(new URI(uri));
                    } catch (IOException | URISyntaxException ex) {
                        log.error("Failed to open URI: {}", uri, ex);
                    }
                }
            }
        });
        return label;
    }
}
