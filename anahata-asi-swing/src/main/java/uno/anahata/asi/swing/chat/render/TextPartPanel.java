/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.chat.render;

import java.awt.Color;
import java.awt.Component;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import javax.swing.Box;
import javax.swing.JComponent;
import javax.swing.border.Border;
import lombok.Getter;
import lombok.NonNull;
import uno.anahata.asi.model.core.TextPart;
import uno.anahata.asi.swing.chat.ChatPanel;
import javax.swing.BorderFactory; 

/**
 * Renders a {@link uno.anahata.asi.model.core.TextPart} into a list of JComponents,
 * handling markdown and code block rendering.
 *
 * @author anahata
 */
@Getter
public class TextPartPanel extends AbstractPartPanel<TextPart> {

    /** 
     * Regex pattern for identifying code blocks in markdown. 
     * Updated to support unclosed blocks for better streaming experience.
     */
    private static final Pattern CODE_BLOCK_PATTERN = Pattern.compile("```(\\w*)\\r?\\n([\\s\\S]*?)(?:\\r?\\n```|\\z)");

    /** Cache of segment renderers to support incremental updates. */
    private final List<AbstractTextSegmentRenderer> cachedSegmentRenderers = new ArrayList<>();
    /** The markdown text that was last rendered. */
    private String lastRenderedMarkdownText; 

    /**
     * Constructs a new TextPartPanel.
     *
     * @param chatPanel The chat panel instance.
     * @param part The TextPart to be rendered.
     */
    public TextPartPanel(@NonNull ChatPanel chatPanel, @NonNull TextPart part) {
        super(chatPanel, part);
    }

    /**
     * {@inheritDoc}
     * This method performs a diffing operation to reuse and update existing components
     * incrementally instead of clearing the entire panel.
     */
    @Override
    protected void renderContent() {
        String markdownText = part.getText();

        // Early exit if markdown text hasn't changed since last render
        if (Objects.equals(markdownText, lastRenderedMarkdownText)) {
            return;
        }
        lastRenderedMarkdownText = markdownText;

        if (markdownText == null || markdownText.trim().isEmpty()) {
            cachedSegmentRenderers.clear(); 
            getCentralContainer().removeAll();
            getCentralContainer().revalidate();
            getCentralContainer().repaint();
            return;
        }

        boolean isThought = part.isThought();
        List<TextSegmentDescriptor> newSegmentDescriptors = parseSegmentDescriptors(markdownText);

        // Perform diffing and update cachedSegmentRenderers
        updateCachedSegmentRenderers(newSegmentDescriptors, isThought);

        // Ensure components are in the correct order and add/remove incrementally
        for (int i = 0; i < cachedSegmentRenderers.size(); i++) {
            AbstractTextSegmentRenderer segmentRenderer = cachedSegmentRenderers.get(i);
            JComponent component = segmentRenderer.getComponent(); 
            if (component != null) {
                component.setAlignmentX(Component.LEFT_ALIGNMENT);
                if (i >= getCentralContainer().getComponentCount() || getCentralContainer().getComponent(i) != component) {
                    getCentralContainer().add(component, i);
                }
            }
        }
        
        // Remove trailing components (leftovers from previous renders with more segments)
        while (getCentralContainer().getComponentCount() > cachedSegmentRenderers.size()) {
            getCentralContainer().remove(getCentralContainer().getComponentCount() - 1);
        }
        
        // Add vertical glue to push content to the top
        getCentralContainer().add(Box.createVerticalGlue());

        getCentralContainer().revalidate();
        getCentralContainer().repaint();
    }

    /**
     * Parses the given markdown text into a list of {@link TextSegmentDescriptor}s.
     * This method identifies code blocks and regular text segments.
     *
     * @param markdownText The markdown text to parse.
     * @return A list of TextSegmentDescriptors.
     */
    private static List<TextSegmentDescriptor> parseSegmentDescriptors(String markdownText) {
        List<TextSegmentDescriptor> descriptors = new ArrayList<>();
        Matcher matcher = CODE_BLOCK_PATTERN.matcher(markdownText);
        int lastEnd = 0;

        while (matcher.find()) {
            // Preceding text segment
            if (matcher.start() > lastEnd) {
                String textSegmentContent = markdownText.substring(lastEnd, matcher.start());
                descriptors.add(new TextSegmentDescriptor(TextSegmentType.TEXT, textSegmentContent, null));
            }

            // Code block segment
            String language = matcher.group(1);
            String code = matcher.group(2);
            descriptors.add(new TextSegmentDescriptor(TextSegmentType.CODE, code, language));

            lastEnd = matcher.end();
        }

        // Remaining text segment
        if (lastEnd < markdownText.length()) {
            String textSegmentContent = markdownText.substring(lastEnd);
            descriptors.add(new TextSegmentDescriptor(TextSegmentType.TEXT, textSegmentContent, null));
        }
        return descriptors;
    }

    /**
     * Performs a diffing operation to update the {@code cachedSegmentRenderers} list.
     * It reuses existing renderers, creates new ones, and removes old ones as needed.
     *
     * @param newSegmentDescriptors The list of segment descriptors parsed from the current markdown text.
     * @param isThought True if the text represents a model thought, false otherwise.
     */
    private void updateCachedSegmentRenderers(List<TextSegmentDescriptor> newSegmentDescriptors, boolean isThought) {
        boolean needsFullRebuild = false;

        if (newSegmentDescriptors.size() != cachedSegmentRenderers.size()) {
            needsFullRebuild = true;
        } else {
            for (int i = 0; i < newSegmentDescriptors.size(); i++) {
                TextSegmentDescriptor newDescriptor = newSegmentDescriptors.get(i);
                AbstractTextSegmentRenderer cachedRenderer = cachedSegmentRenderers.get(i);

                // Check if the cached renderer matches the new descriptor
                if (!cachedRenderer.matches(newDescriptor)) {
                    needsFullRebuild = true;
                    break;
                }
            }
        }

        if (needsFullRebuild) {
            cachedSegmentRenderers.clear();
            for (TextSegmentDescriptor descriptor : newSegmentDescriptors) {
                AbstractTextSegmentRenderer renderer = descriptor.createRenderer(chatPanel, isThought);
                renderer.render(); // CRITICAL: Initialize the component
                cachedSegmentRenderers.add(renderer);
            }
        } else {
            // Sizes and types/languages match, update content of existing renderers
            for (int i = 0; i < newSegmentDescriptors.size(); i++) {
                TextSegmentDescriptor newDescriptor = newSegmentDescriptors.get(i);
                AbstractTextSegmentRenderer cachedRenderer = cachedSegmentRenderers.get(i);
                cachedRenderer.updateContent(newDescriptor.content());
                cachedRenderer.render(); // Call render to update the component if content changed
            }
        }
    }
}
