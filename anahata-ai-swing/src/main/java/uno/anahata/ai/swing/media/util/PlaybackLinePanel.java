package uno.anahata.ai.swing.media.util;

import java.awt.FlowLayout;
import java.util.List;
import javax.sound.sampled.SourceDataLine;
import javax.swing.JPanel;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import org.jdesktop.swingx.JXComboBox;
import uno.anahata.ai.swing.internal.SwingTask;
import uno.anahata.ai.swing.internal.SwingUtils;

/**
 * A panel that encapsulates all playback line-related UI components and logic.
 * This includes a dropdown for selecting output lines.
 *
 * @author pablo
 */
@Slf4j
@Getter
public final class PlaybackLinePanel extends JPanel {

    private final JXComboBox playbackLineComboBox;
    private SourceDataLine selectedSourceDataLine;

    public PlaybackLinePanel() {
        super(new FlowLayout(FlowLayout.RIGHT, 5, 0)); // Align to the right

        playbackLineComboBox = new JXComboBox();
        playbackLineComboBox.setToolTipText("Select audio playback line");
        playbackLineComboBox.setEnabled(false); // Disable until lines are loaded
        playbackLineComboBox.setRenderer(new LineInfoRenderer());
        playbackLineComboBox.addActionListener(e -> {
            LineInfo selectedLineInfo = (LineInfo) playbackLineComboBox.getSelectedItem();
            if (selectedLineInfo != null) {
                setSelectedSourceDataLine(selectedLineInfo.getSourceDataLine());
            }
        });

        add(playbackLineComboBox);

        initPlaybackLineComboBox();
    }

    public void setSelectedSourceDataLine(SourceDataLine newSourceDataLine) {
        if (this.selectedSourceDataLine != null && this.selectedSourceDataLine.isOpen()) {
            this.selectedSourceDataLine.close();
        }
        this.selectedSourceDataLine = newSourceDataLine;
    }

    private void initPlaybackLineComboBox() {
        new SwingTask<List<LineInfo>>("Load Playback Lines",
            () -> SoundUtils.getAvailablePlaybackLines(),
            (lines) -> {
                if (lines != null && !lines.isEmpty()) {
                    for (LineInfo lineInfo : lines) {
                        playbackLineComboBox.addItem(lineInfo);
                    }
                    playbackLineComboBox.setEnabled(true);

                    // Select the first item in the list and initialize the playback line
                    playbackLineComboBox.setSelectedIndex(0);
                    setSelectedSourceDataLine(lines.get(0).getSourceDataLine());
                } else {
                    playbackLineComboBox.setEnabled(false);
                    playbackLineComboBox.setToolTipText("No playback lines found.");
                }
            },
            (error) -> {
                log.error("Failed to load playback lines", error);
                playbackLineComboBox.setEnabled(false);
                playbackLineComboBox.setToolTipText("Error loading playback lines: " + error.getMessage());
                SwingUtils.showException("Load Playback Lines", "Failed to load playback lines", error);
            }
        ).execute();
    }
}
