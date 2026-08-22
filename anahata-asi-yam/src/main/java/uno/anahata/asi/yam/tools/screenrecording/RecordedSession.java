/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.yam.tools.screenrecording;

import java.nio.file.Path;
import lombok.Builder;

/**
 * Encapsulates the artifacts produced during an automated screen recording session.
 * <p>
 * Binds the finalized MP4 video file path, captured thumbnail screenshot path,
 * and the elapsed recording duration in seconds.
 * </p>
 *
 * @param videoPath The absolute path to the recorded .mp4 file.
 * @param thumbnailPath The absolute path to the captured .png thumbnail frame.
 * @param durationSeconds The elapsed recording duration in seconds.
 * 
 * @author anahata
 */
@Builder
public record RecordedSession(
        Path videoPath,
        Path thumbnailPath,
        double durationSeconds
) {
}
