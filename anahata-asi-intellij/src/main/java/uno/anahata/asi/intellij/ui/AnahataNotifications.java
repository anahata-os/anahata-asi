/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.intellij.ui;

import com.intellij.notification.NotificationGroupManager;
import com.intellij.notification.NotificationType;
import com.intellij.openapi.project.Project;

/**
 * Thin helper for surfacing Anahata events as IDE balloon notifications.
 * <p>
 * Notifications are posted to the {@code Anahata ASI} notification group (declared in
 * {@code plugin.xml}), so users can control their display and see them in the Event Log.
 * </p>
 *
 * @author anahata
 */
public final class AnahataNotifications {

    /**
     * The notification group id (must match the {@code notificationGroup} in plugin.xml).
     */
    private static final String GROUP_ID = "Anahata ASI";

    /**
     * Non-instantiable utility holder.
     */
    private AnahataNotifications() {
    }

    /**
     * Shows an informational balloon notification.
     *
     * @param project the project context (may be {@code null} for an application-level balloon).
     * @param content the message body.
     */
    public static void info(Project project, String content) {
        NotificationGroupManager.getInstance()
                .getNotificationGroup(GROUP_ID)
                .createNotification(content, NotificationType.INFORMATION)
                .notify(project);
    }
}
