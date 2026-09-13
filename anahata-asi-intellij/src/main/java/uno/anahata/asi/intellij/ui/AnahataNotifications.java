/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.intellij.ui;

import com.intellij.notification.Notification;
import com.intellij.notification.NotificationGroup;
import com.intellij.notification.NotificationGroupManager;
import com.intellij.notification.NotificationType;
import com.intellij.openapi.project.Project;
import lombok.extern.slf4j.Slf4j;

/**
 * Thin helper for surfacing Anahata events as IDE balloon notifications.
 * <p>
 * Notifications are posted to the {@code Anahata ASI} notification group (declared in
 * {@code plugin.xml}), so users can control their display and see them in the Event Log.
 * If the notification group is temporarily unavailable (e.g. during dynamic plugin unload
 * or reload), it gracefully falls back to direct {@link Notification} instantiation.
 * </p>
 *
 * @author anahata
 */
@Slf4j
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
     * Dispatches a notification to the IDE, safely falling back to direct {@link Notification}
     * instantiation if the {@code Anahata ASI} notification group is not currently registered
     * with {@link NotificationGroupManager} (for example, during dynamic plugin reload or unload).
     *
     * @param project the project context (may be {@code null} for an application-level balloon).
     * @param content the notification message body.
     * @param type    the notification severity type.
     */
    private static void show(Project project, String content, NotificationType type) {
        try {
            NotificationGroup group = NotificationGroupManager.getInstance().getNotificationGroup(GROUP_ID);
            Notification notification;
            if (group != null) {
                notification = group.createNotification(content, type);
            } else {
                notification = new Notification(GROUP_ID, content, type);
            }
            notification.notify(project);
        } catch (Throwable t) {
            log.warn("Failed to display IDE notification: {}", content, t);
        }
    }

    /**
     * Shows an informational balloon notification.
     *
     * @param project the project context (may be {@code null} for an application-level balloon).
     * @param content the message body.
     */
    public static void info(Project project, String content) {
        show(project, content, NotificationType.INFORMATION);
    }

    /**
     * Shows a warning balloon notification.
     *
     * @param project the project context (may be {@code null} for an application-level balloon).
     * @param content the message body.
     */
    public static void warn(Project project, String content) {
        show(project, content, NotificationType.WARNING);
    }

    /**
     * Shows an error balloon notification.
     *
     * @param project the project context (may be {@code null} for an application-level balloon).
     * @param content the message body.
     */
    public static void error(Project project, String content) {
        show(project, content, NotificationType.ERROR);
    }
}
