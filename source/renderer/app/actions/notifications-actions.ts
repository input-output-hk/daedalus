import Action from './lib/Action';
import type {
  NotificationConfig,
  NotificationId,
} from '../types/notificationTypes'; // ======= NOTIFICATIONS ACTIONS =======

export default class NotificationsActions {
  registerNotification: Action<NotificationConfig> = new Action();
  closeActiveNotification: Action<any> = new Action();
  closeNotification: Action<{
    id: NotificationId;
  }> = new Action();
}
