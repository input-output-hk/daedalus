// @flow
import Action from './lib/Action';
import type { StoredNotification } from '../types/notificationType';

// ======= NOTIFICATIONS ACTIONS =======

export default class NotificationsActions {
  registerNotification: Action<StoredNotification>;
  closeActiveNotification: Action<any> = new Action();
  closeNotification: Action<{ id: string }> = new Action();
}
