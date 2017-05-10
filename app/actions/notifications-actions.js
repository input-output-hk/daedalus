// @flow
import Action from './lib/Action';

// ======= NOTIFICATIONS ACTIONS =======

export default class NotificationsActions {
  open: Action<{ id: string, duration?: number }> = new Action();
  updateDataForActiveNotification: Action<{ data: Object }> = new Action();
  closeActiveNotification: Action<{ id: string }>= new Action();
  resetActiveNotification: Action<any> = new Action();
}
