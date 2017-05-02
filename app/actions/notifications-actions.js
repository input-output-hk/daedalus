// @flow
import Action from './lib/Action';

// ======= NOTIFICATIONS ACTIONS =======

export default class NotificationsActions {
  open: Action<{ notification: Function }> = new Action();
  updateDataForActiveNotification: Action<{ data: Object }> = new Action();
  closeActiveNotification: Action<any> = new Action();
  resetActiveNotification: Action<any> = new Action();
}
