// @flow
import Action from './lib/Action';

// ======= NODE UPDATE ACTIONS =======

export default class AppUpdateActions {
  getLatestAvailableAppVersion: Action<any> = new Action();
  installUpdate: Action<any> = new Action();
  openAppUpdateOverlay: Action<any> = new Action();
  closeAppUpdateOverlay: Action<any> = new Action();
}
