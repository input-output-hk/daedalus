// @flow
import Action from './lib/Action';

// ======= NODE UPDATE ACTIONS =======

export default class AppUpdateActions {
  getLatestAvailableAppVersion: Action<any> = new Action();
  onInstallUpdate: Action<any> = new Action();
}
