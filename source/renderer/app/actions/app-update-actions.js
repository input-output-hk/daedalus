// @flow
import Action from './lib/Action';

// ======= NODE UPDATE ACTIONS =======

export default class AppUpdateActions {
  acceptAppUpdate: Action<any> = new Action();
  postponeAppUpdate: Action<any> = new Action();
  getLatestAvailableAppVersion: Action<any> = new Action();
}
