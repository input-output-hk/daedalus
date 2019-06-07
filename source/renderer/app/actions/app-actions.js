// @flow
import Action from './lib/Action';

// ======= APP ACTIONS =======

export default class AppActions {
  openAboutDialog: Action<any> = new Action();
  closeAboutDialog: Action<any> = new Action();
  openDaedalusDiagnosticsDialog: Action<any> = new Action();
  closeDaedalusDiagnosticsDialog: Action<any> = new Action();
  getGpuStatus: Action<any> = new Action();
  initAppEnvironment: Action<any> = new Action();
  toggleBlockConsolidationStatusScreen: Action<any> = new Action();
  downloadLogs: Action<any> = new Action();
  setNotificationVisibility: Action<boolean> = new Action();
}
