// @flow
import Action from './lib/Action';

// ======= APP ACTIONS =======

export default class AppActions {
  downloadLogs: Action<any> = new Action();
  getGpuStatus: Action<any> = new Action();
  initAppEnvironment: Action<any> = new Action();
  setNotificationVisibility: Action<boolean> = new Action();
  toggleNewsFeed: Action<boolean> = new Action();

  // About dialog actions
  closeAboutDialog: Action<any> = new Action();
  openAboutDialog: Action<any> = new Action();

  // Block Consolidation dialog actions
  closeBlockConsolidationStatusDialog: Action<any> = new Action();
  openBlockConsolidationStatusDialog: Action<any> = new Action();

  // Daedalus Diagnostics dialog actions
  closeDaedalusDiagnosticsDialog: Action<any> = new Action();
  openDaedalusDiagnosticsDialog: Action<any> = new Action();
}
