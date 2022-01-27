import Action from './lib/Action'; // ======= APP ACTIONS =======

export default class AppActions {
  downloadLogs: Action<any> = new Action();
  getGpuStatus: Action<any> = new Action();
  initAppEnvironment: Action<any> = new Action();
  setIsDownloadingLogs: Action<boolean> = new Action();
  toggleNewsFeed: Action<boolean> = new Action();
  closeNewsFeed: Action<any> = new Action();
  onUiClicked: Action<any> = new Action();
  // About dialog actions
  closeAboutDialog: Action<any> = new Action();
  openAboutDialog: Action<any> = new Action();
  // Daedalus Diagnostics dialog actions
  closeDaedalusDiagnosticsDialog: Action<any> = new Action();
  openDaedalusDiagnosticsDialog: Action<any> = new Action();
}
