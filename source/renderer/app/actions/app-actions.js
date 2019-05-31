// @flow
import Action from './lib/Action';

// ======= APP ACTIONS =======

export default class AppActions {
  openAboutDialog: Action<any> = new Action();
  closeAboutDialog: Action<any> = new Action();
  openNetworkStatusDialog: Action<any> = new Action();
  closeNetworkStatusDialog: Action<any> = new Action();
  getGpuStatus: Action<any> = new Action();
  initAppEnvironment: Action<any> = new Action();
  toggleBlockConsolidationStatusScreen: Action<any> = new Action();
}
