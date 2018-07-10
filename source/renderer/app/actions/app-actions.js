// @flow
import Action from './lib/Action';

// ======= APP ACTIONS =======

export default class AppActions {
  openAboutDialog: Action<any> = new Action();
  closeAboutDialog: Action<any> = new Action();
  updateGpuStatus: Action<any> = new Action();
}
