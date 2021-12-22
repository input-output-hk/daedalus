import Action from './lib/Action'; // ======= APP UPDATE ACTIONS =======

export default class AppUpdateActions {
  installUpdate: Action<any> = new Action();
  postponeUpdate: Action<any> = new Action();
  openAppUpdateOverlay: Action<any> = new Action();
  closeAppUpdateOverlay: Action<any> = new Action();
}
