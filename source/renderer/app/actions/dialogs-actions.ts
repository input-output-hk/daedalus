import Action from './lib/Action'; // ======= DIALOGS ACTIONS =======

export default class DialogsActions {
  open: Action<{
    dialog: (...args: Array<any>) => any;
  }> = new Action();
  updateDataForActiveDialog: Action<{
    data: Record<string, any>;
  }> = new Action();
  closeActiveDialog: Action<any> = new Action();
  resetActiveDialog: Action<any> = new Action();
}
