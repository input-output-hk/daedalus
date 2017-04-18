// @flow
import Action from './lib/Action';

// ======= DIALOGS ACTIONS =======

export default class DialogsActions {
  open: Action<{ dialog: Function }> = new Action();
  updateDataForActiveDialog: Action<{ data: Object }> = new Action();
  closeActiveDialog: Action<any> = new Action();
  resetActiveDialog: Action<any> = new Action();
}
