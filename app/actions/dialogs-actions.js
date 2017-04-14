// @flow
import { Action } from './lib/actions';

// ======= DIALOGS ACTIONS =======

export type DialogsActions = {
  open: Action<{ dialog: Function }>,
  updateDataForActiveDialog: Action<{ data: Object }>,
  closeActiveDialog: Action<any>,
  resetActiveDialog: Action<any>,
};

const dialogActions: DialogsActions = {
  open: new Action(),
  updateDataForActiveDialog: new Action(),
  closeActiveDialog: new Action(),
  resetActiveDialog: new Action(),
};

export default dialogActions;
