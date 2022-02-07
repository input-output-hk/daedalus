// @flow
import { observable, action } from 'mobx';
import Store from './lib/Store';

export default class UiDialogsStore extends Store {
  @observable activeDialog: ?Function = null;
  @observable secondsSinceActiveDialogIsOpen: number = 0;
  @observable dataForActiveDialog: Object = {};

  _secondsTimerInterval: ?IntervalID = null;

  setup() {
    this.actions.dialogs.open.listen(this._onOpen);
    this.actions.dialogs.closeActiveDialog.listen(this._onClose);
    this.actions.dialogs.resetActiveDialog.listen(this._reset);
    this.actions.dialogs.updateDataForActiveDialog.listen(
      this._onUpdateDataForActiveDialog
    );
  }

  isOpen = (dialog: Function): boolean => this.activeDialog === dialog;

  countdownSinceDialogOpened = (countDownTo: number) =>
    Math.max(countDownTo - this.secondsSinceActiveDialogIsOpen, 0);

  @action _onOpen = ({ dialog }: { dialog: Function }) => {
    this._reset();
    this.activeDialog = dialog;
    this.dataForActiveDialog = observable(dialog.defaultProps || {});
    this.secondsSinceActiveDialogIsOpen = 0;
    if (this._secondsTimerInterval) clearInterval(this._secondsTimerInterval);
    this._secondsTimerInterval = setInterval(this._updateSeconds, 1000);
  };

  @action _onClose = () => {
    this._reset();
  };

  @action _updateSeconds = () => {
    this.secondsSinceActiveDialogIsOpen += 1;
  };

  @action _onUpdateDataForActiveDialog = ({ data }: { data: Object }) => {
    Object.assign(this.dataForActiveDialog, data);
  };

  @action _reset = () => {
    this.activeDialog = null;
    this.secondsSinceActiveDialogIsOpen = 0;
    this.dataForActiveDialog = {};
  };
}
