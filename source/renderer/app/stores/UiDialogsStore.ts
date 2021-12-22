import { observable, action } from 'mobx';
import Store from './lib/Store';

export default class UiDialogsStore extends Store {
  @observable
  activeDialog: ((...args: Array<any>) => any) | null | undefined = null;
  @observable
  secondsSinceActiveDialogIsOpen = 0;
  @observable
  dataForActiveDialog: Record<string, any> = {};
  // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'IntervalID'.
  _secondsTimerInterval: IntervalID | null | undefined = null;

  setup() {
    this.actions.dialogs.open.listen(this._onOpen);
    this.actions.dialogs.closeActiveDialog.listen(this._onClose);
    this.actions.dialogs.resetActiveDialog.listen(this._reset);
    this.actions.dialogs.updateDataForActiveDialog.listen(
      this._onUpdateDataForActiveDialog
    );
  }

  isOpen = (dialog: (...args: Array<any>) => any): boolean =>
    this.activeDialog === dialog;
  countdownSinceDialogOpened = (countDownTo: number) =>
    Math.max(countDownTo - this.secondsSinceActiveDialogIsOpen, 0);
  @action
  _onOpen = ({ dialog }: { dialog: (...args: Array<any>) => any }) => {
    this._reset();

    this.activeDialog = dialog;
    // @ts-ignore ts-migrate(2339) FIXME: Property 'defaultProps' does not exist on type '(.... Remove this comment to see the full error message
    this.dataForActiveDialog = observable(dialog.defaultProps || {});
    this.secondsSinceActiveDialogIsOpen = 0;
    if (this._secondsTimerInterval) clearInterval(this._secondsTimerInterval);
    this._secondsTimerInterval = setInterval(this._updateSeconds, 1000);
  };
  @action
  _onClose = () => {
    this._reset();
  };
  @action
  _updateSeconds = () => {
    this.secondsSinceActiveDialogIsOpen += 1;
  };
  @action
  _onUpdateDataForActiveDialog = ({ data }: { data: Record<string, any> }) => {
    Object.assign(this.dataForActiveDialog, data);
  };
  @action
  _reset = () => {
    this.activeDialog = null;
    this.secondsSinceActiveDialogIsOpen = 0;
    this.dataForActiveDialog = {};
  };
}
