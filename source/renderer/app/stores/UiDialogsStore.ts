import { ReactNode } from 'react';
import { observable, action, makeObservable } from 'mobx';
import Store from './lib/Store';
import WalletReceiveDialog from '../components/wallet/receive/WalletReceiveDialog';
import AssetSettingsDialog from '../components/assets/AssetSettingsDialog';
import DelegationSetupWizardDialog from '../components/staking/delegation-setup-wizard/DelegationSetupWizardDialog';
import { AnalyticsTracker, EventCategories } from '../analytics';
import { Api } from '../api';
import { ActionsMap } from '../actions';

export default class UiDialogsStore extends Store {
  activeDialog: object = null;
  secondsSinceActiveDialogIsOpen = 0;
  dataForActiveDialog: Record<string, any> = {};
  // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'IntervalID'.
  _secondsTimerInterval: IntervalID | null | undefined = null;

  constructor(
    protected api: Api,
    protected actions: ActionsMap,
    protected analytics: AnalyticsTracker
  ) {
    super(api, actions, analytics);

    makeObservable(this, {
      activeDialog: observable,
      secondsSinceActiveDialogIsOpen: observable,
      dataForActiveDialog: observable,
      _onOpen: action,
      _onClose: action,
      _updateSeconds: action,
      _onUpdateDataForActiveDialog: action,
      _reset: action,
    });
  }

  setup() {
    this.actions.dialogs.open.listen(this._onOpen);
    this.actions.dialogs.closeActiveDialog.listen(this._onClose);
    this.actions.dialogs.resetActiveDialog.listen(this._reset);
    this.actions.dialogs.updateDataForActiveDialog.listen(
      this._onUpdateDataForActiveDialog
    );
  }

  isOpen = (dialog: ReactNode): boolean => this.activeDialog === dialog;
  countdownSinceDialogOpened = (countDownTo: number) =>
    Math.max(countDownTo - this.secondsSinceActiveDialogIsOpen, 0);
  _onOpen = ({ dialog }: { dialog: object }) => {
    this._reset();

    this.activeDialog = dialog as object;
    // @ts-ignore ts-migrate(2339) FIXME: Property 'defaultProps' does not exist on type '(.... Remove this comment to see the full error message
    this.dataForActiveDialog = observable(dialog.defaultProps || {});
    this.secondsSinceActiveDialogIsOpen = 0;
    if (this._secondsTimerInterval) clearInterval(this._secondsTimerInterval);
    this._secondsTimerInterval = setInterval(this._updateSeconds, 1000);

    this._handleAnalytics(dialog);
  };
  _onClose = () => {
    this._reset();
  };
  _updateSeconds = () => {
    this.secondsSinceActiveDialogIsOpen += 1;
  };
  _onUpdateDataForActiveDialog = ({ data }: { data: Record<string, any> }) => {
    Object.assign(this.dataForActiveDialog, data);
  };
  _reset = () => {
    this.activeDialog = null;
    this.secondsSinceActiveDialogIsOpen = 0;
    this.dataForActiveDialog = {};
  };
  _handleAnalytics = (dialog: object) => {
    switch (dialog) {
      case WalletReceiveDialog:
        this.analytics.sendEvent(
          EventCategories.WALLETS,
          'Opened share wallet address modal'
        );
        break;

      case AssetSettingsDialog:
        this.analytics.sendEvent(
          EventCategories.WALLETS,
          'Opened native token settings'
        );
        break;

      case DelegationSetupWizardDialog:
        this.analytics.sendEvent(
          EventCategories.STAKE_POOLS,
          'Opened delegate wallet dialog'
        );
        break;

      default:
        break;
    }
  };
}
