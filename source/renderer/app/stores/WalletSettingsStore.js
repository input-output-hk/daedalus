// @flow
import { observable, action } from 'mobx';
import Store from './lib/Store';
import globalMessages from '../i18n/global-messages';
import { WalletAssuranceModeOptions } from '../domains/Wallet';

export default class WalletSettingsStore extends Store {

  WALLET_ASSURANCE_LEVEL_OPTIONS = [
    { value: WalletAssuranceModeOptions.NORMAL, label: globalMessages.assuranceLevelNormal },
    { value: WalletAssuranceModeOptions.STRICT, label: globalMessages.assuranceLevelStrict },
  ];

  @observable walletFieldBeingEdited = null;
  @observable lastUpdatedWalletField = null;

  @action _startEditingWalletField = ({ field }: { field: string }) => {
    this.walletFieldBeingEdited = field;
  };

  @action _stopEditingWalletField = () => {
    if (this.walletFieldBeingEdited) {
      this.lastUpdatedWalletField = this.walletFieldBeingEdited;
    }
    this.walletFieldBeingEdited = null;
  };

  @action _cancelEditingWalletField = () => {
    this.lastUpdatedWalletField = null;
    this.walletFieldBeingEdited = null;
  };

}
