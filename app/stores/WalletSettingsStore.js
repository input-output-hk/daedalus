// @flow
import { observable, action } from 'mobx';
import Store from './lib/Store';
import Request from './lib/Request';
import globalMessages from '../i18n/global-messages';

export default class WalletSettingsStore extends Store {

  WALLET_UNIT_OPTIONS = [
    { value: 0, label: globalMessages.unitAda },
    { value: 1, label: globalMessages.unitLovelace },
  ];

  @observable updateWalletUnitRequest = new Request(this.api, 'setWalletUnit');

  setup() {
    const a = this.actions.walletSettings;
    a.updateWalletUnit.listen(this._updateWalletUnit);
  }

  @action _updateWalletUnit = ({ unit }: { unit: number }) => {
    const walletId = this.stores.wallets.active.id;
    this.updateWalletUnitRequest.execute(walletId, unit);
  };

}
