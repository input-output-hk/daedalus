// @flow
import { observable, action } from 'mobx';
import _ from 'lodash';
import Store from './lib/Store';
import Request from './lib/Request';
import globalMessages from '../i18n/global-messages';
import type { AssuranceMode } from '../types/transactionAssuranceTypes';

export default class WalletSettingsStore extends Store {

  WALLET_ASSURANCE_LEVEL_OPTIONS = [
    { value: 'CWANormal', label: globalMessages.assuranceLevelNormal },
    { value: 'CWAStrict', label: globalMessages.assuranceLevelStrict },
  ];

  @observable changeWalletPasswordRequest = new Request(this.api, 'changeWalletPassword');
  @observable setWalletPasswordRequest = new Request(this.api, 'setWalletPassword');
  @observable updateWalletRequest = new Request(this.api, 'updateWallet');

  setup() {
    const a = this.actions.walletSettings;
    a.changeWalletPassword.listen(this._changeWalletPassword);
    a.setWalletPassword.listen(this._setWalletPassword);
    a.updateWalletAssuranceLevel.listen(this._updateWalletAssuranceLevel);
  }

  @action _updateWalletAssuranceLevel = async ({ assurance }: { assurance: AssuranceMode }) => {
    const { id: walletId, type, currency, name } = this.stores.wallets.active;
    await this.updateWalletRequest.execute({ walletId, type, currency, name, assurance });
    await this.stores.wallets.walletsRequest.patch(result => {
      const wallet = _.find(result, { id: walletId });
      wallet.assurance = assurance;
    });
  };

  @action _changeWalletPassword = ({ walletId, oldPassword, newPassword }: {
    walletId: string, oldPassword: string, newPassword: string,
  }) => {
    this.changeWalletPasswordRequest.execute(walletId, oldPassword, newPassword);
  };

  @action _setWalletPassword = ({ walletId, password }: {
    walletId: string, password: string,
  }) => {
    this.setWalletPasswordRequest.execute(walletId, password);
  };

}
