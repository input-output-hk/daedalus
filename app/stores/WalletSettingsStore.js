// @flow
import { observable, action } from 'mobx';
import _ from 'lodash';
import Store from './lib/Store';
import Request from './lib/LocalizedRequest';
import globalMessages from '../i18n/global-messages';
import type { AssuranceMode } from '../types/transactionAssuranceTypes';
import type {
  UpdateWalletResponse,
  ChangeWalletPasswordResponse,
  SetWalletPasswordResponse,
} from '../api';

export default class WalletSettingsStore extends Store {

  WALLET_ASSURANCE_LEVEL_OPTIONS = [
    { value: 'CWANormal', label: globalMessages.assuranceLevelNormal },
    { value: 'CWAStrict', label: globalMessages.assuranceLevelStrict },
  ];

  @observable updateWalletRequest: Request<UpdateWalletResponse> = new Request(
    this.api.updateWallet
  );

  /* eslint-disable max-len */
  @observable changeWalletPasswordRequest: Request<ChangeWalletPasswordResponse> = new Request(this.api.changeWalletPassword);
  @observable setWalletPasswordRequest: Request<SetWalletPasswordResponse> = new Request(this.api.setWalletPassword);
  @observable updateWalletRequest: Request<UpdateWalletResponse> = new Request(this.api.updateWallet);
  /* eslint-enable max-len */

  setup() {
    const a = this.actions.walletSettings;
    a.changeWalletPassword.listen(this._changeWalletPassword);
    a.setWalletPassword.listen(this._setWalletPassword);
    a.updateWalletAssuranceLevel.listen(this._updateWalletAssuranceLevel);
  }

  @action _updateWalletAssuranceLevel = async ({ assurance }: { assurance: AssuranceMode }) => {
    const activeWallet = this.stores.wallets.active;
    if (!activeWallet) return;
    const { id: walletId, type, currency, name } = activeWallet;
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
