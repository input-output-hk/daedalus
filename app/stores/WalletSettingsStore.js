// @flow
import { observable, action } from 'mobx';
import _ from 'lodash';
import Store from './lib/Store';
import Request from './lib/LocalizedRequest';
import globalMessages from '../i18n/global-messages';
import type { AssuranceMode } from '../types/transactionAssuranceTypes';
import type {
  UpdateWalletResponse,
  UpdateWalletPasswordResponse,
} from '../api';

export default class WalletSettingsStore extends Store {

  WALLET_ASSURANCE_LEVEL_OPTIONS = [
    { value: 'CWANormal', label: globalMessages.assuranceLevelNormal },
    { value: 'CWAStrict', label: globalMessages.assuranceLevelStrict },
  ];

  /* eslint-disable max-len */
  @observable updateWalletRequest: Request<UpdateWalletResponse> = new Request(this.api.updateWallet);
  @observable updateWalletPasswordRequest: Request<UpdateWalletPasswordResponse> = new Request(this.api.updateWalletPassword);
  /* eslint-enable max-len */

  @observable walletFieldBeingEdited = null;
  @observable lastUpdatedWalletField = null;

  setup() {
    const a = this.actions.walletSettings;
    a.startEditingWalletField.listen(this._startEditingWalletField);
    a.stopEditingWalletField.listen(this._stopEditingWalletField);
    a.cancelEditingWalletField.listen(this._cancelEditingWalletField);
    a.updateWalletField.listen(this._updateWalletField);
    a.updateWalletPassword.listen(this._updateWalletPassword);
    a.updateWalletAssuranceLevel.listen(this._updateWalletAssuranceLevel);
  }

  @action _updateWalletAssuranceLevel = async ({ assurance }: { assurance: AssuranceMode }) => {
    const activeWallet = this.stores.wallets.active;
    if (!activeWallet) return;
    const { id: walletId, name } = activeWallet;
    const wallet = await this.updateWalletRequest.execute({ walletId, name, assurance }).promise;
    if (!wallet) return;
    await this.stores.wallets.walletsRequest.patch(result => {
      const walletIndex = _.findIndex(result, { id: walletId });
      result[walletIndex] = wallet;
    });
    this.stores.wallets._setActiveWallet({ walletId });
  };

  @action _updateWalletPassword = async ({ walletId, oldPassword, newPassword }: {
    walletId: string, oldPassword: ?string, newPassword: ?string,
  }) => {
    await this.updateWalletPasswordRequest.execute({ walletId, oldPassword, newPassword });
    this.actions.dialogs.closeActiveDialog.trigger();
    this.updateWalletPasswordRequest.reset();
    await this.stores.wallets.refreshWalletsData();
    this.stores.wallets._setActiveWallet({ walletId });
  };

  @action _updateWalletField = async ({ field, value }: { field: string, value: string }) => {
    const activeWallet = this.stores.wallets.active;
    if (!activeWallet) return;
    const { id: walletId, name, assurance } = activeWallet;
    const walletData = { walletId, name, assurance };
    walletData[field] = value;
    const wallet = await this.updateWalletRequest.execute(walletData).promise;
    if (!wallet) return;
    await this.stores.wallets.walletsRequest.patch(result => {
      const walletIndex = _.findIndex(result, { id: walletId });
      result[walletIndex] = wallet;
    });
    this.stores.wallets._setActiveWallet({ walletId });
  };

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
