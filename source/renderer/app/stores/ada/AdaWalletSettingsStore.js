// @flow
import { observable, action } from 'mobx';
import { findIndex } from 'lodash';
import WalletSettingsStore from '../WalletSettingsStore';
import Wallet from '../../domains/Wallet';
import Request from '../lib/LocalizedRequest';
import type { WalletExportToFileParams } from '../../actions/ada/wallet-settings-actions';

export default class AdaWalletSettingsStore extends WalletSettingsStore {

  /* eslint-disable max-len */
  @observable updateWalletRequest: Request<Wallet> = new Request(this.api.ada.updateWallet);
  @observable updateSpendingPasswordRequest: Request<boolean> = new Request(this.api.ada.updateSpendingPassword);
  @observable exportWalletToFileRequest: Request<Promise<[]>> = new Request(this.api.ada.exportWalletToFile);
  /* eslint-enable max-len */

  setup() {
    const a = this.actions.walletSettings;
    a.startEditingWalletField.listen(this._startEditingWalletField);
    a.stopEditingWalletField.listen(this._stopEditingWalletField);
    a.cancelEditingWalletField.listen(this._cancelEditingWalletField);
    a.updateWalletField.listen(this._updateWalletField);
    a.updateSpendingPassword.listen(this._updateSpendingPassword);
    a.exportToFile.listen(this._exportToFile);
  }

  @action _updateSpendingPassword = async ({ walletId, oldPassword, newPassword }: {
    walletId: string, oldPassword: ?string, newPassword: ?string,
  }) => {
    await this.updateSpendingPasswordRequest.execute({ walletId, oldPassword, newPassword });
    this.actions.dialogs.closeActiveDialog.trigger();
    this.updateSpendingPasswordRequest.reset();
    this.stores.ada.wallets.refreshWalletsData();
  };

  @action _updateWalletField = async ({ field, value }: { field: string, value: string }) => {
    const activeWallet = this.stores.ada.wallets.active;
    if (!activeWallet) return;

    const { id: walletId, name, assurance } = activeWallet;
    const walletData = { walletId, name, assurance };
    walletData[field] = value;

    const wallet = await this.updateWalletRequest.execute({
      walletId: walletData.walletId,
      name: walletData.name,
      assuranceLevel: walletData.assurance
    }).promise;

    if (!wallet) return;

    await this.stores.ada.wallets.walletsRequest.patch(result => {
      const walletIndex = findIndex(result, { id: walletId });
      result[walletIndex] = wallet;
    });
    this.updateWalletRequest.reset();
    this.stores.ada.wallets._setActiveWallet({ walletId });
  };

  @action _exportToFile = async (params: WalletExportToFileParams) => {
    const { walletId, filePath, password } = params;
    await this.exportWalletToFileRequest.execute({ walletId, filePath, password });
    this.actions.dialogs.closeActiveDialog.trigger();
  }

}
