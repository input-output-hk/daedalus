// @flow
import { observable, action } from 'mobx';
import { findIndex, merge } from 'lodash';
import WalletSettingsStore from '../WalletSettingsStore';
import Request from '../lib/LocalizedRequest';
import type { WalletExportToFileParams } from '../../actions/ada/wallet-settings-actions';
import type { ExportWalletToFileResponse } from '../../api/ada/index';
import type { UpdateWalletPasswordResponse, UpdateWalletResponse } from '../../api/common';

export default class EtcWalletSettingsStore extends WalletSettingsStore {

  /* eslint-disable max-len */
  @observable updateWalletRequest: Request<UpdateWalletResponse> = new Request(this.api.ada.updateWallet);
  @observable updateWalletPasswordRequest: Request<UpdateWalletPasswordResponse> = new Request(this.api.ada.updateWalletPassword);
  @observable exportWalletToFileRequest: Request<ExportWalletToFileResponse> = new Request(this.api.ada.exportWalletToFile);
  /* eslint-enable max-len */

  setup() {
    const a = this.actions.ada.walletSettings;
    a.startEditingWalletField.listen(this._startEditingWalletField);
    a.stopEditingWalletField.listen(this._stopEditingWalletField);
    a.cancelEditingWalletField.listen(this._cancelEditingWalletField);
    a.updateWalletField.listen(this._updateWalletField);
    a.updateWalletPassword.listen(this._updateWalletPassword);
    a.exportToFile.listen(this._exportToFile);
  }

  @action _updateWalletPassword = async ({ walletId, oldPassword, newPassword }: {
    walletId: string, oldPassword: ?string, newPassword: ?string,
  }) => {
    await this.updateWalletPasswordRequest.execute({ walletId, oldPassword, newPassword });
    this.actions.dialogs.closeActiveDialog.trigger();
    this.updateWalletPasswordRequest.reset();
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
      // TODO: revert to this original update after full transition to V1 Api
      // result[walletIndex] = wallet;
      const existingWalletData = result[walletIndex];
      result[walletIndex] = merge(existingWalletData, wallet);
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
