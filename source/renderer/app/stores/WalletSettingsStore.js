// @flow
import { observable, action, runInAction } from 'mobx';
import { findIndex } from 'lodash';
import Store from './lib/Store';
import Request from './lib/LocalizedRequest';
import globalMessages from '../i18n/global-messages';
import Wallet, { WalletAssuranceModeOptions } from '../domains/Wallet';
import type { WalletExportToFileParams } from '../actions/wallet-settings-actions';
import type { WalletUtxos } from '../api/wallets/types';

export default class WalletSettingsStore extends Store {
  WALLET_ASSURANCE_LEVEL_OPTIONS = [
    {
      value: WalletAssuranceModeOptions.NORMAL,
      label: globalMessages.assuranceLevelNormal,
    },
    {
      value: WalletAssuranceModeOptions.STRICT,
      label: globalMessages.assuranceLevelStrict,
    },
  ];

  /* eslint-disable max-len */
  @observable updateWalletRequest: Request<Wallet> = new Request(
    this.api.ada.updateWallet
  );
  @observable updateSpendingPasswordRequest: Request<boolean> = new Request(
    this.api.ada.updateSpendingPassword
  );
  @observable exportWalletToFileRequest: Request<Promise<[]>> = new Request(
    this.api.ada.exportWalletToFile
  );
  @observable getWalletUtxosRequest: Request<WalletUtxos> = new Request(
    this.api.ada.getWalletUtxos
  );

  /* eslint-enable max-len */

  @observable walletFieldBeingEdited = null;
  @observable lastUpdatedWalletField = null;
  @observable walletUtxos: ?WalletUtxos = null;

  setup() {
    const a = this.actions.walletSettings;
    a.startEditingWalletField.listen(this._startEditingWalletField);
    a.stopEditingWalletField.listen(this._stopEditingWalletField);
    a.cancelEditingWalletField.listen(this._cancelEditingWalletField);
    a.updateWalletField.listen(this._updateWalletField);
    a.updateSpendingPassword.listen(this._updateSpendingPassword);
    a.exportToFile.listen(this._exportToFile);
    a.getWalletUtxos.listen(this._getWalletUtxos);
  }

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

  @action _updateSpendingPassword = async ({
    walletId,
    oldPassword,
    newPassword,
  }: {
    walletId: string,
    oldPassword: ?string,
    newPassword: ?string,
  }) => {
    await this.updateSpendingPasswordRequest.execute({
      walletId,
      oldPassword,
      newPassword,
    });
    this.actions.dialogs.closeActiveDialog.trigger();
    this.updateSpendingPasswordRequest.reset();
    this.stores.wallets.refreshWalletsData();
  };

  @action _updateWalletField = async ({
    field,
    value,
  }: {
    field: string,
    value: string,
  }) => {
    const activeWallet = this.stores.wallets.active;
    if (!activeWallet) return;

    const { id: walletId, name, assurance } = activeWallet;
    const walletData = { walletId, name, assurance };
    walletData[field] = value;

    const wallet = await this.updateWalletRequest.execute({
      walletId: walletData.walletId,
      name: walletData.name,
      assuranceLevel: walletData.assurance,
    }).promise;

    if (!wallet) return;

    await this.stores.wallets.walletsRequest.patch(result => {
      const walletIndex = findIndex(result, { id: walletId });
      result[walletIndex] = wallet;
    });
    this.updateWalletRequest.reset();
    this.stores.wallets.refreshWalletsData();
  };

  @action _exportToFile = async (params: WalletExportToFileParams) => {
    const { walletId, filePath, password } = params;
    await this.exportWalletToFileRequest.execute({
      walletId,
      filePath,
      password,
    });
    this.actions.dialogs.closeActiveDialog.trigger();
  };

  @action _getWalletUtxos = async () => {
    this.walletUtxos = null;
    const activeWallet = this.stores.wallets.active;
    if (!activeWallet) return;
    const { id: walletId } = activeWallet;
    const walletUtxos = await this.getWalletUtxosRequest.execute({ walletId });
    runInAction(() => {
      this.walletUtxos = walletUtxos;
    });
  };
}
