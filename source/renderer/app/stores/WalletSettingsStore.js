// @flow
import { observable, action, runInAction, computed } from 'mobx';
import { findIndex } from 'lodash';
import Store from './lib/Store';
import Request from './lib/LocalizedRequest';
import Wallet from '../domains/Wallet';
import { WALLET_UTXO_API_REQUEST_INTERVAL } from '../config/timingConfig';
import { getRecoveryWalletIdChannel } from '../ipc/getRecoveryWalletIdChannel';
import { getStatusFromWalletData } from '../utils/walletRecoveryPhraseVerificationUtils';
import { getRawWalletId } from '../api/utils';
import type { WalletExportToFileParams } from '../actions/wallet-settings-actions';
import type { WalletUtxos } from '../api/wallets/types';
import type { WalletLocalData } from '../api/utils/localStorage';
import { RECOVERY_PHRASE_VERIFICATION_STATUSES } from '../config/walletRecoveryPhraseVerificationConfig';

export default class WalletSettingsStore extends Store {
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

  @observable walletFieldBeingEdited = null;
  @observable lastUpdatedWalletField = null;
  @observable walletUtxos: ?WalletUtxos = null;
  @observable recoveryPhraseStep = 0;

  pollingApiInterval: ?IntervalID = null;

  setup() {
    const {
      walletSettings: walletSettingsActions,
      sidebar: sidebarActions,
    } = this.actions;
    walletSettingsActions.startEditingWalletField.listen(
      this._startEditingWalletField
    );
    walletSettingsActions.stopEditingWalletField.listen(
      this._stopEditingWalletField
    );
    walletSettingsActions.cancelEditingWalletField.listen(
      this._cancelEditingWalletField
    );
    walletSettingsActions.updateWalletField.listen(this._updateWalletField);
    walletSettingsActions.updateSpendingPassword.listen(
      this._updateSpendingPassword
    );
    walletSettingsActions.exportToFile.listen(this._exportToFile);
    walletSettingsActions.startWalletUtxoPolling.listen(
      this._startWalletUtxoPolling
    );
    walletSettingsActions.stopWalletUtxoPolling.listen(
      this._stopWalletUtxoPolling
    );
    walletSettingsActions.recoveryPhraseVerificationContinue.listen(
      this._recoveryPhraseVerificationContinue
    );
    walletSettingsActions.recoveryPhraseVerificationCheck.listen(
      this._recoveryPhraseVerificationCheck
    );
    walletSettingsActions.recoveryPhraseVerificationClose.listen(
      this._recoveryPhraseVerificationClose
    );
    walletSettingsActions.toggleShowUsedAddresses.listen(
      this._toggleShowUsedAddressesStatuses
    );

    sidebarActions.walletSelected.listen(this._onWalletSelected);
  }

  // =================== PUBLIC API ==================== //

  // GETTERS

  getWalletsRecoveryPhraseVerificationData = (walletId: string) =>
    this.walletsRecoveryPhraseVerificationData[walletId] || {};

  getWalletShowUsedAddressesStatuses = (walletId: string) =>
    this.walletsShowUsedAddressesStatuses[walletId] || {};

  @computed get walletsShowUsedAddressesStatuses() {
    const { all: walletsLocalData } = this.stores.walletsLocal;
    // $FlowFixMe
    return Object.values(walletsLocalData).reduce(
      (
        obj,
        { id, showUsedAddresses }: WalletLocalData
      ) => {
        obj[id] = {
          id,
          showUsedAddresses,
        };
        return obj;
      },
      {}
    );
  }

  @computed get walletsRecoveryPhraseVerificationData() {
    const { all: walletsLocalData } = this.stores.walletsLocal;
    // $FlowFixMe
    return Object.values(walletsLocalData).reduce(
      (
        obj,
        { id, recoveryPhraseVerificationDate, creationDate }: WalletLocalData
      ) => {
        const {
          recoveryPhraseVerificationStatus,
          recoveryPhraseVerificationStatusType,
        } = getStatusFromWalletData({
          creationDate,
          recoveryPhraseVerificationDate,
        });
        const hasNotification =
          recoveryPhraseVerificationStatus ===
            RECOVERY_PHRASE_VERIFICATION_STATUSES.NOTIFICATION &&
          !global.isIncentivizedTestnet;
        obj[id] = {
          id,
          recoveryPhraseVerificationDate,
          creationDate,
          recoveryPhraseVerificationStatus,
          recoveryPhraseVerificationStatusType,
          hasNotification,
        };
        return obj;
      },
      {}
    );
  }

  // =================== PRIVATE API ==================== //

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
    isLegacy,
  }: {
    walletId: string,
    oldPassword: string,
    newPassword: string,
    isLegacy: boolean,
  }) => {
    await this.updateSpendingPasswordRequest.execute({
      walletId,
      oldPassword,
      newPassword,
      isLegacy,
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

    const { id: walletId, name, isLegacy } = activeWallet;
    const walletData = { walletId, name, isLegacy };
    walletData[field] = value;

    const wallet = await this.updateWalletRequest.execute({
      walletId: walletData.walletId,
      name: walletData.name,
      isLegacy: walletData.isLegacy,
    }).promise;

    if (!wallet) return;

    await this.stores.wallets.walletsRequest.patch((result) => {
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

  @action _startWalletUtxoPolling = () => {
    this._clearWalletUtxoPollingInterval();
    this._getWalletUtxoApiData();

    this.pollingApiInterval = setInterval(
      this._getWalletUtxoApiData,
      WALLET_UTXO_API_REQUEST_INTERVAL
    );
  };

  @action _stopWalletUtxoPolling = () => {
    this._clearWalletUtxoPollingInterval();
    this.getWalletUtxosRequest.reset();
  };

  @action _clearWalletUtxoPollingInterval = () => {
    if (this.pollingApiInterval) {
      clearInterval(this.pollingApiInterval);
      this.pollingApiInterval = null;
    }
  };

  @action _getWalletUtxoApiData = async () => {
    const activeWallet = this.stores.wallets.active;
    if (!activeWallet) return;

    const { id: walletId, isLegacy } = activeWallet;
    const walletUtxos = await this.getWalletUtxosRequest.execute({
      walletId,
      isLegacy,
    });
    this._updateWalletUtxos(walletUtxos);
  };

  @action _updateWalletUtxos = (walletUtxos: ?WalletUtxos) => {
    this.walletUtxos = walletUtxos;
  };

  @action _onWalletSelected = () => {
    this._updateWalletUtxos(null);
  };

  /* ==========================================================
=            Wallet Recovery Phrase Verification            =
========================================================== */

  @action _recoveryPhraseVerificationContinue = async () => {
    const step = this.recoveryPhraseStep;
    if (step === 4) this.recoveryPhraseStep = 2;
    else this.recoveryPhraseStep = step + 1;
  };

  @action _recoveryPhraseVerificationClose = async () => {
    this.recoveryPhraseStep = 0;
  };

  @action _recoveryPhraseVerificationCheck = async ({
    recoveryPhrase,
  }: {
    recoveryPhrase: Array<string>,
  }) => {
    const walletId = await getRecoveryWalletIdChannel.request(recoveryPhrase);
    if (!walletId)
      throw new Error('It was not possible to retrieve the walletId.');
    const activeWallet = this.stores.wallets.active;
    if (!activeWallet)
      throw new Error(
        'Active wallet required before checking recovery phrase.'
      );
    const activeWalletId = getRawWalletId(activeWallet.id);
    const isCorrect = walletId === activeWalletId;
    const nextStep = isCorrect ? 3 : 4;
    if (isCorrect) {
      const recoveryPhraseVerificationDate = new Date();
      await this.actions.walletsLocal.setWalletLocalData.trigger({
        walletId: activeWallet.id,
        updatedWalletData: { recoveryPhraseVerificationDate },
      });
    }
    runInAction(
      'AdaWalletBackupStore::_recoveryPhraseVerificationCheck',
      () => {
        this.recoveryPhraseStep = nextStep;
      }
    );
  };

  /* ====  End of Wallet Recovery Phrase Verification  ===== */

  @action _toggleShowUsedAddressesStatuses = async () => {
    const activeWallet = this.stores.wallets.active;
    if (!activeWallet)
      throw new Error(
        'Active wallet required before checking show used addresses statuses.'
      );
    const usedAddressesStatus = this.getWalletShowUsedAddressesStatuses(activeWallet.id);
    await this.actions.walletsLocal.setWalletLocalData.trigger({
      walletId: activeWallet.id,
      updatedWalletData: { showUsedAddresses: !usedAddressesStatus.showUsedAddresses },
    });
  };
}
