// @flow
import { observable, action, runInAction, computed } from 'mobx';
import { findIndex } from 'lodash';
import Store from './lib/Store';
import Request from './lib/LocalizedRequest';
import Wallet from '../domains/Wallet';
import { WALLET_UTXO_API_REQUEST_INTERVAL } from '../config/timingConfig';
import { getRecoveryWalletIdChannel } from '../ipc/getRecoveryWalletIdChannel';
import { getStatusFromWalletData } from '../utils/walletRecoveryPhraseVerificationUtils';
import { getRawWalletId, WalletIdPrefixes } from '../api/utils';
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

    sidebarActions.walletSelected.listen(this._onWalletSelected);
  }

  // =================== PUBLIC API ==================== //

  // GETTERS

  getWalletsRecoveryPhraseVerificationData = (walletId: string) =>
    this.walletsRecoveryPhraseVerificationData[walletId] || {};

  @computed get walletsRecoveryPhraseVerificationData() {
    const { all: walletsLocalData } = this.stores.walletsLocal;
    const { isIncentivizedTestnet } = global;
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
          !isIncentivizedTestnet;
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
    const {
      isHardwareWalletRoute,
      active,
      activeHardwareWallet,
    } = this.stores.wallets;
    const activeWallet = isHardwareWalletRoute ? activeHardwareWallet : active;
    if (!activeWallet) return;

    const { id: walletId, name, isLegacy, isHardwareWallet } = activeWallet;
    const walletData = { walletId, name, isLegacy, isHardwareWallet };
    walletData[field] = value;

    const wallet = await this.updateWalletRequest.execute({
      walletId: walletData.walletId,
      name: walletData.name,
      isLegacy: walletData.isLegacy,
      isHardwareWallet: walletData.isHardwareWallet,
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

  @action _startWalletUtxoPolling = () => {
    this._getWalletUtxoApiData();
    this._stopWalletUtxoPolling();

    this.pollingApiInterval = setInterval(
      this._getWalletUtxoApiData,
      WALLET_UTXO_API_REQUEST_INTERVAL
    );
  };

  @action _stopWalletUtxoPolling = () => {
    if (this.pollingApiInterval) clearInterval(this.pollingApiInterval);
  };

  @action _getWalletUtxoApiData = async () => {
    const {
      active,
      activeHardwareWallet,
      isHardwareWalletRoute,
    } = this.stores.wallets;
    const activeWallet = isHardwareWalletRoute ? activeHardwareWallet : active;
    if (!activeWallet) return;

    const { id, isLegacy } = activeWallet;
    const walletId = isHardwareWalletRoute
      ? getRawWalletId(id, WalletIdPrefixes.HARDWARE_WALLET)
      : id;
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
}
