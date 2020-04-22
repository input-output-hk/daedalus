// @flow
import { observable, action, runInAction } from 'mobx';
import { findIndex } from 'lodash';
import moment from 'moment';
import Store from './lib/Store';
import Request from './lib/LocalizedRequest';
import Wallet from '../domains/Wallet';
import { asyncForEach } from '../utils/asyncForEach';
import type { WalletExportToFileParams } from '../actions/wallet-settings-actions';
import type { WalletUtxos } from '../api/wallets/types';
import { WALLET_UTXO_API_REQUEST_INTERVAL } from '../config/timingConfig';
import {
  RECOVERY_PHRASE_VERIFICATION_NOTIFICATION,
  RECOVERY_PHRASE_VERIFICATION_WARNING,
  RECOVERY_PHRASE_VERIFICATION_STATUSES,
  WALLET_RECOVERY_PHRASE_VERIFICATION_TYPES,
} from '../config/walletRecoveryPhraseVerificationConfig';
import type {
  WalletLocalData,
  WalletsLocalData,
} from '../api/utils/localStorage';
import { getRecoveryWalletIdChannel } from '../ipc/getRecoveryWalletIdChannel';
import { getRawWalletId } from '../api/utils';
import type {
  WalletRecoveryPhraseVerificationData,
  RecoveryPhraseVerificationData,
} from '../types/walletRecoveryPhraseVerificationTypes';

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
  @observable forceWalletResyncRequest: Request<void> = new Request(
    this.api.ada.forceWalletResync
  );
  @observable setWalletLocalDataRequest: Request<any> = new Request(
    this.api.localStorage.setWalletLocalData
  );
  @observable updateWalletLocalDataRequest: Request<any> = new Request(
    this.api.localStorage.updateWalletLocalData
  );
  @observable unsetWalletLocalDataRequest: Request<any> = new Request(
    this.api.localStorage.unsetWalletLocalData
  );
  @observable
  getWalletsLocalDataRequest: Request<WalletsLocalData> = new Request(
    this.api.localStorage.getWalletsLocalData
  );

  @observable walletFieldBeingEdited = null;
  @observable lastUpdatedWalletField = null;
  @observable walletUtxos: ?WalletUtxos = null;
  @observable isForcedWalletResyncStarting = false;

  @observable
  recoveryPhraseVerificationData: RecoveryPhraseVerificationData = {};
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
    walletSettingsActions.forceWalletResync.listen(this._forceWalletResync);
    walletSettingsActions.updateRecoveryPhraseVerificationDate.listen(
      this._updateRecoveryPhraseVerificationDate
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
    const activeWallet = this.stores.wallets.active;
    if (!activeWallet || this.isForcedWalletResyncStarting) return;
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

  @action _forceWalletResync = async ({
    walletId,
    isLegacy,
  }: {
    walletId: string,
    isLegacy: boolean,
  }) => {
    const {
      _pausePolling,
      _resumePolling,
      refreshWalletsData,
    } = this.stores.wallets;
    _pausePolling();
    this.isForcedWalletResyncStarting = true;
    this.forceWalletResyncRequest.reset();
    try {
      await this.forceWalletResyncRequest.execute({ walletId, isLegacy });
    } finally {
      _resumePolling();
      await refreshWalletsData();
      runInAction('set isForcedWalletResyncStarting', () => {
        this.isForcedWalletResyncStarting = false;
        const activeWallet = this.stores.wallets.active;
        if (
          this.pollingApiInterval && // Is "true" if UTXO screen is active
          activeWallet &&
          activeWallet.id === walletId &&
          !isLegacy
        ) {
          this._getWalletUtxoApiData();
        }
      });
    }
  };

  /* ==========================================================
=            Wallet Recovery Phrase Verification            =
========================================================== */

  _createWalletLocalData = async (id: string) => {
    const walletLocalData = {
      id,
      creationDate: new Date(),
    };
    await this.stores.walletSettings.setWalletLocalDataRequest.execute(
      walletLocalData
    );
  };

  _getWalletsLocalData = async () => {
    const walletsLocalData: WalletsLocalData = await this.getWalletsLocalDataRequest.execute();
    return walletsLocalData;
  };

  /**
   * - Receives a walet local data
   * - Returns the wallet's recovery phrase verification status
   */
  _setWalletRecoveryPhraseVerificationData = ({
    recoveryPhraseVerificationDate,
    creationDate,
  }: WalletLocalData) => {
    const dateToCheck =
      recoveryPhraseVerificationDate || creationDate || new Date();
    const daysSinceDate = moment().diff(moment(dateToCheck), 'days');
    let recoveryPhraseVerificationStatus =
      RECOVERY_PHRASE_VERIFICATION_STATUSES.OK;
    if (daysSinceDate > RECOVERY_PHRASE_VERIFICATION_NOTIFICATION)
      recoveryPhraseVerificationStatus =
        RECOVERY_PHRASE_VERIFICATION_STATUSES.NOTIFICATION;
    else if (daysSinceDate > RECOVERY_PHRASE_VERIFICATION_WARNING)
      recoveryPhraseVerificationStatus =
        RECOVERY_PHRASE_VERIFICATION_STATUSES.WARNING;
    const recoveryPhraseVerificationStatusType = recoveryPhraseVerificationDate
      ? WALLET_RECOVERY_PHRASE_VERIFICATION_TYPES.ALREADY_CHECKED
      : WALLET_RECOVERY_PHRASE_VERIFICATION_TYPES.NEVER_CHECKED;
    return {
      creationDate,
      recoveryPhraseVerificationDate,
      recoveryPhraseVerificationStatus,
      recoveryPhraseVerificationStatusType,
    };
  };

  /**
   * - Receives a list of wallets Ids
   * - Retrieves the wallets local data from localStorage
   * - Populates `recoveryPhraseVerificationData` with
   *   the wallets recovery phrase verification data
   * - Updates localStorage in case of missing information
   */
  _setWalletsRecoveryPhraseVerificationData = async (
    walletIds: Array<string>
  ): RecoveryPhraseVerificationData => {
    const walletsLocalData = await this._getWalletsLocalData();
    const recoveryPhraseVerificationData = {};
    await asyncForEach(walletIds, async id => {
      let walletLocalData = walletsLocalData[id];
      // In case a wallet is not in the localStorage yet, it adds it
      if (!walletLocalData) {
        walletLocalData = {
          id,
          creationDate: new Date(),
        };
        await this.setWalletLocalDataRequest.execute(walletLocalData);
      }
      // In case a wallet doesn't have creationDate in the localStorage yet, it adds it
      if (!walletLocalData.creationDate) {
        walletLocalData.creationDate = new Date();
        await this._updateWalletLocalData({
          id,
          creationDate: walletLocalData.creationDate,
        });
      }
      recoveryPhraseVerificationData[
        id
      ] = this._setWalletRecoveryPhraseVerificationData(walletLocalData);
    });
    runInAction('refresh recovery phrase verification data', async () => {
      this.recoveryPhraseVerificationData = recoveryPhraseVerificationData;
    });
  };

  getWalletRecoveryPhraseVerification = (
    walletId: string
  ): WalletRecoveryPhraseVerificationData => {
    const {
      creationDate,
      recoveryPhraseVerificationDate,
      recoveryPhraseVerificationStatus,
      recoveryPhraseVerificationStatusType,
    } = this.recoveryPhraseVerificationData[walletId] || {};

    return {
      creationDate: creationDate || new Date(),
      recoveryPhraseVerificationDate,
      recoveryPhraseVerificationStatus:
        recoveryPhraseVerificationStatus ||
        RECOVERY_PHRASE_VERIFICATION_STATUSES.OK,
      recoveryPhraseVerificationStatusType:
        recoveryPhraseVerificationStatusType ||
        WALLET_RECOVERY_PHRASE_VERIFICATION_TYPES.NEVER_CHECKED,
    };
  };

  _updateWalletLocalData = async (updatedWalletData: Object): Object => {
    const { id } = updatedWalletData;
    const walletLocalData = await this.updateWalletLocalDataRequest.execute(
      updatedWalletData
    );
    runInAction('Update wallet verification date', () => {
      this.recoveryPhraseVerificationData[
        id
      ] = this._setWalletRecoveryPhraseVerificationData(walletLocalData);
    });
  };

  _updateRecoveryPhraseVerificationDate = async () => {
    const { active: activeWallet } = this.stores.wallets;
    if (!activeWallet) return;
    const { id } = activeWallet;
    const recoveryPhraseVerificationDate = new Date();
    await this._updateWalletLocalData({
      id,
      recoveryPhraseVerificationDate,
    });
  };

  _unsetWalletLocalData = async (walletId: string) => {
    await this.unsetWalletLocalDataRequest.execute(walletId);
  };

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
    const activeWallet = this.stores.wallets.active;
    if (!activeWallet)
      throw new Error(
        'Active wallet required before checking recovery phrase.'
      );
    const activeWalletId = getRawWalletId(activeWallet.id);
    const isCorrect = walletId === activeWalletId;
    const nextStep = isCorrect ? 3 : 4;
    if (isCorrect) {
      this._updateRecoveryPhraseVerificationDate();
    }
    runInAction('AdaWalletBackupStore::_checkRecoveryPhrase', () => {
      this.recoveryPhraseStep = nextStep;
    });
  };

  /* ====  End of Wallet Recovery Phrase Verification  ===== */
}
