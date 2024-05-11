import {
  observable,
  action,
  runInAction,
  computed,
  makeObservable,
} from 'mobx';
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
import { RECOVERY_PHRASE_VERIFICATION_STATUSES } from '../config/walletRecoveryPhraseVerificationConfig';
import { AnalyticsTracker, EventCategories } from '../analytics';
import { WalletLocalData } from '../types/localDataTypes';
import { Api } from '../api';
import { ActionsMap } from '../actions';

export default class WalletSettingsStore extends Store {
  updateWalletRequest: Request<Wallet> = new Request(this.api.ada.updateWallet);
  updateSpendingPasswordRequest: Request<boolean> = new Request(
    this.api.ada.updateSpendingPassword
  );
  exportWalletToFileRequest: Request<Promise<[]>> = new Request(
    this.api.ada.exportWalletToFile
  );
  getWalletUtxosRequest: Request<WalletUtxos> = new Request(
    this.api.ada.getWalletUtxos
  );
  walletFieldBeingEdited = null;
  lastUpdatedWalletField = null;
  walletUtxos: WalletUtxos | null | undefined = null;
  recoveryPhraseStep = 0;
  // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'IntervalID'.
  pollingApiInterval: IntervalID | null | undefined = null;

  constructor(
    protected api: Api,
    protected actions: ActionsMap,
    protected analytics: AnalyticsTracker
  ) {
    super(api, actions, analytics);

    makeObservable(this, {
      updateWalletRequest: observable,
      updateSpendingPasswordRequest: observable,
      exportWalletToFileRequest: observable,
      getWalletUtxosRequest: observable,
      walletFieldBeingEdited: observable,
      lastUpdatedWalletField: observable,
      walletUtxos: observable,
      recoveryPhraseStep: observable,
      walletsRecoveryPhraseVerificationData: computed,
      _startEditingWalletField: action,
      _stopEditingWalletField: action,
      _cancelEditingWalletField: action,
      _updateSpendingPassword: action,
      _updateWalletField: action,
      _exportToFile: action,
      _startWalletUtxoPolling: action,
      _stopWalletUtxoPolling: action,
      _clearWalletUtxoPollingInterval: action,
      _getWalletUtxoApiData: action,
      _updateWalletUtxos: action,
      _onWalletSelected: action,
      _recoveryPhraseVerificationContinue: action,
      _recoveryPhraseVerificationClose: action,
      _recoveryPhraseVerificationCheck: action,
      _toggleShowUsedAddressesStatuses: action,
    });
  }

  setup() {
    const { walletSettings: walletSettingsActions, sidebar: sidebarActions } =
      this.actions;
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
  getLocalWalletDataById = (id: any): WalletLocalData | null | undefined => {
    const { all: walletsLocalData } = this.stores.walletsLocal;
    return walletsLocalData[id];
  };

  get walletsRecoveryPhraseVerificationData() {
    const { all: walletsLocalData } = this.stores.walletsLocal;
    return Object.keys(walletsLocalData)
      .map((key) => walletsLocalData[key])
      .reduce((obj, { id, recoveryPhraseVerificationDate, creationDate }) => {
        const {
          recoveryPhraseVerificationStatus,
          recoveryPhraseVerificationStatusType,
        } = getStatusFromWalletData({
          creationDate,
          recoveryPhraseVerificationDate,
        });
        const hasNotification =
          recoveryPhraseVerificationStatus ===
          RECOVERY_PHRASE_VERIFICATION_STATUSES.NOTIFICATION;
        obj[id] = {
          id,
          recoveryPhraseVerificationDate,
          creationDate,
          recoveryPhraseVerificationStatus,
          recoveryPhraseVerificationStatusType,
          hasNotification,
        };
        return obj;
      }, {});
  }

  // =================== PRIVATE API ==================== //
  _startEditingWalletField = ({ field }: { field: string }) => {
    this.walletFieldBeingEdited = field;
  };
  _stopEditingWalletField = () => {
    if (this.walletFieldBeingEdited) {
      this.lastUpdatedWalletField = this.walletFieldBeingEdited;
    }

    this.walletFieldBeingEdited = null;
  };
  _cancelEditingWalletField = () => {
    this.lastUpdatedWalletField = null;
    this.walletFieldBeingEdited = null;
  };
  _updateSpendingPassword = async ({
    walletId,
    oldPassword,
    newPassword,
    isLegacy,
  }: {
    walletId: string;
    oldPassword: string;
    newPassword: string;
    isLegacy: boolean;
  }) => {
    // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
    await this.updateSpendingPasswordRequest.execute({
      walletId,
      oldPassword,
      newPassword,
      isLegacy,
    });
    // @ts-ignore ts-migrate(2554) FIXME: Expected 1 arguments, but got 0.
    this.actions.dialogs.closeActiveDialog.trigger();
    this.updateSpendingPasswordRequest.reset();
    this.stores.wallets.refreshWalletsData();
    this.analytics.sendEvent(
      EventCategories.WALLETS,
      'Changed wallet settings',
      'password'
    );
  };
  _updateWalletField = async ({
    field,
    value,
  }: {
    field: string;
    value: string;
  }) => {
    const activeWallet = this.stores.wallets.active;
    if (!activeWallet) return;
    const { id: walletId, name, isLegacy } = activeWallet;
    const walletData = {
      walletId,
      name,
      isLegacy,
    };
    walletData[field] = value;
    const wallet = await this.updateWalletRequest.execute({
      walletId: walletData.walletId,
      name: walletData.name,
      isLegacy: walletData.isLegacy,
    }).promise;
    if (!wallet) return;
    // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
    await this.stores.wallets.walletsRequest.patch((result) => {
      const walletIndex = findIndex(result, {
        id: walletId,
      });
      result[walletIndex] = wallet;
    });
    this.updateWalletRequest.reset();
    this.stores.wallets.refreshWalletsData();
    this.analytics.sendEvent(
      EventCategories.WALLETS,
      'Changed wallet settings',
      field
    );
  };
  _exportToFile = async (params: WalletExportToFileParams) => {
    const { walletId, filePath, password } = params;
    // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
    await this.exportWalletToFileRequest.execute({
      walletId,
      filePath,
      password,
    });
    // @ts-ignore ts-migrate(2554) FIXME: Expected 1 arguments, but got 0.
    this.actions.dialogs.closeActiveDialog.trigger();
  };
  _startWalletUtxoPolling = () => {
    this._clearWalletUtxoPollingInterval();

    this._getWalletUtxoApiData();

    this.pollingApiInterval = setInterval(
      this._getWalletUtxoApiData,
      WALLET_UTXO_API_REQUEST_INTERVAL
    );
  };
  _stopWalletUtxoPolling = () => {
    this._clearWalletUtxoPollingInterval();

    this.getWalletUtxosRequest.reset();
  };
  _clearWalletUtxoPollingInterval = () => {
    if (this.pollingApiInterval) {
      clearInterval(this.pollingApiInterval);
      this.pollingApiInterval = null;
    }
  };
  _getWalletUtxoApiData = async () => {
    const activeWallet = this.stores.wallets.active;
    if (!activeWallet) return;
    const { id: walletId, isLegacy } = activeWallet;
    // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
    const walletUtxos = await this.getWalletUtxosRequest.execute({
      walletId,
      isLegacy,
    });

    this._updateWalletUtxos(walletUtxos);
  };
  _updateWalletUtxos = (walletUtxos: WalletUtxos | null | undefined) => {
    this.walletUtxos = walletUtxos;
  };
  _onWalletSelected = () => {
    this._updateWalletUtxos(null);
  };

  /* ==========================================================
  =            Wallet Recovery Phrase Verification            =
  ========================================================== */
  _recoveryPhraseVerificationContinue = async () => {
    const step = this.recoveryPhraseStep;
    if (step === 4) this.recoveryPhraseStep = 2;
    else this.recoveryPhraseStep = step + 1;
  };
  _recoveryPhraseVerificationClose = async () => {
    this.recoveryPhraseStep = 0;
  };
  _recoveryPhraseVerificationCheck = async ({
    recoveryPhrase,
  }: {
    recoveryPhrase: Array<string>;
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

    this.analytics.sendEvent(
      EventCategories.WALLETS,
      'Verified recovery phrase'
    );

    if (isCorrect) {
      const recoveryPhraseVerificationDate = new Date();
      await this.actions.walletsLocal.setWalletLocalData.trigger({
        walletId: activeWallet.id,
        updatedWalletData: {
          recoveryPhraseVerificationDate,
        },
      });
    }

    runInAction(() => {
      this.recoveryPhraseStep = nextStep;
    });
  };

  /* ====  End of Wallet Recovery Phrase Verification  ===== */
  _toggleShowUsedAddressesStatuses = async () => {
    const activeWallet = this.stores.wallets.active;
    if (!activeWallet)
      throw new Error(
        'Active wallet required before checking show used addresses statuses.'
      );
    const localWalletData: WalletLocalData | null | undefined =
      this.getLocalWalletDataById(activeWallet ? activeWallet.id : '');
    const { showUsedAddresses } = localWalletData || {};
    await this.actions.walletsLocal.setWalletLocalData.trigger({
      walletId: activeWallet.id,
      updatedWalletData: {
        showUsedAddresses: !showUsedAddresses,
      },
    });
  };
}
