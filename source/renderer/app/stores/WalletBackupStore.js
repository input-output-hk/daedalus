// @flow
import { observable, action, computed, runInAction } from 'mobx';
import { getRecoveryWalletIdChannel } from '../ipc/getRecoveryWalletIdChannel';
import Store from './lib/Store';
import WalletBackupDialog from '../components/wallet/WalletBackupDialog';
import { WALLET_BACKUP_STEPS } from '../types/walletBackupTypes';
import { getRawWalletId } from '../api/utils';

import type {
  RecoveryPhraseWord,
  walletBackupStep,
} from '../types/walletBackupTypes';

export default class WalletBackupStore extends Store {
  @observable inProgress = false;
  @observable currentStep: walletBackupStep = WALLET_BACKUP_STEPS.NOT_INITIATED;
  @observable recoveryPhrase = [];
  @observable recoveryPhraseWords: Array<RecoveryPhraseWord> = [];
  @observable recoveryPhraseShuffled: Array<RecoveryPhraseWord> = [];
  @observable completed = false;
  @observable enteredPhrase = [];
  @observable isPrivacyNoticeAccepted = false;
  @observable isEntering = false;
  @observable isTermOfflineAccepted = false;
  @observable isTermRecoveryAccepted = false;
  @observable isTermRewardsAccepted = false;
  @observable countdownRemaining = 0;
  @observable recoveryPhraseStep = 0;

  countdownTimerInterval: ?IntervalID = null;

  setup() {
    const a = this.actions.walletBackup;
    a.initiateWalletBackup.listen(this._initiateWalletBackup);
    a.acceptPrivacyNoticeForWalletBackup.listen(
      this._acceptPrivacyNoticeForWalletBackup
    );
    a.continueToRecoveryPhraseForWalletBackup.listen(
      this._continueToRecoveryPhraseForWalletBackup
    );
    a.startWalletBackup.listen(this._startWalletBackup);
    a.addWordToWalletBackupVerification.listen(
      this._addWordToWalletBackupVerification
    );
    a.clearEnteredRecoveryPhrase.listen(this._clearEnteredRecoveryPhrase);
    a.acceptWalletBackupTermOffline.listen(this._acceptWalletBackupTermOffline);
    a.acceptWalletBackupTermRecovery.listen(
      this._acceptWalletBackupTermRecovery
    );
    a.acceptWalletBackupTermRewards.listen(this._acceptWalletBackupTermRewards);
    a.restartWalletBackup.listen(this._restartWalletBackup);
    a.cancelWalletBackup.listen(this._cancelWalletBackup);
    a.finishWalletBackup.listen(this._finishWalletBackup);
    this.actions.app.initAppEnvironment.listen(() => {});
    a.recoveryPhraseContinue.listen(this._recoveryPhraseContinue);
    a.recoveryPhraseCheck.listen(this._recoveryPhraseCheck);
    a.recoveryPhraseClose.listen(this._recoveryPhraseClose);
  }

  @action _initiateWalletBackup = (params: {
    recoveryPhrase: Array<string>,
  }) => {
    this.recoveryPhrase = params.recoveryPhrase;
    this.inProgress = true;
    this.currentStep = WALLET_BACKUP_STEPS.PRIVACY_WARNING;
    this.recoveryPhraseWords = this.recoveryPhrase.map((word: string) => ({
      word,
    }));
    this.recoveryPhraseShuffled = this.recoveryPhrase
      .sort(() => 0.5 - Math.random())
      .map(w => ({ word: w, isActive: true }));
    this.completed = false;
    this.enteredPhrase = [];
    this.isPrivacyNoticeAccepted = false;
    this.isEntering = false;
    this.isTermOfflineAccepted = false;
    this.isTermRecoveryAccepted = false;
    this.isTermRewardsAccepted = false;
    this.countdownRemaining = this.environment.isTest ? 0 : 10;
    if (this.countdownTimerInterval) clearInterval(this.countdownTimerInterval);
    this.countdownTimerInterval = setInterval(() => {
      if (this.countdownRemaining > 0) {
        action(() => this.countdownRemaining--)();
      } else if (this.countdownTimerInterval != null) {
        clearInterval(this.countdownTimerInterval);
      }
    }, 1000);
    this.actions.dialogs.open.trigger({
      dialog: WalletBackupDialog,
    });
  };

  @action _acceptPrivacyNoticeForWalletBackup = () => {
    this.isPrivacyNoticeAccepted = true;
  };

  @action _continueToRecoveryPhraseForWalletBackup = () => {
    this.currentStep = WALLET_BACKUP_STEPS.RECOVERY_PHRASE_DISPLAY;
  };

  @action _startWalletBackup = () => {
    this.currentStep = WALLET_BACKUP_STEPS.RECOVERY_PHRASE_ENTRY;
  };

  @action _addWordToWalletBackupVerification = (params: {
    word: string,
    index: number,
  }) => {
    const { word, index } = params;
    this.enteredPhrase.push({ word });
    const pickedWord = this.recoveryPhraseShuffled[index];
    if (pickedWord && pickedWord.word === word) pickedWord.isActive = false;
  };

  @action _clearEnteredRecoveryPhrase = () => {
    this.enteredPhrase = [];
    this.recoveryPhraseShuffled = this.recoveryPhraseShuffled.map(
      ({ word }) => ({ word, isActive: true })
    );
  };

  @action _recoveryPhraseContinue = async () => {
    const step = this.recoveryPhraseStep;
    if (step === 4) this.recoveryPhraseStep = 1;
    else this.recoveryPhraseStep = step + 1;
  };

  @action _recoveryPhraseClose = async () => {
    this.recoveryPhraseStep = 0;
  };

  @action _recoveryPhraseCheck = async ({
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
    const nextStep = walletId === activeWalletId ? 3 : 4;
    runInAction('AdaWalletBackupStore::_checkRecoveryPhrase', () => {
      this.recoveryPhraseStep = nextStep;
    });
  };

  @computed get isRecoveryPhraseValid(): boolean {
    return (
      this.recoveryPhraseWords.reduce((words, { word }) => words + word, '') ===
      this.enteredPhrase.reduce((words, { word }) => words + word, '')
    );
  }

  @action _acceptWalletBackupTermOffline = () => {
    this.isTermOfflineAccepted = true;
  };

  @action _acceptWalletBackupTermRecovery = () => {
    this.isTermRecoveryAccepted = true;
  };

  @action _acceptWalletBackupTermRewards = () => {
    this.isTermRewardsAccepted = true;
  };

  @action _restartWalletBackup = () => {
    this._clearEnteredRecoveryPhrase();
    this.currentStep = WALLET_BACKUP_STEPS.RECOVERY_PHRASE_DISPLAY;
  };

  @action _cancelWalletBackup = () => {
    this.inProgress = false;
    this._clearEnteredRecoveryPhrase();
  };

  @action _finishWalletBackup = () => {
    this.inProgress = false;
  };
}
