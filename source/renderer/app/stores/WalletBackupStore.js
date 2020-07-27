// @flow
import { observable, action, computed } from 'mobx';
import Store from './lib/Store';
import WalletBackupDialog from '../components/wallet/WalletBackupDialog';
import { WALLET_BACKUP_STEPS } from '../types/walletBackupTypes';
import type { walletBackupStep } from '../types/walletBackupTypes';

export default class WalletBackupStore extends Store {
  @observable inProgress = false;
  @observable currentStep: walletBackupStep = WALLET_BACKUP_STEPS.NOT_INITIATED;
  @observable recoveryPhrase = [];
  @observable completed = false;
  @observable enteredPhrase = [];
  @observable isPrivacyNoticeAccepted = false;
  @observable isEntering = false;
  @observable isTermOfflineAccepted = false;
  @observable isTermRecoveryAccepted = false;
  @observable isTermRewardsAccepted = false;
  @observable countdownRemaining = 0;

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
    a.updateWalletBackupVerificationPhrase.listen(
      this._updateWalletBackupVerificationPhrase
    );
    a.acceptWalletBackupTermOffline.listen(this._acceptWalletBackupTermOffline);
    a.acceptWalletBackupTermRecovery.listen(
      this._acceptWalletBackupTermRecovery
    );
    a.acceptWalletBackupTermRewards.listen(this._acceptWalletBackupTermRewards);
    a.restartWalletBackup.listen(this._restartWalletBackup);
    a.cancelWalletBackup.listen(this._cancelWalletBackup);
    a.finishWalletBackup.listen(this._finishWalletBackup);
    this.actions.app.initAppEnvironment.listen(() => {});
  }

  @action _initiateWalletBackup = (params: {
    recoveryPhrase: Array<string>,
  }) => {
    this.recoveryPhrase = params.recoveryPhrase;
    this.inProgress = true;
    this.currentStep = WALLET_BACKUP_STEPS.PRIVACY_WARNING;
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

  @action _updateWalletBackupVerificationPhrase = (params: {
    verificationPhrase: Array<string>,
  }) => {
    const { verificationPhrase } = params;
    this.enteredPhrase = verificationPhrase;
  };

  @action _clearEnteredRecoveryPhrase = () => {
    this.enteredPhrase = [];
  };

  @computed get isRecoveryPhraseValid(): boolean {
    return this.recoveryPhrase.join(' ') === this.enteredPhrase.join(' ');
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
