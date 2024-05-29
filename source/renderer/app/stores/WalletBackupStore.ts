import { observable, action, computed, makeObservable } from 'mobx';
import Store from './lib/Store';
import WalletBackupDialog from '../components/wallet/WalletBackupDialog';
import { WALLET_BACKUP_STEPS } from '../types/walletBackupTypes';
import type { walletBackupStep } from '../types/walletBackupTypes';
import { Api } from '../api';
import { ActionsMap } from '../actions';
import { AnalyticsTracker } from '../analytics';

export default class WalletBackupStore extends Store {
  inProgress = false;
  currentStep: walletBackupStep = WALLET_BACKUP_STEPS.NOT_INITIATED;
  recoveryPhrase = [];
  completed = false;
  enteredPhrase = [];
  isPrivacyNoticeAccepted = false;
  isEntering = false;
  isTermOfflineAccepted = false;
  isTermRecoveryAccepted = false;
  countdownRemaining = 0;
  // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'IntervalID'.
  countdownTimerInterval: IntervalID | null | undefined = null;

  constructor(
    protected api: Api,
    protected actions: ActionsMap,
    protected analytics: AnalyticsTracker
  ) {
    super(api, actions, analytics);

    makeObservable(this, {
      inProgress: observable,
      currentStep: observable,
      recoveryPhrase: observable,
      completed: observable,
      enteredPhrase: observable,
      isPrivacyNoticeAccepted: observable,
      isEntering: observable,
      isTermOfflineAccepted: observable,
      isTermRecoveryAccepted: observable,
      countdownRemaining: observable,
      _initiateWalletBackup: action,
      _acceptPrivacyNoticeForWalletBackup: action,
      _continueToRecoveryPhraseForWalletBackup: action,
      _startWalletBackup: action,
      _updateWalletBackupVerificationPhrase: action,
      _clearEnteredRecoveryPhrase: action,
      isRecoveryPhraseValid: computed,
      _acceptWalletBackupTermOffline: action,
      _acceptWalletBackupTermRecovery: action,
      _restartWalletBackup: action,
      _cancelWalletBackup: action,
      _finishWalletBackup: action,
    });
  }

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
    a.restartWalletBackup.listen(this._restartWalletBackup);
    a.cancelWalletBackup.listen(this._cancelWalletBackup);
    a.finishWalletBackup.listen(this._finishWalletBackup);
    this.actions.app.initAppEnvironment.listen(() => {});
  }

  _initiateWalletBackup = (params: { recoveryPhrase: Array<string> }) => {
    this.recoveryPhrase = params.recoveryPhrase;
    this.inProgress = true;
    // @ts-ignore ts-migrate(2322) FIXME: Type 'string' is not assignable to type 'walletBac... Remove this comment to see the full error message
    this.currentStep = WALLET_BACKUP_STEPS.PRIVACY_WARNING;
    this.completed = false;
    this.enteredPhrase = [];
    this.isPrivacyNoticeAccepted = false;
    this.isEntering = false;
    this.isTermOfflineAccepted = false;
    this.isTermRecoveryAccepted = false;
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
      // @ts-ignore ts-migrate(2322) FIXME: Type 'typeof WalletBackupDialog' is not assignable... Remove this comment to see the full error message
      dialog: WalletBackupDialog,
    });
  };
  _acceptPrivacyNoticeForWalletBackup = () => {
    this.isPrivacyNoticeAccepted = true;
  };
  _continueToRecoveryPhraseForWalletBackup = () => {
    // @ts-ignore ts-migrate(2322) FIXME: Type 'string' is not assignable to type 'walletBac... Remove this comment to see the full error message
    this.currentStep = WALLET_BACKUP_STEPS.RECOVERY_PHRASE_DISPLAY;
  };
  _startWalletBackup = () => {
    // @ts-ignore ts-migrate(2322) FIXME: Type 'string' is not assignable to type 'walletBac... Remove this comment to see the full error message
    this.currentStep = WALLET_BACKUP_STEPS.RECOVERY_PHRASE_ENTRY;
  };
  _updateWalletBackupVerificationPhrase = (params: {
    verificationPhrase: Array<string>;
  }) => {
    const { verificationPhrase } = params;
    this.enteredPhrase = verificationPhrase;
  };
  _clearEnteredRecoveryPhrase = () => {
    this.enteredPhrase = [];
  };

  get isRecoveryPhraseValid(): boolean {
    return this.recoveryPhrase.join(' ') === this.enteredPhrase.join(' ');
  }

  _acceptWalletBackupTermOffline = () => {
    this.isTermOfflineAccepted = true;
  };
  _acceptWalletBackupTermRecovery = () => {
    this.isTermRecoveryAccepted = true;
  };
  _restartWalletBackup = () => {
    this._clearEnteredRecoveryPhrase();

    // @ts-ignore ts-migrate(2322) FIXME: Type 'string' is not assignable to type 'walletBac... Remove this comment to see the full error message
    this.currentStep = WALLET_BACKUP_STEPS.RECOVERY_PHRASE_DISPLAY;
  };
  _cancelWalletBackup = () => {
    this.inProgress = false;

    this._clearEnteredRecoveryPhrase();
  };
  _finishWalletBackup = () => {
    this.inProgress = false;
  };
}
