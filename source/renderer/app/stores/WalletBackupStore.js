// @flow
import { observable, action, computed } from 'mobx';
import { ipcRenderer } from 'electron';
import Store from './lib/Store';
import { GET_APP_ENVIRONMENT } from '../../../common/ipc-api';
import WalletBackupDialog from '../components/wallet/WalletBackupDialog';
import type { walletBackupStep } from '../types/walletBackupTypes';
import { WALLET_BACKUP_STEPS } from '../types/walletBackupTypes';

export default class WalletBackupStore extends Store {
  _network: ?string = null;

  @observable inProgress = false;
  @observable currentStep: walletBackupStep = WALLET_BACKUP_STEPS.NOT_INITIATED;
  @observable recoveryPhrase = [];
  @observable recoveryPhraseWords = [];
  @observable recoveryPhraseShuffled = [];
  @observable completed = false;
  @observable enteredPhrase = [];
  @observable isPrivacyNoticeAccepted = false;
  @observable isEntering = false;
  @observable isTermDeviceAccepted = false;
  @observable isTermRecoveryAccepted = false;
  @observable countdownRemaining = 0;

  countdownTimerInterval: ?number = null;

  setup() {
    const a = this.actions.walletBackup;
    a.initiateWalletBackup.listen(this._initiateWalletBackup);
    a.acceptPrivacyNoticeForWalletBackup.listen(this._acceptPrivacyNoticeForWalletBackup);
    a.continueToRecoveryPhraseForWalletBackup.listen(this._continueToRecoveryPhraseForWalletBackup);
    a.startWalletBackup.listen(this._startWalletBackup);
    a.addWordToWalletBackupVerification.listen(this._addWordToWalletBackupVerification);
    a.clearEnteredRecoveryPhrase.listen(this._clearEnteredRecoveryPhrase);
    a.acceptWalletBackupTermDevice.listen(this._acceptWalletBackupTermDevice);
    a.acceptWalletBackupTermRecovery.listen(this._acceptWalletBackupTermRecovery);
    a.restartWalletBackup.listen(this._restartWalletBackup);
    a.cancelWalletBackup.listen(this._cancelWalletBackup);
    a.finishWalletBackup.listen(this._finishWalletBackup);
    this.actions.app.initAppEnvironment.listen(() => {});
    ipcRenderer.on(GET_APP_ENVIRONMENT.SUCCESS, this._onGetAppEnvironmentSuccess);
  }

  @computed get isTestnet(): boolean {
    return (this._network === 'testnet');
  }

  _onGetAppEnvironmentSuccess = action((event, environment) => {
    this._network = environment.NETWORK;
  });

  @action _initiateWalletBackup = (params: { recoveryPhrase: Array<string> }) => {
    this.recoveryPhrase = params.recoveryPhrase;
    this.inProgress = true;
    this.currentStep = WALLET_BACKUP_STEPS.PRIVACY_WARNING;
    this.recoveryPhraseWords = this.recoveryPhrase.map(word => ({ word }));
    this.recoveryPhraseShuffled = this.recoveryPhrase
      .sort(() => 0.5 - Math.random())
      .map(w => ({ word: w, isActive: true }));
    this.completed = false;
    this.enteredPhrase = [];
    this.isPrivacyNoticeAccepted = false;
    this.isEntering = false;
    this.isTermDeviceAccepted = false;
    this.isTermRecoveryAccepted = false;
    this.countdownRemaining = this.isTestnet ? 0 : 10;
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

  @action _addWordToWalletBackupVerification = (params: { word: string, index: number }) => {
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

  @computed get isRecoveryPhraseValid(): boolean {
    return (
      this.recoveryPhraseWords.reduce((words, { word }) => words + word, '') ===
      this.enteredPhrase.reduce((words, { word }) => words + word, '')
    );
  }

  @action _acceptWalletBackupTermDevice = () => {
    this.isTermDeviceAccepted = true;
  };

  @action _acceptWalletBackupTermRecovery = () => {
    this.isTermRecoveryAccepted = true;
  };

  @action _restartWalletBackup = () => {
    this._clearEnteredRecoveryPhrase();
    this.currentStep = WALLET_BACKUP_STEPS.RECOVERY_PHRASE_DISPLAY;
  };

  @action _cancelWalletBackup = () => {
    this.inProgress = false;
    this._clearEnteredRecoveryPhrase();
  };

  @action _finishWalletBackup = async () => {
    this.inProgress = false;
  }

}
