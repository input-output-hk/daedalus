// @flow
import { observable, action } from 'mobx';
import Store from './lib/Store';

export default class WalletsStore extends Store {

  @observable inProgress = false;
  @observable walletId = '';
  @observable recoveryPhrase = [];
  @observable completed = false;
  @observable enteredPhrase = [];
  @observable isEntering = false;
  @observable isValid = false;
  @observable isWalletBackupStartAccepted = false;
  @observable countdownRemaining = 0;
  @observable countdownTimer = null;

  constructor(...args) {
    super(...args);
    this.actions.initiateWalletBackup.listen(this._initiateWalletBackup);
    this.actions.acceptWalletBackupStart.listen(this._acceptWalletBackupStart);
    this.actions.startWalletBackup.listen(this._startWalletBackup);
  }

  @action _initiateWalletBackup = (params) => {
    this.actions.toggleCreateWalletDialog();
    const { walletId, recoveryPhrase } = params;
    this.inProgress = true;
    this.walletId = walletId;
    this.recoveryPhrase = recoveryPhrase;
    this.completed = false;
    this.enteredPhrase = [];
    this.isEntering = false;
    this.isValid = false;
    this.isWalletBackupStartAccepted = false;
    this.countdownRemaining = 10;
    this.countdownTimer = null;
    this.countdownTimer = setInterval(() => {
      if (this.countdownRemaining > 0) {
        action(() => this.countdownRemaining--)();
      } else {
        clearInterval(this.countdownTimer);
      }
    }, 1000);
  };

  @action _acceptWalletBackupStart = () => {
    this.isWalletBackupStartAccepted = true;
  };

  @action _startWalletBackup = () => {
    this.isEntering = true;
  };

}
