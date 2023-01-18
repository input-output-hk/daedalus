import Action from './lib/Action'; // ======= WALLET BACKUP ACTIONS =======

export default class WalletBackupActions {
  startWalletBackup: Action<any> = new Action();
  initiateWalletBackup: Action<{
    recoveryPhrase: Array<string>;
  }> = new Action();
  acceptPrivacyNoticeForWalletBackup: Action<any> = new Action();
  continueToRecoveryPhraseForWalletBackup: Action<any> = new Action();
  updateWalletBackupVerificationPhrase: Action<{
    verificationPhrase: Array<string>;
  }> = new Action();
  acceptWalletBackupTermOffline: Action<any> = new Action();
  acceptWalletBackupTermRecovery: Action<any> = new Action();
  restartWalletBackup: Action<any> = new Action();
  cancelWalletBackup: Action<any> = new Action();
  finishWalletBackup: Action<any> = new Action();
}
