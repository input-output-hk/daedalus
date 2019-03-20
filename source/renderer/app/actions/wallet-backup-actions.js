// @flow
import Action from './lib/Action';

// ======= WALLET BACKUP ACTIONS =======

export default class WalletBackupActions {
  startWalletBackup: Action<any> = new Action();
  initiateWalletBackup: Action<{ recoveryPhrase: string[] }> = new Action();
  acceptPrivacyNoticeForWalletBackup: Action<any> = new Action();
  continueToRecoveryPhraseForWalletBackup: Action<any> = new Action();
  addWordToWalletBackupVerification: Action<{
    word: string,
    index: number,
  }> = new Action();
  clearEnteredRecoveryPhrase: Action<any> = new Action();
  acceptWalletBackupTermDevice: Action<any> = new Action();
  acceptWalletBackupTermRecovery: Action<any> = new Action();
  restartWalletBackup: Action<any> = new Action();
  cancelWalletBackup: Action<any> = new Action();
  finishWalletBackup: Action<any> = new Action();
}
