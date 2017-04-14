// @flow
import { Action } from './lib/actions';

// ======= WALLET BACKUP ACTIONS =======

export default class WalletBackupActions {
  startWalletBackup: Action<any> = new Action();
  initiateWalletBackup: Action<{ recoveryPhrase: string[] }> = new Action();
  acceptPrivacyNoticeForWalletBackup: Action<any> = new Action();
  continueToRecoveryPhraseForWalletBackup: Action<any> = new Action();
  addWordToWalletBackupVerification: Action<{ word: string }> = new Action();
  clearEnteredRecoveryPhrase: Action<any> = new Action();
  acceptWalletBackupTermDevice: Action<any> = new Action();
  acceptWalletBackupTermRecovery: Action<any> = new Action();
  restartWalletBackup: Action<any> = new Action();
  cancelWalletBackup: Action<any> = new Action();
  finishWalletBackup: Action<any> = new Action();
};


  // initiateWalletBackup: Action<{ recoveryPhrase: string[] }>,
  // acceptPrivacyNoticeForWalletBackup: Action<any>,
  // continueToRecoveryPhraseForWalletBackup: Action<any>,
  // addWordToWalletBackupVerification: Action<{ word: string }>,
  // clearEnteredRecoveryPhrase: Action<any>,
  // acceptWalletBackupTermDevice: Action<any>,
  // acceptWalletBackupTermRecovery: Action<any>,
  // restartWalletBackup: Action<any>,
  // cancelWalletBackup: Action<any>,
  // finishWalletBackup: Action<any>

//   startWalletBackup: new Action(),
//   initiateWalletBackup: new Action(),
//   acceptPrivacyNoticeForWalletBackup: new Action(),
//   continueToRecoveryPhraseForWalletBackup: new Action(),
//   addWordToWalletBackupVerification: new Action(),
//   clearEnteredRecoveryPhrase: new Action(),
//   acceptWalletBackupTermDevice: new Action(),
//   acceptWalletBackupTermRecovery: new Action(),
//   restartWalletBackup: new Action(),
//   cancelWalletBackup: new Action(),
//   finishWalletBackup: new Action(),
// };
