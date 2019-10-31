// @flow
import Action from './lib/Action';
import type { walletExportTypeChoices } from '../types/walletExportTypes';

export type WalletImportFromFileParams = {
  filePath: string,
  walletName: ?string,
  spendingPassword: ?string,
};

// ======= WALLET ACTIONS =======

export default class WalletsActions {
  // Create Wallet
  createWallet: Action<{
    name: string,
    spendingPassword: ?string,
  }> = new Action();
  createWalletBegin: Action<any> = new Action();
  createWalletChangeStep: Action<any> = new Action();
  createWalletClose: Action<any> = new Action();
  createWalletAbort: Action<any> = new Action();
  // ---
  restoreWallet: Action<{
    recoveryPhrase: string,
    walletName: string,
    spendingPassword: ?string,
    type?: string,
  }> = new Action();
  importWalletFromFile: Action<WalletImportFromFileParams> = new Action();
  deleteWallet: Action<{ walletId: string }> = new Action();
  sendMoney: Action<{
    receiver: string,
    amount: string,
    password: ?string,
  }> = new Action();
  chooseWalletExportType: Action<{
    walletExportType: walletExportTypeChoices,
  }> = new Action();
  generateCertificate: Action<{ filePath: string }> = new Action();
  updateCertificateStep: Action<any> = new Action();
  closeCertificateGeneration: Action<any> = new Action();
  setCertificateTemplate: Action<{ selectedTemplate: string }> = new Action();
  finishCertificate: Action<any> = new Action();
  updateWalletLocalData: Action<any> = new Action();
  updateRecoveryPhraseVerificationDate: Action<any> = new Action();
}
