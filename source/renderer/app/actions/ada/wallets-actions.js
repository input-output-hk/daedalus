// @flow
import Action from '../lib/Action';
import type { walletExportTypeChoices } from '../../types/walletExportTypes';

export type WalletImportFromFileParams = {
  filePath: string,
  walletName: ?string,
  walletPassword: ?string,
};

// ======= WALLET ACTIONS =======

export default class WalletsActions {
  createWallet: Action<{ name: string, password: ?string }> = new Action();
  // eslint-disable-next-line max-len
  restoreWallet: Action<{recoveryPhrase: string, walletName: string, walletPassword: ?string, type?: string }> = new Action();
  importWalletFromFile: Action<WalletImportFromFileParams> = new Action();
  deleteWallet: Action<{ walletId: string }> = new Action();
  sendMoney: Action<{ receiver: string, amount: string, password: ?string }> = new Action();
  chooseWalletExportType: Action<{ walletExportType: walletExportTypeChoices }> = new Action();
  generateCertificate: Action<{ filePath: string }> = new Action();
  updateCertificateStep: Action<any> = new Action();
  closeCertificateGeneration: Action<any> = new Action();
  setCertificateTemplate: Action<{ selectedTemplate: string }> = new Action();
  finishCertificate: Action<any> = new Action();
  discardAntivirusRestorationSlowdownNotificationForActiveWallet: Action<any> = new Action();
}
