// @flow
import Action from './lib/Action';
import type { walletExportTypeChoices } from '../types/walletExportTypes';
import type { CsvRecord } from '../../../common/types/rewards-csv-request.types';

export type WalletImportFromFileParams = {
  filePath: string,
  walletName: ?string,
  spendingPassword: string,
};

// ======= WALLET ACTIONS =======

export default class WalletsActions {
  /* ----------  Create Wallet  ---------- */
  createWallet: Action<{
    name: string,
    spendingPassword: string,
  }> = new Action();
  createWalletBegin: Action<any> = new Action();
  createWalletChangeStep: Action<any> = new Action();
  createWalletClose: Action<any> = new Action();
  createWalletAbort: Action<any> = new Action();

  /* ----------  Restore Wallet  ---------- */
  restoreWallet: Action<{
    name: string,
    spendingPassword: string,
  }> = new Action();
  restoreWalletBegin: Action<any> = new Action();
  restoreWalletChangeStep: Action<any> = new Action();
  restoreWalletClose: Action<any> = new Action();
  restoreWalletAbort: Action<any> = new Action();
  restoreWalletSetKind: Action<{ param?: string, kind: string }> = new Action();
  restoreWalletSetMnemonics: Action<{
    mnemonics: Array<string>,
  }> = new Action();
  restoreWalletSetName: Action<any> = new Action();
  restoreWalletSetPassphrase: Action<any> = new Action();

  restoreWallet: Action<{
    recoveryPhrase: string,
    walletName: string,
    spendingPassword: string,
    type?: string,
  }> = new Action();
  importWalletFromFile: Action<WalletImportFromFileParams> = new Action();
  deleteWallet: Action<{ walletId: string, isLegacy?: boolean }> = new Action();
  undelegateWallet: Action<{
    walletId: string,
    stakePoolId: string,
    passphrase: string,
  }> = new Action();
  setUndelegateWalletSubmissionSuccess: Action<{
    result: boolean,
  }> = new Action();
  sendMoney: Action<{
    receiver: string,
    amount: string,
    passphrase: string,
  }> = new Action();
  chooseWalletExportType: Action<{
    walletExportType: walletExportTypeChoices,
  }> = new Action();
  generateCertificate: Action<{ filePath: string }> = new Action();
  generateRewardsCsv: Action<{
    filePath: string,
    rewards: Array<CsvRecord>,
  }> = new Action();
  generateAddressPDF: Action<{
    address: string,
    note: string,
    filePath: string,
  }> = new Action();
  updateCertificateStep: Action<any> = new Action();
  closeCertificateGeneration: Action<any> = new Action();
  closeRewardsCsvGeneration: Action<any> = new Action();
  setCertificateTemplate: Action<{ selectedTemplate: string }> = new Action();
  finishCertificate: Action<any> = new Action();
  finishRewardsCsv: Action<any> = new Action();
  updateWalletLocalData: Action<any> = new Action();
  updateRecoveryPhraseVerificationDate: Action<any> = new Action();

  /* ----------  Transfer Funds  ---------- */
  transferFundsNextStep: Action<any> = new Action();
  transferFundsPrevStep: Action<any> = new Action();
  transferFundsSetSourceWalletId: Action<{
    sourceWalletId: string,
  }> = new Action();
  transferFundsSetTargetWalletId: Action<{
    targetWalletId: string,
  }> = new Action();
  transferFundsRedeem: Action<any> = new Action();
  transferFundsClose: Action<any> = new Action();
  transferFundsCalculateFee: Action<{ sourceWalletId: string }> = new Action();
  transferFunds: Action<{ spendingPassword: string }> = new Action();
}
