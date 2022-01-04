import Action from './lib/Action';
import type { WalletExportTypeChoices } from '../types/walletExportTypes';
import type {
  TransportDevice,
  HardwareWalletExtendedPublicKeyResponse,
} from '../../../common/types/hardware-wallets.types';
import type { CsvFileContent } from '../../../common/types/csv-request.types';
import type { QuitStakePoolRequest } from '../api/staking/types';
import type { AssetToken } from '../api/assets/types';

export type WalletImportFromFileParams = {
  filePath: string;
  walletName: string | null | undefined;
  spendingPassword: string;
}; // ======= WALLET ACTIONS =======

export default class WalletsActions {
  refreshWalletsDataSuccess: Action<any> = new Action();

  /* ----------  Create Wallet  ---------- */
  createWallet: Action<{
    name: string;
    spendingPassword: string;
  }> = new Action();
  createWalletBegin: Action<any> = new Action();
  createWalletChangeStep: Action<any> = new Action();
  createWalletClose: Action<any> = new Action();
  createWalletAbort: Action<any> = new Action();

  /* ----------  Restore Wallet  ---------- */
  restoreWalletBegin: Action<any> = new Action();
  restoreWalletEnd: Action<any> = new Action();
  restoreWalletChangeStep: Action<any> = new Action();
  restoreWalletClose: Action<any> = new Action();
  restoreWalletCancelClose: Action<any> = new Action();
  restoreWalletSetKind: Action<{
    param?: string;
    kind: string;
  }> = new Action();
  restoreWalletSetMnemonics: Action<{
    mnemonics: Array<string>;
  }> = new Action();
  restoreWalletSetConfig: Action<{
    param: string;
    value: string;
  }> = new Action();
  restoreWallet: Action<any> = new Action();
  importWalletFromFile: Action<WalletImportFromFileParams> = new Action();
  deleteWallet: Action<{
    walletId: string;
    isLegacy: boolean;
  }> = new Action();
  undelegateWallet: Action<QuitStakePoolRequest> = new Action();
  setUndelegateWalletSubmissionSuccess: Action<{
    result: boolean;
  }> = new Action();
  sendMoney: Action<{
    receiver: string;
    amount: string;
    passphrase: string;
    assets?: Array<AssetToken>;
    assetsAmounts?: Array<string>;
  }> = new Action();
  chooseWalletExportType: Action<{
    walletExportType: WalletExportTypeChoices;
  }> = new Action();
  generateCertificate: Action<{
    filePath: string;
  }> = new Action();
  generateCsv: Action<{
    filePath: string;
    fileContent: CsvFileContent;
  }> = new Action();
  generateAddressPDF: Action<{
    note: string;
    address: string;
    filePath: string;
  }> = new Action();
  generateAddressPDFSuccess: Action<{
    walletAddress: string;
  }> = new Action();
  saveQRCodeImage: Action<{
    address: string;
    filePath: string;
  }> = new Action();
  saveQRCodeImageSuccess: Action<{
    walletAddress: string;
  }> = new Action();
  getAccountPublicKey: Action<{
    spendingPassword: string;
  }> = new Action();
  getICOPublicKey: Action<{
    spendingPassword: string;
  }> = new Action();
  copyWalletPublicKey: Action<{
    publicKey: string;
  }> = new Action();
  copyICOPublicKey: Action<{
    publicKey: string;
  }> = new Action();
  copyAddress: Action<{
    address: string;
  }> = new Action();
  updateCertificateStep: Action<any> = new Action();
  closeCertificateGeneration: Action<any> = new Action();
  closeRewardsCsvGeneration: Action<any> = new Action();
  setCertificateTemplate: Action<{
    selectedTemplate: string;
  }> = new Action();
  finishCertificate: Action<any> = new Action();
  finishRewardsCsv: Action<any> = new Action();

  /* ----------  Transfer Funds  ---------- */
  setActiveAsset: Action<string> = new Action();
  unsetActiveAsset: Action<any> = new Action();
  transferFundsNextStep: Action<any> = new Action();
  transferFundsPrevStep: Action<any> = new Action();
  transferFundsSetSourceWalletId: Action<{
    sourceWalletId: string;
  }> = new Action();
  transferFundsSetTargetWalletId: Action<{
    targetWalletId: string;
  }> = new Action();
  transferFundsRedeem: Action<any> = new Action();
  transferFundsClose: Action<any> = new Action();
  transferFundsCalculateFee: Action<{
    sourceWalletId: string;
  }> = new Action();
  transferFunds: Action<{
    spendingPassword: string;
  }> = new Action();
  createHardwareWallet: Action<{
    walletName: string;
    extendedPublicKey: HardwareWalletExtendedPublicKeyResponse;
    device: TransportDevice;
  }> = new Action();
}
