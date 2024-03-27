'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const Action_1 = __importDefault(require('./lib/Action'));
class WalletsActions {
  refreshWalletsDataSuccess = new Action_1.default();
  /* ----------  Create Wallet  ---------- */
  createWallet = new Action_1.default();
  createWalletBegin = new Action_1.default();
  createWalletChangeStep = new Action_1.default();
  createWalletClose = new Action_1.default();
  createWalletAbort = new Action_1.default();
  /* ----------  Restore Wallet  ---------- */
  restoreWalletBegin = new Action_1.default();
  restoreWalletEnd = new Action_1.default();
  restoreWalletChangeStep = new Action_1.default();
  restoreWalletClose = new Action_1.default();
  restoreWalletCancelClose = new Action_1.default();
  restoreWalletSetKind = new Action_1.default();
  restoreWalletSetMnemonics = new Action_1.default();
  restoreWalletSetConfig = new Action_1.default();
  restoreWallet = new Action_1.default();
  importWalletFromFile = new Action_1.default();
  deleteWallet = new Action_1.default();
  undelegateWallet = new Action_1.default();
  setUndelegateWalletSubmissionSuccess = new Action_1.default();
  sendMoney = new Action_1.default();
  chooseWalletExportType = new Action_1.default();
  generateCertificate = new Action_1.default();
  generateCsv = new Action_1.default();
  generateAddressPDF = new Action_1.default();
  generateAddressPDFSuccess = new Action_1.default();
  saveQRCodeImage = new Action_1.default();
  saveQRCodeImageSuccess = new Action_1.default();
  getAccountPublicKey = new Action_1.default();
  getICOPublicKey = new Action_1.default();
  copyWalletPublicKey = new Action_1.default();
  copyICOPublicKey = new Action_1.default();
  copyAddress = new Action_1.default();
  updateCertificateStep = new Action_1.default();
  closeCertificateGeneration = new Action_1.default();
  closeRewardsCsvGeneration = new Action_1.default();
  setCertificateTemplate = new Action_1.default();
  finishCertificate = new Action_1.default();
  finishRewardsCsv = new Action_1.default();
  /* ----------  Transfer Funds  ---------- */
  setActiveAsset = new Action_1.default();
  unsetActiveAsset = new Action_1.default();
  transferFundsNextStep = new Action_1.default();
  transferFundsPrevStep = new Action_1.default();
  transferFundsSetSourceWalletId = new Action_1.default();
  transferFundsSetTargetWalletId = new Action_1.default();
  transferFundsRedeem = new Action_1.default();
  transferFundsClose = new Action_1.default();
  transferFundsCalculateFee = new Action_1.default();
  transferFunds = new Action_1.default();
  createHardwareWallet = new Action_1.default();
}
exports.default = WalletsActions;
//# sourceMappingURL=wallets-actions.js.map
