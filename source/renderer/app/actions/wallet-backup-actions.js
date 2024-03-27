'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const Action_1 = __importDefault(require('./lib/Action')); // ======= WALLET BACKUP ACTIONS =======
class WalletBackupActions {
  startWalletBackup = new Action_1.default();
  initiateWalletBackup = new Action_1.default();
  acceptPrivacyNoticeForWalletBackup = new Action_1.default();
  continueToRecoveryPhraseForWalletBackup = new Action_1.default();
  updateWalletBackupVerificationPhrase = new Action_1.default();
  acceptWalletBackupTermOffline = new Action_1.default();
  acceptWalletBackupTermRecovery = new Action_1.default();
  restartWalletBackup = new Action_1.default();
  cancelWalletBackup = new Action_1.default();
  finishWalletBackup = new Action_1.default();
}
exports.default = WalletBackupActions;
//# sourceMappingURL=wallet-backup-actions.js.map
