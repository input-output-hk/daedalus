'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const Action_1 = __importDefault(require('./lib/Action'));
class WalletSettingsActions {
  cancelEditingWalletField = new Action_1.default();
  startEditingWalletField = new Action_1.default();
  stopEditingWalletField = new Action_1.default();
  updateWalletField = new Action_1.default();
  updateSpendingPassword = new Action_1.default();
  exportToFile = new Action_1.default();
  /* ----------  UTXO  ---------- */
  startWalletUtxoPolling = new Action_1.default();
  stopWalletUtxoPolling = new Action_1.default();
  /* ----------  Recovery Phrase Verification  ---------- */
  recoveryPhraseVerificationContinue = new Action_1.default();
  recoveryPhraseVerificationCheck = new Action_1.default();
  recoveryPhraseVerificationClose = new Action_1.default();
  toggleShowUsedAddresses = new Action_1.default();
}
exports.default = WalletSettingsActions;
//# sourceMappingURL=wallet-settings-actions.js.map
