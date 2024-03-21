'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const Action_1 = __importDefault(require('./lib/Action'));
class WalletMigrationActions {
  initiateMigration = new Action_1.default();
  startMigration = new Action_1.default();
  finishMigration = new Action_1.default();
  resetMigration = new Action_1.default();
  toggleWalletImportSelection = new Action_1.default();
  updateWalletName = new Action_1.default();
  nextStep = new Action_1.default();
  selectExportSourcePath = new Action_1.default();
  resetExportSourcePath = new Action_1.default();
}
exports.default = WalletMigrationActions;
//# sourceMappingURL=wallet-migration-actions.js.map
