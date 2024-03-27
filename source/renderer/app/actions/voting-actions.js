'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const Action_1 = __importDefault(require('./lib/Action'));
class VotingActions {
  selectWallet = new Action_1.default();
  sendTransaction = new Action_1.default();
  generateQrCode = new Action_1.default();
  saveAsPDF = new Action_1.default();
  saveAsPDFSuccess = new Action_1.default();
  nextRegistrationStep = new Action_1.default();
  previousRegistrationStep = new Action_1.default();
  resetRegistration = new Action_1.default();
  showConfirmationDialog = new Action_1.default();
  closeConfirmationDialog = new Action_1.default();
}
exports.default = VotingActions;
//# sourceMappingURL=voting-actions.js.map
