'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const Action_1 = __importDefault(require('./lib/Action'));
class StakingActions {
  fakeStakePoolsLoading = new Action_1.default();
  goToStakingInfoPage = new Action_1.default();
  goToStakingDelegationCenterPage = new Action_1.default();
  joinStakePool = new Action_1.default();
  quitStakePool = new Action_1.default();
  updateDelegatingStake = new Action_1.default();
  rankStakePools = new Action_1.default();
  selectDelegationWallet = new Action_1.default();
  requestCSVFile = new Action_1.default();
  requestCSVFileSuccess = new Action_1.default();
  selectSmashServerUrl = new Action_1.default();
  resetSmashServerError = new Action_1.default();
  /* ----------  Redeem ITN Rewards  ---------- */
  onRedeemStart = new Action_1.default();
  onConfigurationContinue = new Action_1.default();
  onCalculateRedeemWalletFees = new Action_1.default();
  onConfirmationContinue = new Action_1.default();
  onResultContinue = new Action_1.default();
  closeRedeemDialog = new Action_1.default();
  setStakingInfoWasOpen = new Action_1.default();
}
exports.default = StakingActions;
//# sourceMappingURL=staking-actions.js.map
