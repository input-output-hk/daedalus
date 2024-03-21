'use strict';
var __createBinding =
  (this && this.__createBinding) ||
  (Object.create
    ? function (o, m, k, k2) {
        if (k2 === undefined) k2 = k;
        var desc = Object.getOwnPropertyDescriptor(m, k);
        if (
          !desc ||
          ('get' in desc ? !m.__esModule : desc.writable || desc.configurable)
        ) {
          desc = {
            enumerable: true,
            get: function () {
              return m[k];
            },
          };
        }
        Object.defineProperty(o, k2, desc);
      }
    : function (o, m, k, k2) {
        if (k2 === undefined) k2 = k;
        o[k2] = m[k];
      });
var __setModuleDefault =
  (this && this.__setModuleDefault) ||
  (Object.create
    ? function (o, v) {
        Object.defineProperty(o, 'default', { enumerable: true, value: v });
      }
    : function (o, v) {
        o['default'] = v;
      });
var __importStar =
  (this && this.__importStar) ||
  function (mod) {
    if (mod && mod.__esModule) return mod;
    var result = {};
    if (mod != null)
      for (var k in mod)
        if (k !== 'default' && Object.prototype.hasOwnProperty.call(mod, k))
          __createBinding(result, mod, k);
    __setModuleDefault(result, mod);
    return result;
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.shouldShowEmptyWalletWarning = exports.isWalletRewardsWithdrawalPossible = void 0;
exports.default = Promise.resolve()
  .then(() => __importStar(require('@iohk-jormungandr/wallet-js')))
  .then((modules) => modules);
const MINIMUM_ADA_BALANCE_FOR_WITHDRAWING_REWARDS = 10; // 1 ADA | unit: ADA
const isWalletRewardsWithdrawalPossible = (transactionAmount, walletBalance) =>
  !!transactionAmount &&
  !!walletBalance &&
  transactionAmount
    .plus(MINIMUM_ADA_BALANCE_FOR_WITHDRAWING_REWARDS)
    .isLessThanOrEqualTo(walletBalance);
exports.isWalletRewardsWithdrawalPossible = isWalletRewardsWithdrawalPossible;
// For more details check acceptance tests https://github.com/input-output-hk/daedalus/pull/2617
const shouldShowEmptyWalletWarning = (
  totalAmountToSpend,
  wallet,
  hasAssets = false
) => {
  const { amount: walletBalance, isLegacy, isDelegating } = wallet;
  const willRemainZeroAdaAndZeroAssetsAndNotDelegating =
    !isDelegating &&
    walletBalance.minus(totalAmountToSpend).isZero() &&
    !hasAssets;
  if (willRemainZeroAdaAndZeroAssetsAndNotDelegating) return false;
  return (
    !isLegacy &&
    !(0, exports.isWalletRewardsWithdrawalPossible)(
      totalAmountToSpend,
      walletBalance
    )
  );
};
exports.shouldShowEmptyWalletWarning = shouldShowEmptyWalletWarning;
//# sourceMappingURL=walletUtils.js.map
