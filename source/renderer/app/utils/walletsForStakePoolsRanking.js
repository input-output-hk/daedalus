'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.getAllAmounts = exports.getFilteredWallets = void 0;
const bignumber_js_1 = __importDefault(require('bignumber.js'));
const stakingConfig_1 = require('../config/stakingConfig');
const getFilteredWallets = (wallets) => {
  return wallets.filter(
    (w) =>
      w.amount.isGreaterThanOrEqualTo(
        new bignumber_js_1.default(stakingConfig_1.MIN_DELEGATION_FUNDS)
      ) && !w.isLegacy
  );
};
exports.getFilteredWallets = getFilteredWallets;
const getAllAmounts = (wallets) => {
  const filteredWallets = (0, exports.getFilteredWallets)(wallets);
  if (filteredWallets.length > 0) {
    return filteredWallets
      .map((w) => w.amount)
      .reduce((acc, cur) => acc.plus(cur), new bignumber_js_1.default(0));
  }
  return new bignumber_js_1.default(stakingConfig_1.MIN_DELEGATION_FUNDS);
};
exports.getAllAmounts = getAllAmounts;
//# sourceMappingURL=walletsForStakePoolsRanking.js.map
