'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.discreetWalletTokenAmount = void 0;
const formatters_1 = require('../../../utils/formatters');
const discreetWalletTokenAmount = ({ amount, metadata, decimals, isShort }) => {
  return (isDiscreetMode, replacement) => {
    if (!isDiscreetMode) {
      return (0, formatters_1.formattedTokenWalletAmount)(
        amount,
        metadata,
        decimals,
        isShort
      );
    }
    const { ticker } = metadata || {};
    if (!ticker) {
      return replacement;
    }
    return `${replacement} ${ticker}`;
  };
};
exports.discreetWalletTokenAmount = discreetWalletTokenAmount;
//# sourceMappingURL=discreetWalletTokenAmount.js.map
