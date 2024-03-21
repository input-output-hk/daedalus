'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.discreetWalletAmount = void 0;
const formatters_1 = require('../../../utils/formatters');
const discreetWalletAmount = ({
  amount,
  withCurrency = true,
  long = true,
  currency = 'ADA',
}) => {
  return (isDiscreetMode, replacement) => {
    if (!isDiscreetMode) {
      return (0, formatters_1.formattedWalletAmount)(
        amount,
        withCurrency,
        long,
        currency
      );
    }
    if (!withCurrency) {
      return replacement;
    }
    return `${replacement} ${currency}`;
  };
};
exports.discreetWalletAmount = discreetWalletAmount;
//# sourceMappingURL=discreetWalletAmount.js.map
