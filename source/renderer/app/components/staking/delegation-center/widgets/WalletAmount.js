'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.WalletAmount = void 0;
const react_1 = __importDefault(require('react'));
const react_intl_1 = require('react-intl');
const discreet_mode_1 = require('../../../../features/discreet-mode');
function WalletAmount({ amount, walletAmount }) {
  const discreetModeFeature = (0, discreet_mode_1.useDiscreetModeFeature)();
  return react_1.default.createElement(react_intl_1.FormattedHTMLMessage, {
    ...walletAmount,
    values: {
      amount: discreetModeFeature.discreetValue({
        value: amount,
      }),
    },
  });
}
exports.WalletAmount = WalletAmount;
//# sourceMappingURL=WalletAmount.js.map
