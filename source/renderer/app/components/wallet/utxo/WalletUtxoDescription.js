'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.WalletUtxoDescription = void 0;
const react_1 = __importDefault(require('react'));
const react_intl_1 = require('react-intl');
const mobx_react_1 = require('mobx-react');
const discreet_mode_1 = require('../../../features/discreet-mode');
function WalletUtxoDescriptionComponent({
  description,
  formattedWalletAmount,
  walletUtxosAmount,
}) {
  const discreetModeFeature = (0, discreet_mode_1.useDiscreetModeFeature)();
  return react_1.default.createElement(react_intl_1.FormattedHTMLMessage, {
    ...description,
    values: {
      formattedWalletAmount: discreetModeFeature.discreetValue({
        value: formattedWalletAmount,
      }),
      walletUtxosAmount,
    },
  });
}
exports.WalletUtxoDescription = (0, mobx_react_1.observer)(
  WalletUtxoDescriptionComponent
);
//# sourceMappingURL=WalletUtxoDescription.js.map
