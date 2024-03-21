'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.getFormattedAssetAmount = void 0;
const bignumber_js_1 = __importDefault(require('bignumber.js'));
const formatters_1 = require('../../../utils/formatters');
const getFormattedAssetAmount = ({ metadata, decimals }, assetAmount = 0) => {
  return (0, formatters_1.formattedTokenWalletAmount)(
    new bignumber_js_1.default(assetAmount),
    metadata,
    decimals
  );
};
exports.getFormattedAssetAmount = getFormattedAssetAmount;
//# sourceMappingURL=helpers.js.map
