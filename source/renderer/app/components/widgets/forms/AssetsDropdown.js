'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.onSearchAssetsDropdown = void 0;
const react_1 = __importDefault(require('react'));
const lodash_1 = require('lodash');
const discreetWalletTokenAmount_1 = require('../../../features/discreet-mode/replacers/discreetWalletTokenAmount');
const ItemsDropdown_1 = __importDefault(require('./ItemsDropdown'));
const discreet_mode_1 = require('../../../features/discreet-mode');
const Asset_1 = __importDefault(require('../../assets/Asset'));
const AssetsDropdown_scss_1 = __importDefault(require('./AssetsDropdown.scss'));
const onSearchAssetsDropdown = (searchValue, options) => {
  return (0, lodash_1.filter)(options, ({ asset }) => {
    if (searchValue.length < 3) {
      return true;
    }
    const { policyId, assetName, fingerprint, metadata } = asset;
    const { name, ticker, description } = metadata || {};
    const checkList = [
      policyId,
      assetName,
      fingerprint,
      metadata,
      name,
      ticker,
      description,
    ];
    const regex = new RegExp((0, lodash_1.escapeRegExp)(searchValue), 'i');
    return checkList.some((item) => regex.test(item));
  });
};
exports.onSearchAssetsDropdown = onSearchAssetsDropdown;
function AssetsDropdown({
  assets = [],
  onSearch = exports.onSearchAssetsDropdown,
  ...props
}) {
  const discreetModeFeature = (0, discreet_mode_1.useDiscreetModeFeature)();
  const ItemsDropdownProps = {
    ...(0, lodash_1.omit)(props, ['wallets', 'options']),
    onSearch,
  };
  const formattedOptions = assets.map((asset) => {
    const { uniqueId: value, metadata, quantity, decimals } = asset;
    const detail = discreetModeFeature.discreetValue({
      replacer: (0, discreetWalletTokenAmount_1.discreetWalletTokenAmount)({
        amount: quantity,
        metadata,
        decimals,
      }),
    });
    return {
      label:
        // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
        react_1.default.createElement(Asset_1.default, {
          asset: asset,
          className: AssetsDropdown_scss_1.default.assetToken,
          hidePopOver: true,
          small: true,
        }),
      detail,
      value,
      asset,
    };
  });
  return react_1.default.createElement(ItemsDropdown_1.default, {
    options: formattedOptions,
    ...ItemsDropdownProps,
  });
}
exports.default = AssetsDropdown;
//# sourceMappingURL=AssetsDropdown.js.map
