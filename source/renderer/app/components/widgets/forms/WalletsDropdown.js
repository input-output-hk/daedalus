'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.onSearchWalletsDropdown = void 0;
const react_1 = __importDefault(require('react'));
const mobx_react_1 = require('mobx-react');
const lodash_1 = require('lodash');
const discreetWalletAmount_1 = require('../../../features/discreet-mode/replacers/discreetWalletAmount');
const WalletsDropdownLabel_1 = __importDefault(
  require('./WalletsDropdownLabel')
);
const discreet_mode_1 = require('../../../features/discreet-mode');
const ItemsDropdown_1 = __importDefault(require('./ItemsDropdown'));
const onSearchWalletsDropdown = (searchValue, options) => {
  return (0, lodash_1.filter)(options, (option) => {
    const { walletName, detail } = option;
    const regex = new RegExp((0, lodash_1.escapeRegExp)(searchValue), 'i');
    return [walletName, detail].some((item) => regex.test(item));
  });
};
exports.onSearchWalletsDropdown = onSearchWalletsDropdown;
function WalletsDropdown({
  className,
  getStakePoolById,
  numberOfStakePools,
  onSearch = exports.onSearchWalletsDropdown,
  wallets = [],
  ...props
}) {
  const discreetModeFeature = (0, discreet_mode_1.useDiscreetModeFeature)();
  const itemsDropdownProps = {
    ...(0, lodash_1.omit)(props, ['wallets', 'options']),
    onSearch,
  };
  const formattedOptions = wallets.map((wallet) => {
    const {
      id: value,
      amount,
      isRestoring,
      isSyncing,
      restorationProgress: syncingProgress,
    } = wallet;
    const detail = !isRestoring
      ? discreetModeFeature.discreetValue({
          replacer: (0, discreetWalletAmount_1.discreetWalletAmount)({
            amount,
          }),
        })
      : null;
    return {
      label: react_1.default.createElement(WalletsDropdownLabel_1.default, {
        wallet: wallet,
        getStakePoolById: getStakePoolById,
        numberOfStakePools: numberOfStakePools,
      }),
      detail,
      value,
      walletName: wallet.name,
      isSyncing,
      syncingProgress,
    };
  });
  return react_1.default.createElement(ItemsDropdown_1.default, {
    className: className,
    options: formattedOptions,
    ...itemsDropdownProps,
  });
}
exports.default = (0, mobx_react_1.observer)(WalletsDropdown);
//# sourceMappingURL=WalletsDropdown.js.map
