'use strict';
// @ts-nocheck
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const react_1 = __importDefault(require('react'));
const classnames_1 = __importDefault(require('classnames'));
const react_intl_1 = require('react-intl');
const PopOver_1 = require('@react-polymorph/components/PopOver');
const mobx_react_1 = require('mobx-react');
const AssetsTransactionConfirmation_scss_1 = __importDefault(
  require('./AssetsTransactionConfirmation.scss')
);
const AssetTransactionConfirmation_1 = __importDefault(
  require('./AssetTransactionConfirmation')
);
const global_messages_1 = __importDefault(
  require('../../i18n/global-messages')
);
const formatters_1 = require('../../utils/formatters');
const assets_1 = require('../../utils/assets');
const AssetsTransactionConfirmation = (0, mobx_react_1.observer)((props) => {
  const {
    adaAmount,
    assets,
    assetsAmounts,
    className,
    intl,
    wallet,
    adaError,
  } = props;
  const insufficientAdaAmount = wallet?.amount.isLessThan(adaAmount);
  const componentStyles = (0, classnames_1.default)([
    AssetsTransactionConfirmation_scss_1.default.component,
    className,
  ]);
  const adaAmountStyles = (0, classnames_1.default)([
    AssetsTransactionConfirmation_scss_1.default.adaAmount,
    insufficientAdaAmount
      ? AssetsTransactionConfirmation_scss_1.default.adaAmountError
      : null,
  ]);
  const adaAmountContent = react_1.default.createElement(
    'div',
    { className: adaAmountStyles },
    react_1.default.createElement(
      'p',
      null,
      intl.formatMessage(global_messages_1.default.adaName)
    ),
    react_1.default.createElement(
      'div',
      { className: AssetsTransactionConfirmation_scss_1.default.amount },
      (0, formatters_1.formattedWalletAmount)(adaAmount)
    )
  );
  return react_1.default.createElement(
    'div',
    { className: componentStyles },
    adaError
      ? react_1.default.createElement(
          PopOver_1.PopOver,
          {
            content: adaError,
            className:
              AssetsTransactionConfirmation_scss_1.default.adaErrorPopOver,
            appendTo: 'parent',
          },
          adaAmountContent
        )
      : adaAmountContent,
    assets.map((asset, index) =>
      react_1.default.createElement(AssetTransactionConfirmation_1.default, {
        key: asset.uniqueId,
        assetNumber: index + 1,
        isHardwareWallet: false,
        asset: asset,
        amount: assetsAmounts[index],
        tokenIsMissing: (0, assets_1.isTokenMissingInWallet)(wallet, asset),
        insufficientBalance:
          !!wallet &&
          !(0, assets_1.tokenHasBalance)(asset, assetsAmounts[index]),
      })
    )
  );
});
exports.default = (0, react_intl_1.injectIntl)(AssetsTransactionConfirmation);
//# sourceMappingURL=AssetsTransactionConfirmation.js.map
