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
const PopOver_1 = require('@react-polymorph/components/PopOver');
const react_intl_1 = require('react-intl');
const mobx_react_1 = require('mobx-react');
const discreetWalletTokenAmount_1 = require('../../features/discreet-mode/replacers/discreetWalletTokenAmount');
const AssetAmount_scss_1 = __importDefault(require('./AssetAmount.scss'));
const discreet_mode_1 = require('../../features/discreet-mode');
const messages = (0, react_intl_1.defineMessages)({
  unformattedAmount: {
    id: 'assets.assetAmount.unformattedAmount',
    defaultMessage: '!!!Unformatted amount {amount}',
    description: 'Unformatted amount',
  },
});
function AssetAmount({
  amount,
  metadata,
  decimals,
  isLoading,
  className,
  isShort,
}) {
  const discreetModeFeature = (0, discreet_mode_1.useDiscreetModeFeature)();
  if (isLoading) return '-';
  const componentStyles = (0, classnames_1.default)([
    AssetAmount_scss_1.default.component,
    className,
  ]);
  const content = !isLoading
    ? discreetModeFeature.discreetValue({
        replacer: (0, discreetWalletTokenAmount_1.discreetWalletTokenAmount)({
          amount,
          metadata,
          decimals,
          isShort,
        }),
      })
    : '-';
  return react_1.default.createElement(
    'div',
    { className: componentStyles },
    decimals
      ? react_1.default.createElement(
          PopOver_1.PopOver,
          {
            content: react_1.default.createElement(
              react_intl_1.FormattedHTMLMessage,
              {
                ...messages.unformattedAmount,
                values: {
                  amount: discreetModeFeature.discreetValue({
                    replacer: (0,
                    discreetWalletTokenAmount_1.discreetWalletTokenAmount)({
                      amount,
                      metadata: null,
                      decimals: 0,
                    }),
                  }),
                },
              }
            ),
            visible: decimals ? undefined : false,
            className: AssetAmount_scss_1.default.unformattedAmount,
          },
          content
        )
      : react_1.default.createElement('span', null, content)
  );
}
// @ts-ignore ts-migrate(2345) FIXME: Argument of type '({ amount, metadata, decimals, i... Remove this comment to see the full error message
exports.default = (0, mobx_react_1.observer)(AssetAmount);
//# sourceMappingURL=AssetAmount.js.map
