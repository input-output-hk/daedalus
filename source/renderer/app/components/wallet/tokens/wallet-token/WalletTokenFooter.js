'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const react_1 = __importDefault(require('react'));
const classnames_1 = __importDefault(require('classnames'));
const mobx_react_1 = require('mobx-react');
const react_intl_1 = require('react-intl');
const Button_1 = require('@react-polymorph/components/Button');
const PopOver_1 = require('@react-polymorph/components/PopOver');
const react_svg_inline_1 = __importDefault(require('react-svg-inline'));
const WalletTokenFooter_scss_1 = __importDefault(
  require('./WalletTokenFooter.scss')
);
const AssetAmount_1 = __importDefault(require('../../../assets/AssetAmount'));
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../../assets/images/asse... Remove this comment to see the full error message
const asset_token_warning_ic_inline_svg_1 = __importDefault(
  require('../../../../assets/images/asset-token-warning-ic.inline.svg')
);
const WalletToken_messages_1 = require('./WalletToken.messages');
function WalletTokenFooter(props) {
  const {
    asset,
    className,
    intl,
    isLoading,
    hasWarning,
    onAssetSettings,
    onOpenAssetSend,
  } = props;
  const { recommendedDecimals, decimals } = asset;
  const warningPopOverMessage =
    typeof decimals === 'number'
      ? WalletToken_messages_1.messages.settingsWarningPopOverNotUsing
      : WalletToken_messages_1.messages.settingsWarningPopOverAvailable;
  return react_1.default.createElement(
    'div',
    {
      className: (0, classnames_1.default)(
        WalletTokenFooter_scss_1.default.root,
        className
      ),
    },
    react_1.default.createElement(
      'div',
      { className: WalletTokenFooter_scss_1.default.amount },
      react_1.default.createElement(
        'span',
        { className: WalletTokenFooter_scss_1.default.amountLabel },
        intl.formatMessage(WalletToken_messages_1.messages.amountLabel)
      ),
      react_1.default.createElement(
        'div',
        { className: WalletTokenFooter_scss_1.default.amountValue },
        react_1.default.createElement(AssetAmount_1.default, {
          amount: asset.quantity,
          metadata: asset.metadata,
          decimals: asset.decimals,
          isLoading: isLoading,
        })
      )
    ),
    react_1.default.createElement(
      'div',
      { className: WalletTokenFooter_scss_1.default.buttons },
      onAssetSettings &&
        react_1.default.createElement(
          PopOver_1.PopOver,
          {
            content:
              hasWarning &&
              intl.formatMessage(warningPopOverMessage, {
                recommendedDecimals,
              }),
          },
          react_1.default.createElement(Button_1.Button, {
            className: (0, classnames_1.default)([
              'flat',
              WalletTokenFooter_scss_1.default.button,
              WalletTokenFooter_scss_1.default.settingsButton,
            ]),
            label: react_1.default.createElement(
              react_1.default.Fragment,
              null,
              intl.formatMessage(
                WalletToken_messages_1.messages.settingsButtonLabel
              ),
              hasWarning &&
                react_1.default.createElement(
                  'span',
                  { 'data-testid': 'warning-icon' },
                  react_1.default.createElement(react_svg_inline_1.default, {
                    svg: asset_token_warning_ic_inline_svg_1.default,
                  })
                )
            ),
            onClick: () =>
              onAssetSettings({
                asset,
              }),
          })
        ),
      onOpenAssetSend &&
        react_1.default.createElement(Button_1.Button, {
          className: (0, classnames_1.default)([
            'primary',
            WalletTokenFooter_scss_1.default.button,
            asset.quantity.isZero() &&
              WalletTokenFooter_scss_1.default.disabled,
          ]),
          onClick: () => onOpenAssetSend(asset),
          label: intl.formatMessage(
            WalletToken_messages_1.messages.tokenSendButton
          ),
          disabled: asset.quantity.isZero(),
        })
    )
  );
}
exports.default = (0, react_intl_1.injectIntl)(
  (0, mobx_react_1.observer)(WalletTokenFooter)
);
//# sourceMappingURL=WalletTokenFooter.js.map
