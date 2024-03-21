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
const react_svg_inline_1 = __importDefault(require('react-svg-inline'));
const question_mark_inline_svg_1 = __importDefault(
  require('../../assets/images/question-mark.inline.svg')
);
const AssetTransactionConfirmation_scss_1 = __importDefault(
  require('./AssetTransactionConfirmation.scss')
);
const Asset_1 = __importDefault(require('./Asset'));
const formatters_1 = require('../../utils/formatters');
const messages = (0, react_intl_1.defineMessages)({
  assetLabel: {
    id: 'asset.transactionConfirmation.assetLabel',
    defaultMessage: '!!!Token #{assetNumber}',
    description: '"assetLabel" item on AssetTransactionConfirmation.',
  },
  unformattedAmountLabel: {
    id: 'asset.transactionConfirmation.unformattedAmountLabel',
    defaultMessage: '!!!unformatted amount',
    description:
      '"unformattedAmountLabel" item on AssetTransactionConfirmation.',
  },
  unformattedAmountMessageForHardwareWallets: {
    id:
      'asset.transactionConfirmation.unformattedAmountMessageForHardwareWallets',
    defaultMessage:
      '!!!Native assets may specify a number of decimal places, as defined in the Cardano token registry. Daedalus uses this information to format the amount that is being sent in the transaction.<br /><br />The native token unformatted amount is the amount without these decimal places. Please ensure that you verify both amounts, as some wallet software may not yet use the Cardano token registry.',
    description:
      '"unformattedAmountMessageForHardwareWallets" item on AssetTransactionConfirmation.',
  },
  unformattedAmountMessageForSoftwareWallets: {
    id:
      'asset.transactionConfirmation.unformattedAmountMessageForSoftwareWallets',
    defaultMessage:
      '!!!Native assets may specify a number of decimal places, as defined in the Cardano token registry. Daedalus uses this information to format the amount that is being sent in the transaction.<br /><br />The native token unformatted amount is the amount without these decimal places. Please ensure that you verify both amounts, as some wallet software may not yet use the Cardano token registry.<br /><br />The native token unformatted amount will be displayed on the hardware wallet device during transaction confirmation.',
    description:
      '"unformattedAmountMessageForSoftwareWallets" item on AssetTransactionConfirmation.',
  },
  missingToken: {
    id: 'asset.transactionConfirmation.missingToken',
    defaultMessage: '!!!There is no such token in this wallet',
    description: '"missingToken" item on AssetTransactionConfirmation.',
  },
  insufficientBalance: {
    id: 'asset.transactionConfirmation.insufficientBalance',
    defaultMessage:
      '!!!Insufficient funds. The balance of the token in this wallet is  {formattedBalance} (Unformatted: {unformattedBalance})',
    description: '"insufficientBalance" item on AssetTransactionConfirmation.',
  },
});
const onCopyAssetParam = () => {};
const AssetTransactionConfirmation = (0, mobx_react_1.observer)((props) => {
  const {
    assetNumber,
    asset,
    intl,
    isHardwareWallet,
    tokenIsMissing,
    insufficientBalance,
    amount,
  } = props;
  const hasError = tokenIsMissing || insufficientBalance;
  const { metadata, decimals } = asset;
  const formattedAmount = (0, formatters_1.formattedTokenWalletAmount)(
    amount,
    metadata,
    decimals
  );
  const unformattedAmount = (0, formatters_1.formattedTokenWalletAmount)(
    amount,
    null,
    0
  );
  const formattedBalance = (0, formatters_1.formattedTokenWalletAmount)(
    asset.quantity,
    metadata,
    decimals
  );
  const unformattedBalance = (0, formatters_1.formattedTokenWalletAmount)(
    asset.quantity,
    null,
    0
  );
  const componentStyles = (0, classnames_1.default)(
    AssetTransactionConfirmation_scss_1.default.component,
    {
      [AssetTransactionConfirmation_scss_1.default.error]: hasError,
    }
  );
  const content = react_1.default.createElement(
    react_1.default.Fragment,
    null,
    react_1.default.createElement(
      'div',
      {
        className: AssetTransactionConfirmation_scss_1.default.assetsContainer,
      },
      react_1.default.createElement(
        'h3',
        null,
        react_1.default.createElement(
          'span',
          { className: AssetTransactionConfirmation_scss_1.default.assetLabel },
          intl.formatMessage(messages.assetLabel, {
            assetNumber,
          }),
          ' '
        ),
        react_1.default.createElement(Asset_1.default, {
          asset: asset,
          onCopyAssetParam: onCopyAssetParam,
          hasError: hasError,
        })
      ),
      react_1.default.createElement(
        'div',
        {
          className:
            AssetTransactionConfirmation_scss_1.default.amountFeesWrapper,
        },
        react_1.default.createElement(
          'div',
          { className: AssetTransactionConfirmation_scss_1.default.amount },
          formattedAmount
        )
      )
    ),
    react_1.default.createElement(
      'div',
      {
        className: AssetTransactionConfirmation_scss_1.default.assetsContainer,
      },
      react_1.default.createElement('div', {
        className:
          AssetTransactionConfirmation_scss_1.default.unformattedAmountLine,
      }),
      react_1.default.createElement(
        'div',
        {
          className:
            AssetTransactionConfirmation_scss_1.default.unformattedAmountLabel,
        },
        intl.formatMessage(messages.unformattedAmountLabel),
        react_1.default.createElement(
          PopOver_1.PopOver,
          {
            content: react_1.default.createElement(
              react_intl_1.FormattedHTMLMessage,
              {
                ...messages[
                  isHardwareWallet
                    ? 'unformattedAmountMessageForHardwareWallets'
                    : 'unformattedAmountMessageForSoftwareWallets'
                ],
                tagName: 'div',
              }
            ),
          },
          react_1.default.createElement(
            'div',
            {
              className:
                AssetTransactionConfirmation_scss_1.default.questionMark,
            },
            react_1.default.createElement(react_svg_inline_1.default, {
              svg: question_mark_inline_svg_1.default,
            })
          )
        ),
        ':'
      ),
      react_1.default.createElement(
        'div',
        {
          className:
            AssetTransactionConfirmation_scss_1.default.unformattedAmount,
        },
        unformattedAmount
      )
    )
  );
  if (tokenIsMissing) {
    return react_1.default.createElement(
      'div',
      { className: componentStyles },
      react_1.default.createElement(
        PopOver_1.PopOver,
        {
          content: intl.formatMessage(messages.missingToken),
          appendTo: 'parent',
          placement: 'bottom',
        },
        content
      )
    );
  }
  if (insufficientBalance) {
    return react_1.default.createElement(
      'div',
      { className: componentStyles },
      react_1.default.createElement(
        PopOver_1.PopOver,
        {
          content: intl.formatMessage(messages.insufficientBalance, {
            formattedBalance,
            unformattedBalance,
          }),
          appendTo: 'parent',
          placement: 'bottom',
        },
        content
      )
    );
  }
  return react_1.default.createElement(
    'div',
    { className: componentStyles },
    content
  );
});
exports.default = (0, react_intl_1.injectIntl)(AssetTransactionConfirmation);
//# sourceMappingURL=AssetTransactionConfirmation.js.map
