'use strict';
var __createBinding =
  (this && this.__createBinding) ||
  (Object.create
    ? function (o, m, k, k2) {
        if (k2 === undefined) k2 = k;
        var desc = Object.getOwnPropertyDescriptor(m, k);
        if (
          !desc ||
          ('get' in desc ? !m.__esModule : desc.writable || desc.configurable)
        ) {
          desc = {
            enumerable: true,
            get: function () {
              return m[k];
            },
          };
        }
        Object.defineProperty(o, k2, desc);
      }
    : function (o, m, k, k2) {
        if (k2 === undefined) k2 = k;
        o[k2] = m[k];
      });
var __setModuleDefault =
  (this && this.__setModuleDefault) ||
  (Object.create
    ? function (o, v) {
        Object.defineProperty(o, 'default', { enumerable: true, value: v });
      }
    : function (o, v) {
        o['default'] = v;
      });
var __importStar =
  (this && this.__importStar) ||
  function (mod) {
    if (mod && mod.__esModule) return mod;
    var result = {};
    if (mod != null)
      for (var k in mod)
        if (k !== 'default' && Object.prototype.hasOwnProperty.call(mod, k))
          __createBinding(result, mod, k);
    __setModuleDefault(result, mod);
    return result;
  };
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.DialogContentWithAssets = void 0;
const react_1 = __importStar(require('react'));
const compose_1 = __importDefault(require('lodash/fp/compose'));
const react_svg_inline_1 = __importDefault(require('react-svg-inline'));
const mobx_react_1 = require('mobx-react');
const PopOver_1 = require('@react-polymorph/components/PopOver');
const react_intl_1 = require('react-intl');
const question_mark_inline_svg_1 = __importDefault(
  require('../../../../assets/images/question-mark.inline.svg')
);
const Asset_1 = __importDefault(require('../../../../components/assets/Asset'));
const global_messages_1 = __importDefault(
  require('../../../../i18n/global-messages')
);
const messages_1 = require('./messages');
const helpers_1 = require('./helpers');
const DialogContentWithAssets_scss_1 = __importDefault(
  require('./DialogContentWithAssets.scss')
);
function Component({
  intl,
  amount,
  receiver,
  transactionFee,
  isHardwareWallet,
  selectedAssets,
  assetsAmounts,
  onCopyAssetParam,
}) {
  const assetsSeparatorBasicHeight = 27;
  const assetsSeparatorCalculatedHeight = selectedAssets.length
    ? assetsSeparatorBasicHeight * selectedAssets.length * 2 - 18
    : assetsSeparatorBasicHeight;
  return react_1.default.createElement(
    'div',
    { className: DialogContentWithAssets_scss_1.default.root },
    react_1.default.createElement(
      'div',
      {
        className: DialogContentWithAssets_scss_1.default.addressToLabelWrapper,
      },
      react_1.default.createElement(
        'div',
        { className: DialogContentWithAssets_scss_1.default.receiverRow },
        react_1.default.createElement(
          'div',
          { className: DialogContentWithAssets_scss_1.default.receiverRowItem },
          react_1.default.createElement(
            'h2',
            null,
            intl.formatMessage(messages_1.messages.receiverLabel)
          ),
          react_1.default.createElement(
            'div',
            {
              className:
                DialogContentWithAssets_scss_1.default.receiverRowItemAddresses,
            },
            react_1.default.createElement(
              'p',
              { className: DialogContentWithAssets_scss_1.default.addressTo },
              receiver
            ),
            react_1.default.createElement(
              'div',
              {
                className: DialogContentWithAssets_scss_1.default.assetsWrapper,
              },
              react_1.default.createElement('div', {
                className:
                  DialogContentWithAssets_scss_1.default.assetsSeparator,
                style: {
                  height: `${assetsSeparatorCalculatedHeight}px`,
                  top: `${assetsSeparatorCalculatedHeight + 5}px`,
                  marginTop: `-${assetsSeparatorCalculatedHeight + 5}px`,
                },
              }),
              react_1.default.createElement(
                'div',
                {
                  className:
                    DialogContentWithAssets_scss_1.default.assetsContainer,
                },
                react_1.default.createElement(
                  'h3',
                  null,
                  react_1.default.createElement(
                    'span',
                    null,
                    intl.formatMessage(global_messages_1.default.adaName)
                  )
                ),
                react_1.default.createElement(
                  'div',
                  {
                    className:
                      DialogContentWithAssets_scss_1.default.amountFeesWrapper,
                  },
                  react_1.default.createElement(
                    'div',
                    {
                      className: DialogContentWithAssets_scss_1.default.amount,
                    },
                    amount,
                    ' ',
                    intl.formatMessage(global_messages_1.default.adaUnit)
                  )
                )
              ),
              selectedAssets.map(
                (asset, index) =>
                  asset?.uniqueId &&
                  react_1.default.createElement(
                    react_1.Fragment,
                    { key: asset.uniqueId },
                    react_1.default.createElement(
                      'div',
                      {
                        className:
                          DialogContentWithAssets_scss_1.default
                            .assetsContainer,
                      },
                      react_1.default.createElement(
                        'h3',
                        null,
                        react_1.default.createElement(
                          'span',
                          null,
                          intl.formatMessage(messages_1.messages.assetLabel),
                          '\u00A0#',
                          index + 1
                        ),
                        react_1.default.createElement(Asset_1.default, {
                          asset: asset,
                          onCopyAssetParam: onCopyAssetParam,
                          className:
                            DialogContentWithAssets_scss_1.default.assetToken,
                        })
                      ),
                      react_1.default.createElement(
                        'div',
                        {
                          className:
                            DialogContentWithAssets_scss_1.default
                              .amountFeesWrapper,
                        },
                        react_1.default.createElement(
                          'div',
                          {
                            className:
                              DialogContentWithAssets_scss_1.default.amount,
                          },
                          (0, helpers_1.getFormattedAssetAmount)(
                            asset,
                            Number(assetsAmounts[index])
                          )
                        )
                      )
                    ),
                    react_1.default.createElement(
                      'div',
                      {
                        className:
                          DialogContentWithAssets_scss_1.default
                            .assetsContainer,
                      },
                      react_1.default.createElement('div', {
                        className:
                          DialogContentWithAssets_scss_1.default
                            .unformattedAmountLine,
                      }),
                      react_1.default.createElement(
                        'div',
                        {
                          className:
                            DialogContentWithAssets_scss_1.default
                              .unformattedAmountLabel,
                        },
                        intl.formatMessage(
                          messages_1.messages.unformattedAmountLabel
                        ),
                        react_1.default.createElement(
                          PopOver_1.PopOver,
                          {
                            content: react_1.default.createElement(
                              'div',
                              { className: 'UnformattedAmountTooltip' },
                              react_1.default.createElement(
                                react_intl_1.FormattedHTMLMessage,
                                {
                                  ...messages_1.messages[
                                    isHardwareWallet
                                      ? 'unformattedAmountMessageForHardwareWallets'
                                      : 'unformattedAmountMessageForSoftwareWallets'
                                  ],
                                  tagName: 'div',
                                }
                              )
                            ),
                            key: 'tooltip',
                          },
                          react_1.default.createElement(
                            'div',
                            {
                              className:
                                DialogContentWithAssets_scss_1.default
                                  .questionMark,
                            },
                            react_1.default.createElement(
                              react_svg_inline_1.default,
                              { svg: question_mark_inline_svg_1.default }
                            )
                          )
                        ),
                        ':'
                      ),
                      react_1.default.createElement(
                        'div',
                        {
                          className:
                            DialogContentWithAssets_scss_1.default
                              .unformattedAmount,
                        },
                        assetsAmounts[index] || 0
                      )
                    )
                  )
              )
            )
          )
        )
      )
    ),
    react_1.default.createElement(
      'div',
      { className: DialogContentWithAssets_scss_1.default.feesWrapper },
      react_1.default.createElement(
        'div',
        { className: DialogContentWithAssets_scss_1.default.feesLabel },
        intl.formatMessage(messages_1.messages.feesLabel)
      ),
      react_1.default.createElement(
        'div',
        { className: DialogContentWithAssets_scss_1.default.fees },
        '+',
        transactionFee,
        react_1.default.createElement(
          'span',
          null,
          '\u00A0',
          intl.formatMessage(global_messages_1.default.adaUnit)
        )
      )
    )
  );
}
exports.DialogContentWithAssets = (0, compose_1.default)(
  react_intl_1.injectIntl,
  mobx_react_1.observer
)(Component);
//# sourceMappingURL=DialogContentWithAssets.js.map
