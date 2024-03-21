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
var __decorate =
  (this && this.__decorate) ||
  function (decorators, target, key, desc) {
    var c = arguments.length,
      r =
        c < 3
          ? target
          : desc === null
          ? (desc = Object.getOwnPropertyDescriptor(target, key))
          : desc,
      d;
    if (typeof Reflect === 'object' && typeof Reflect.decorate === 'function')
      r = Reflect.decorate(decorators, target, key, desc);
    else
      for (var i = decorators.length - 1; i >= 0; i--)
        if ((d = decorators[i]))
          r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
    return c > 3 && r && Object.defineProperty(target, key, r), r;
  };
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
const react_1 = __importStar(require('react'));
const mobx_react_1 = require('mobx-react');
const react_intl_1 = require('react-intl');
const BorderedBox_1 = __importDefault(require('../../../widgets/BorderedBox'));
const WalletNoTokens_scss_1 = __importDefault(require('./WalletNoTokens.scss'));
const ExternalLinkButton_1 = require('../../../widgets/ExternalLinkButton');
const messages = (0, react_intl_1.defineMessages)({
  tokensTitle: {
    id: 'wallet.summary.assets.tokensTitle',
    defaultMessage: '!!!Tokens',
    description: 'Number of tokens title on Wallet summary assets page',
  },
  learnMoreTextTop: {
    id: 'wallet.summary.noTokens.learnMore.textTop',
    defaultMessage: '!!!Want to find out more about native tokens?',
    description: '"Learn more" text in the Wallets Summary No Tokens component',
  },
  learnMoreTextBottom: {
    id: 'wallet.summary.noTokens.learnMore.textBottom',
    defaultMessage: '!!!Start by visiting the IOHK blog for a useful primer.',
    description: '"Learn more" text in the Wallets Summary No Tokens component',
  },
  learnMoreLinkLabel: {
    id: 'wallet.summary.noTokens.learnMore.linkLabel',
    defaultMessage: '!!!Learn more',
    description:
      '"Learn more" label or button in the Wallets Summary No Tokens component',
  },
  learnMoreLinkUrl: {
    id: 'wallet.summary.noTokens.learnMore.linkUrl',
    defaultMessage:
      '!!!https://iohk.io/en/blog/posts/2021/02/04/native-tokens-to-bring-new-utility-to-life-on-cardano/',
    description:
      '"Learn more" link URL in the Wallets Summary No Tokens component',
  },
});
let WalletSummaryNoTokens = class WalletSummaryNoTokens extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  render() {
    const { isLoadingAssets, onExternalLinkClick, numberOfAssets } = this.props;
    const { intl } = this.context;
    return react_1.default.createElement(
      react_1.default.Fragment,
      null,
      !isLoadingAssets &&
        react_1.default.createElement(
          'div',
          { className: WalletNoTokens_scss_1.default.numberOfAssets },
          intl.formatMessage(messages.tokensTitle),
          ' (',
          numberOfAssets,
          ')'
        ),
      react_1.default.createElement(
        'div',
        { className: WalletNoTokens_scss_1.default.component },
        react_1.default.createElement(
          BorderedBox_1.default,
          null,
          react_1.default.createElement(
            'div',
            { className: WalletNoTokens_scss_1.default.noTokensContainer },
            react_1.default.createElement(
              'div',
              {
                className: WalletNoTokens_scss_1.default.noTokensLeftContainer,
              },
              react_1.default.createElement(
                'p',
                null,
                intl.formatMessage(messages.learnMoreTextTop)
              ),
              react_1.default.createElement(
                'p',
                null,
                intl.formatMessage(messages.learnMoreTextBottom)
              )
            ),
            react_1.default.createElement(
              ExternalLinkButton_1.ExternalLinkButton,
              {
                label: intl.formatMessage(messages.learnMoreLinkLabel),
                onClick: () =>
                  onExternalLinkClick(
                    intl.formatMessage(messages.learnMoreLinkUrl)
                  ),
              }
            )
          )
        )
      )
    );
  }
};
WalletSummaryNoTokens = __decorate(
  [mobx_react_1.observer],
  WalletSummaryNoTokens
);
exports.default = WalletSummaryNoTokens;
//# sourceMappingURL=WalletNoTokens.js.map
