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
const Button_1 = require('@react-polymorph/components/Button');
const ButtonSkin_1 = require('@react-polymorph/skins/simple/ButtonSkin');
const Link_1 = require('@react-polymorph/components/Link');
const LinkSkin_1 = require('@react-polymorph/skins/simple/LinkSkin');
const ButtonLink_1 = __importDefault(require('../widgets/ButtonLink'));
const LegacyNotification_scss_1 = __importDefault(
  require('./LegacyNotification.scss')
);
const messages = (0, react_intl_1.defineMessages)({
  moveFundsTitle: {
    id: 'wallet.byron.notification.moveFundsTitle',
    defaultMessage: '!!!Move funds from {activeWalletName}',
    description:
      'Title "Move funds from the legacy wallet" on the legacy notification.',
  },
  addWalletTitle: {
    id: 'wallet.byron.notification.addWalletTitle',
    defaultMessage: '!!!Create a Shelley wallet',
    description: 'Title "Create a Shelley wallet" on the legacy notification.',
  },
  moveFundsDescriptionLine1: {
    id: 'wallet.byron.notification.moveFundsDescription.line1',
    defaultMessage:
      '!!!"{activeWalletName}"" is a Byron legacy wallet that does not support Shelley delegation features. To earn ada from delegating your stake, please move all funds from this wallet to a new wallet that is Shelley-compatible.',
    description: 'Legacy notification description.',
  },
  moveFundsDescriptionLine2: {
    id: 'wallet.byron.notification.moveFundsDescription.line2',
    defaultMessage:
      '!!!You can create a {moveFundsLink} or move funds to one of your existing wallets.',
    description: 'Legacy notification description.',
  },
  moveFundsLinkLabel: {
    id: 'wallet.byron.notification.moveFundsDescription.line2.link.label',
    defaultMessage: '!!!brand new wallet',
    description: 'Legacy notification link label.',
  },
  descriptionWithFunds: {
    id: 'wallet.legacy.notification.descriptionWithFunds',
    defaultMessage:
      '!!!"{transferWalletName}"" is a legacy wallet. It does not support Shelley delegation features. To earn ada from delegating your stake, please move all funds from this wallet to a new, Shelley-compatible wallet. You can create a brand new wallet or move funds to one of the existing wallets.',
    description: 'Legacy notification description WithFunds.',
  },
  addWalletDescriptionLine1: {
    id: 'wallet.byron.notification.addWalletDescription.line1',
    defaultMessage:
      '!!!"{activeWalletName}"" is a Byron legacy wallet that does not support Shelley delegation features. To earn ada from delegating your stake, please move all funds from this wallet to a new wallet that is Shelley-compatible.',
    description: 'Legacy notification description.',
  },
  addWalletDescriptionLine2: {
    id: 'wallet.byron.notification.addWalletDescription.line2',
    defaultMessage:
      '!!!Since all of your wallets are Byron legacy wallets you will first need to create a new Shelley wallet.',
    description: 'Legacy notification description.',
  },
  actionLearnMore: {
    id: 'wallet.byron.notification.actionLearnMore',
    defaultMessage: '!!!Learn more',
    description: 'Learn more action of legacy notification.',
  },
  actionMove: {
    id: 'wallet.byron.notification.actionMove',
    defaultMessage: '!!!Move ada to an existing wallet',
    description: 'Move Move ada from this wallet of legacy notification.',
  },
  addWallet: {
    id: 'wallet.byron.notification.addWallet',
    defaultMessage: '!!!Create a new wallet',
    description: 'Create a new wallet action of legacy notification.',
  },
  learnMoreLinkUrl: {
    id: 'wallet.byron.notification.learnMore.url',
    defaultMessage:
      '!!!https://iohk.zendesk.com/hc/en-us/articles/360038726373',
    description: '"Learn more" link URL',
  },
});
let LegacyNotification = class LegacyNotification extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  onLearnMore = () => {
    const { intl } = this.context;
    const learnMoreLinkUrl = intl.formatMessage(messages.learnMoreLinkUrl);
    this.props.onLearnMore(learnMoreLinkUrl);
  };
  getValue = (messageHasRewardsWallets, messageNoRewardsWallets, _values) => {
    const { hasRewardsWallets, activeWalletName } = this.props;
    const message = hasRewardsWallets
      ? messageHasRewardsWallets
      : messageNoRewardsWallets;
    const values = {
      activeWalletName,
      ..._values,
    };
    return react_1.default.createElement(react_intl_1.FormattedMessage, {
      ...{ message },
      values: values,
    });
  };
  render() {
    const { intl } = this.context;
    const { onTransferFunds, hasRewardsWallets, onWalletAdd } = this.props;
    const { getValue, onLearnMore } = this;
    const showLearnMoreButton = false;
    const moveFundsLink = react_1.default.createElement(Link_1.Link, {
      className: LegacyNotification_scss_1.default.descriptionLink,
      onClick: onWalletAdd,
      label: intl.formatMessage(messages.moveFundsLinkLabel),
      skin: LinkSkin_1.LinkSkin,
      hasIconAfter: false,
    });
    const title = getValue(messages.moveFundsTitle, messages.addWalletTitle);
    const description1 = getValue(
      messages.moveFundsDescriptionLine1,
      messages.addWalletDescriptionLine1
    );
    const description2 = getValue(
      messages.moveFundsDescriptionLine2,
      messages.addWalletDescriptionLine2,
      {
        moveFundsLink,
      }
    );
    const buttonLabel = getValue(messages.actionMove, messages.addWallet);
    const buttonAction = hasRewardsWallets ? onTransferFunds : onWalletAdd;
    return react_1.default.createElement(
      'div',
      { className: LegacyNotification_scss_1.default.component },
      react_1.default.createElement(
        'div',
        { className: LegacyNotification_scss_1.default.title },
        title
      ),
      react_1.default.createElement(
        'div',
        { className: LegacyNotification_scss_1.default.description },
        react_1.default.createElement('p', null, description1),
        react_1.default.createElement('p', null, description2)
      ),
      react_1.default.createElement(
        'div',
        { className: LegacyNotification_scss_1.default.actions },
        showLearnMoreButton &&
          react_1.default.createElement(
            ButtonLink_1.default,
            // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
            {
              // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
              className: LegacyNotification_scss_1.default.actionLearnMore,
              onClick: onLearnMore,
              skin: ButtonSkin_1.ButtonSkin,
              label: intl.formatMessage(messages.actionLearnMore),
              linkProps: {
                className: LegacyNotification_scss_1.default.externalLink,
                hasIconBefore: false,
                hasIconAfter: true,
              },
            }
          ),
        react_1.default.createElement(Button_1.Button, {
          className: LegacyNotification_scss_1.default.actionMove,
          label: buttonLabel,
          onClick: buttonAction,
          skin: ButtonSkin_1.ButtonSkin,
        })
      )
    );
  }
};
LegacyNotification = __decorate([mobx_react_1.observer], LegacyNotification);
exports.default = LegacyNotification;
//# sourceMappingURL=LegacyNotification.js.map
