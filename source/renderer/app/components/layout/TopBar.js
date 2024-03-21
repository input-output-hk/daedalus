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
const react_svg_inline_1 = __importDefault(require('react-svg-inline'));
const classnames_1 = __importDefault(require('classnames'));
const mobx_react_1 = require('mobx-react');
const walletsConfig_1 = require('../../config/walletsConfig');
const LegacyBadge_1 = __importStar(require('../notifications/LegacyBadge'));
const LegacyNotification_1 = __importDefault(
  require('../notifications/LegacyNotification')
);
const TopBar_scss_1 = __importDefault(require('./TopBar.scss'));
const header_logo_inline_svg_1 = __importDefault(
  require('../../assets/images/header-logo.inline.svg')
);
const discreet_mode_1 = require('../../features/discreet-mode');
let TopBar = class TopBar extends react_1.Component {
  render() {
    const {
      onLeftIconClick,
      leftIcon,
      activeWallet,
      children,
      hasRewardsWallets,
      onTransferFunds,
      onWalletAdd,
      onLearnMore,
      isShelleyActivated,
    } = this.props;
    const topBarStyles = (0, classnames_1.default)([
      TopBar_scss_1.default.topBar,
      activeWallet
        ? TopBar_scss_1.default.withWallet
        : TopBar_scss_1.default.withoutWallet,
    ]);
    const hasLegacyNotification =
      activeWallet &&
      activeWallet.isLegacy &&
      isShelleyActivated &&
      activeWallet.amount.gt(0) &&
      !activeWallet.isRestoring &&
      ((hasRewardsWallets && onTransferFunds) || onWalletAdd);
    const onTransferFundsFn =
      onTransferFunds && activeWallet
        ? () => onTransferFunds(activeWallet.id)
        : () => {};
    const isRestoreActive = activeWallet ? activeWallet.isRestoring : false;
    const topBarTitle = activeWallet
      ? react_1.default.createElement(
          'span',
          { className: TopBar_scss_1.default.walletInfo },
          react_1.default.createElement(
            'span',
            { className: TopBar_scss_1.default.walletName },
            activeWallet.name,
            activeWallet.isLegacy &&
              react_1.default.createElement(LegacyBadge_1.default, {
                mode: LegacyBadge_1.LEGACY_BADGE_MODES.NATURAL,
              })
          ),
          react_1.default.createElement(
            'span',
            { className: TopBar_scss_1.default.walletAmount },
            // show currency and use long format
            isRestoreActive
              ? '-'
              : react_1.default.createElement(
                  discreet_mode_1.DiscreetWalletAmount,
                  { amount: activeWallet.amount }
                )
          )
        )
      : null;
    const leftIconSVG =
      leftIcon &&
      react_1.default.createElement(react_svg_inline_1.default, {
        svg: leftIcon,
        className: TopBar_scss_1.default.sidebarIcon,
      });
    return react_1.default.createElement(
      'header',
      null,
      react_1.default.createElement(
        'div',
        { className: topBarStyles },
        leftIcon &&
          react_1.default.createElement(
            'button',
            {
              className: TopBar_scss_1.default.leftIcon,
              onClick: onLeftIconClick,
            },
            leftIconSVG
          ),
        activeWallet
          ? react_1.default.createElement(
              'div',
              { className: TopBar_scss_1.default.topBarTitle },
              topBarTitle
            )
          : react_1.default.createElement(react_svg_inline_1.default, {
              svg: header_logo_inline_svg_1.default,
              className: TopBar_scss_1.default.headerLogo,
            }),
        children
      ),
      walletsConfig_1.IS_BYRON_WALLET_MIGRATION_ENABLED &&
        hasLegacyNotification &&
        activeWallet &&
        react_1.default.createElement(LegacyNotification_1.default, {
          activeWalletName: activeWallet.name,
          onLearnMore: onLearnMore,
          onTransferFunds: onTransferFundsFn,
          hasRewardsWallets: hasRewardsWallets,
          onWalletAdd: onWalletAdd,
        })
    );
  }
};
TopBar = __decorate([mobx_react_1.observer], TopBar);
exports.default = TopBar;
//# sourceMappingURL=TopBar.js.map
