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
const classnames_1 = __importDefault(require('classnames'));
const mobx_react_1 = require('mobx-react');
const WalletNavigation_1 = __importDefault(
  require('../navigation/WalletNavigation')
);
const WalletWithNavigation_scss_1 = __importDefault(
  require('./WalletWithNavigation.scss')
);
const NotResponding_1 = __importDefault(
  require('../not-responding/NotResponding')
);
const SetWalletPassword_1 = __importDefault(
  require('../settings/SetWalletPassword')
);
let WalletWithNavigation = class WalletWithNavigation extends react_1.Component {
  render() {
    const {
      children,
      activeItem,
      hasNotification,
      hasPassword,
      isActiveScreen,
      isLegacy,
      isNotResponding,
      isHardwareWallet,
      isSetWalletPasswordDialogOpen,
      onOpenExternalLink,
      onRestartNode,
      onSetWalletPassword,
      onWalletNavItemClick,
    } = this.props;
    const componentStyles = (0, classnames_1.default)([
      WalletWithNavigation_scss_1.default.component,
      WalletWithNavigation_scss_1.default[activeItem],
    ]);
    return react_1.default.createElement(
      'div',
      { className: componentStyles },
      react_1.default.createElement(
        'div',
        { className: WalletWithNavigation_scss_1.default.navigation },
        react_1.default.createElement(WalletNavigation_1.default, {
          isActiveNavItem: isActiveScreen,
          isLegacy: isLegacy,
          onNavItemClick: onWalletNavItemClick,
          activeItem: activeItem,
          hasNotification: hasNotification,
        })
      ),
      react_1.default.createElement(
        'div',
        { className: WalletWithNavigation_scss_1.default.page },
        children
      ),
      !hasPassword &&
        !isHardwareWallet &&
        react_1.default.createElement(SetWalletPassword_1.default, {
          isSetWalletPasswordDialogOpen: isSetWalletPasswordDialogOpen,
          onSetWalletPassword: onSetWalletPassword,
        }),
      isNotResponding &&
        react_1.default.createElement(NotResponding_1.default, {
          walletName: activeItem,
          onRestartNode: onRestartNode,
          onOpenExternalLink: onOpenExternalLink,
        })
    );
  }
};
WalletWithNavigation = __decorate(
  [mobx_react_1.observer],
  WalletWithNavigation
);
exports.default = WalletWithNavigation;
//# sourceMappingURL=WalletWithNavigation.js.map
