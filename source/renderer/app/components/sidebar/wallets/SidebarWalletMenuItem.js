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
const classnames_1 = __importDefault(require('classnames'));
const highlight_words_1 = __importDefault(require('highlight-words'));
const react_svg_inline_1 = __importDefault(require('react-svg-inline'));
const LegacyBadge_1 = __importStar(require('../../notifications/LegacyBadge'));
const ProgressBar_1 = __importDefault(require('../../widgets/ProgressBar'));
const SidebarWalletMenuItem_scss_1 = __importDefault(
  require('./SidebarWalletMenuItem.scss')
);
const hardwareWalletsConfig_1 = require('../../../config/hardwareWalletsConfig');
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/hardwar... Remove this comment to see the full error message
const connect_ic_inline_svg_1 = __importDefault(
  require('../../../assets/images/hardware-wallet/connect-ic.inline.svg')
);
const discreet_mode_1 = require('../../../features/discreet-mode');
let SidebarWalletMenuItem = class SidebarWalletMenuItem extends react_1.Component {
  render() {
    const {
      title,
      amount,
      active,
      className,
      onClick,
      isRestoreActive,
      isShelleyActivated,
      restoreProgress,
      isLegacy,
      isNotResponding,
      hasNotification,
      isHardwareWalletDisconnected,
      isHardwareWallet,
      searchValue,
    } = this.props;
    const showLegacyBadge = isLegacy && isShelleyActivated;
    const componentStyles = (0, classnames_1.default)([
      SidebarWalletMenuItem_scss_1.default.component,
      active ? SidebarWalletMenuItem_scss_1.default.active : null,
      showLegacyBadge ? SidebarWalletMenuItem_scss_1.default.legacyItem : null,
      className,
      hasNotification
        ? SidebarWalletMenuItem_scss_1.default.notification
        : null,
      isNotResponding
        ? SidebarWalletMenuItem_scss_1.default.notResponding
        : null,
    ]);
    const hwIconStyles = (0, classnames_1.default)([
      SidebarWalletMenuItem_scss_1.default.hardwareWalletsIcon,
      isHardwareWallet &&
      isHardwareWalletDisconnected &&
      hardwareWalletsConfig_1.isHardwareWalletIndicatorEnabled
        ? SidebarWalletMenuItem_scss_1.default.disconnected
        : null,
    ]);
    const chunks = (0, highlight_words_1.default)({
      text: title,
      query: `/(${searchValue.split('').join('|')})/i`,
    });
    return react_1.default.createElement(
      'button',
      {
        className: componentStyles,
        onClick: onClick,
        'data-testid': 'walletMenu',
      },
      react_1.default.createElement(
        'div',
        { className: SidebarWalletMenuItem_scss_1.default.meta },
        react_1.default.createElement(
          'div',
          { className: SidebarWalletMenuItem_scss_1.default.topContainer },
          react_1.default.createElement(
            'div',
            {
              className: SidebarWalletMenuItem_scss_1.default.title,
              'data-testid': title,
            },
            chunks.map(({ text, match, key }) =>
              react_1.default.createElement(
                'span',
                {
                  key: key,
                  className: match
                    ? SidebarWalletMenuItem_scss_1.default.searchMatch
                    : SidebarWalletMenuItem_scss_1.default.searchUnmatch,
                },
                text
              )
            )
          ),
          isHardwareWallet &&
            react_1.default.createElement(
              'div',
              {
                className:
                  SidebarWalletMenuItem_scss_1.default
                    .hardwareWalletsIconWrapper,
              },
              react_1.default.createElement(react_svg_inline_1.default, {
                svg: connect_ic_inline_svg_1.default,
                className: hwIconStyles,
              })
            )
        ),
        react_1.default.createElement(
          'div',
          { className: SidebarWalletMenuItem_scss_1.default.info },
          isRestoreActive
            ? '-'
            : // @ts-ignore ts-migrate(2322) FIXME: Type 'number' is not assignable to type 'BigNumber... Remove this comment to see the full error message
              react_1.default.createElement(
                discreet_mode_1.DiscreetWalletAmount,
                { amount: amount, withCurrency: true, long: false }
              )
        ),
        isRestoreActive
          ? react_1.default.createElement(ProgressBar_1.default, {
              progress: restoreProgress,
            })
          : null,
        showLegacyBadge &&
          react_1.default.createElement(LegacyBadge_1.default, {
            mode: LegacyBadge_1.LEGACY_BADGE_MODES.FLOATING,
          })
      )
    );
  }
};
SidebarWalletMenuItem = __decorate(
  [mobx_react_1.observer],
  SidebarWalletMenuItem
);
exports.default = SidebarWalletMenuItem;
//# sourceMappingURL=SidebarWalletMenuItem.js.map
