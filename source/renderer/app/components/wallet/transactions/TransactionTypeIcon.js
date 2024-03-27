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
const react_1 = __importStar(require('react'));
const classnames_1 = __importDefault(require('classnames'));
const react_svg_inline_1 = __importDefault(require('react-svg-inline'));
const LoadingSpinner_1 = require('@react-polymorph/components/LoadingSpinner');
const LoadingSpinnerSkin_1 = require('@react-polymorph/skins/simple/LoadingSpinnerSkin');
const TransactionTypeIcon_scss_1 = __importDefault(
  require('./TransactionTypeIcon.scss')
);
const SpinnerOverrides_scss_1 = __importDefault(
  require('./SpinnerOverrides.scss')
);
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/wallet-... Remove this comment to see the full error message
const send_ic_inline_svg_1 = __importDefault(
  require('../../../assets/images/wallet-nav/send-ic.inline.svg')
);
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/wallet-... Remove this comment to see the full error message
const receive_ic_inline_svg_1 = __importDefault(
  require('../../../assets/images/wallet-nav/receive-ic.inline.svg')
);
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/exchang... Remove this comment to see the full error message
const exchange_ic_inline_svg_1 = __importDefault(
  require('../../../assets/images/exchange-ic.inline.svg')
);
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/wallet-... Remove this comment to see the full error message
const pending_inline_svg_1 = __importDefault(
  require('../../../assets/images/wallet-nav/pending.inline.svg')
);
const WalletTransaction_1 = require('../../../domains/WalletTransaction');
class TransactionTypeIcon extends react_1.Component {
  applyIconStyles = (iconType) => {
    if (iconType !== WalletTransaction_1.TransactionStates.PENDING) {
      return iconType;
    }
    if (!this.props.exceedsPendingTimeLimit) {
      return `${iconType}_regular`;
    }
    return `${iconType}_warning`;
  };
  renderPendingIcon = () => {
    if (!this.props.exceedsPendingTimeLimit) {
      return this.renderPendingRegularIcon();
    }
    return this.renderPendingWarningIcon();
  };
  renderPendingRegularIcon = () =>
    react_1.default.createElement(
      'div',
      { className: TransactionTypeIcon_scss_1.default.pendingTxnIconWrapper },
      react_1.default.createElement(LoadingSpinner_1.LoadingSpinner, {
        skin: LoadingSpinnerSkin_1.LoadingSpinnerSkin,
        themeOverrides: SpinnerOverrides_scss_1.default,
      })
    );
  renderPendingWarningIcon = () =>
    react_1.default.createElement(
      'div',
      { className: TransactionTypeIcon_scss_1.default.pendingTxnIconWrapper },
      react_1.default.createElement(LoadingSpinner_1.LoadingSpinner, {
        skin: LoadingSpinnerSkin_1.LoadingSpinnerSkin,
        themeOverrides: SpinnerOverrides_scss_1.default,
      }),
      react_1.default.createElement(react_svg_inline_1.default, {
        svg: pending_inline_svg_1.default,
        className: TransactionTypeIcon_scss_1.default.pendingTxnIcon,
      })
    );
  renderFailedIcon = () => {
    return react_1.default.createElement(react_svg_inline_1.default, {
      svg: pending_inline_svg_1.default,
      className: TransactionTypeIcon_scss_1.default.transactionTypeIcon,
    });
  };
  renderIcon = (icon) => {
    let iconType;
    if (this.props.iconType === WalletTransaction_1.TransactionStates.PENDING) {
      iconType = this.renderPendingIcon();
    } else if (
      this.props.iconType === WalletTransaction_1.TransactionStates.FAILED
    ) {
      iconType = this.renderFailedIcon();
    } else {
      iconType = react_1.default.createElement(react_svg_inline_1.default, {
        svg: icon,
        className: TransactionTypeIcon_scss_1.default.transactionTypeIcon,
      });
    }
    return iconType;
  };
  render() {
    const { iconType } = this.props;
    const transactionTypeIconClasses = (0, classnames_1.default)([
      TransactionTypeIcon_scss_1.default.transactionTypeIconWrapper,
      TransactionTypeIcon_scss_1.default[this.applyIconStyles(iconType)],
    ]);
    let icon;
    switch (iconType) {
      case WalletTransaction_1.TransactionTypes.EXPEND:
        icon = send_ic_inline_svg_1.default;
        break;
      case WalletTransaction_1.TransactionTypes.INCOME:
        icon = receive_ic_inline_svg_1.default;
        break;
      case WalletTransaction_1.TransactionTypes.EXCHANGE:
        icon = exchange_ic_inline_svg_1.default;
        break;
      default:
        icon = '';
        break;
    }
    return react_1.default.createElement(
      'div',
      { className: transactionTypeIconClasses },
      this.renderIcon(icon)
    );
  }
}
exports.default = TransactionTypeIcon;
//# sourceMappingURL=TransactionTypeIcon.js.map
