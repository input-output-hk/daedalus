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
exports.messages = void 0;
const react_1 = __importStar(require('react'));
const mobx_react_1 = require('mobx-react');
const classnames_1 = __importDefault(require('classnames'));
const react_intl_1 = require('react-intl');
const react_svg_inline_1 = __importDefault(require('react-svg-inline'));
const FilterButton_1 = __importDefault(require('./FilterButton'));
const FilterDialog_1 = __importDefault(require('./FilterDialog'));
const WalletTransactionsHeader_scss_1 = __importDefault(
  require('./WalletTransactionsHeader.scss')
);
const TinyButton_1 = __importDefault(require('../../widgets/forms/TinyButton'));
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/download... Remove this comment to see the full error message
const download_icon_inline_svg_1 = __importDefault(
  require('../../../assets/images/download-icon.inline.svg')
);
exports.messages = (0, react_intl_1.defineMessages)({
  transactions: {
    id: 'wallet.transactions.header.transactions',
    defaultMessage: '!!!Transactions',
    description: 'Label for the "Transactions" header.',
  },
  exportCSVButtonLabel: {
    id: 'wallet.transactions.header.exportCSV.button.label',
    defaultMessage: '!!!Export CSV',
    description: 'Label for the "Export CSV" button.',
  },
});
let WalletTransactionsHeader = class WalletTransactionsHeader extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  render() {
    const { intl } = this.context;
    const {
      isFilterDisabled,
      isScrolling,
      filterDialogProps,
      numberOfFilterDimensionsApplied,
      numberOfTransactions,
      onRequestCSVFile,
    } = this.props;
    const hasAny = true;
    const componentClassnames = (0, classnames_1.default)([
      WalletTransactionsHeader_scss_1.default.component,
      isScrolling ? WalletTransactionsHeader_scss_1.default.isScrolling : null,
    ]);
    const isCsvButtonDisabled = numberOfTransactions === 0;
    const cvsButtonClassnames = (0, classnames_1.default)([
      WalletTransactionsHeader_scss_1.default.csvButton,
      isCsvButtonDisabled
        ? WalletTransactionsHeader_scss_1.default.csvButtonDisabled
        : null,
      'flat',
    ]);
    return react_1.default.createElement(
      'div',
      { className: componentClassnames },
      react_1.default.createElement(
        'div',
        {
          className:
            WalletTransactionsHeader_scss_1.default.numberOfTransactions,
        },
        intl.formatMessage(exports.messages.transactions),
        ' (',
        numberOfTransactions,
        ')'
      ),
      hasAny &&
        react_1.default.createElement(
          'div',
          { className: WalletTransactionsHeader_scss_1.default.actions },
          react_1.default.createElement(TinyButton_1.default, {
            label: react_1.default.createElement(
              react_1.Fragment,
              null,
              intl.formatMessage(exports.messages.exportCSVButtonLabel),
              react_1.default.createElement(react_svg_inline_1.default, {
                svg: download_icon_inline_svg_1.default,
                className: WalletTransactionsHeader_scss_1.default.downloadIcon,
              })
            ),
            onClick: onRequestCSVFile,
            containerClassName:
              WalletTransactionsHeader_scss_1.default.csvButtonContainer,
            className: cvsButtonClassnames,
            disabled: isCsvButtonDisabled,
            loading: false,
          }),
          react_1.default.createElement(FilterDialog_1.default, {
            ...filterDialogProps,
            triggerElement: react_1.default.createElement(
              FilterButton_1.default,
              {
                disabled: isFilterDisabled,
                numberOfFilterDimensionsApplied: numberOfFilterDimensionsApplied,
              }
            ),
          })
        )
    );
  }
};
WalletTransactionsHeader = __decorate(
  [mobx_react_1.observer],
  WalletTransactionsHeader
);
exports.default = WalletTransactionsHeader;
//# sourceMappingURL=WalletTransactionsHeader.js.map
