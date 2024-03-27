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
// @ts-nocheck
const react_1 = __importStar(require('react'));
const react_intl_1 = require('react-intl');
const classnames_1 = __importDefault(require('classnames'));
const Input_1 = require('@react-polymorph/components/Input');
const react_svg_inline_1 = __importDefault(require('react-svg-inline'));
const mobx_react_1 = require('mobx-react');
const WalletTokensSearch_scss_1 = __importDefault(
  require('./WalletTokensSearch.scss')
);
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../../assets/images/sear... Remove this comment to see the full error message
const search_inline_svg_1 = __importDefault(
  require('../../../../assets/images/search.inline.svg')
);
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../../assets/images/clos... Remove this comment to see the full error message
const close_cross_inline_svg_1 = __importDefault(
  require('../../../../assets/images/close-cross.inline.svg')
);
const messages = (0, react_intl_1.defineMessages)({
  placeholder: {
    id: 'wallet.tokens.search.placeholder',
    defaultMessage: '!!!Search tokens',
    description: 'Search placeholder for the Wallet Tokens search',
  },
});
function WalletTokensSearch(props) {
  const { searchValue, onSearch, intl } = props;
  const [isSearchInputFocused, setSearchInputFocused] = (0, react_1.useState)(
    false
  );
  return react_1.default.createElement(
    'div',
    { className: WalletTokensSearch_scss_1.default.component },
    react_1.default.createElement(react_svg_inline_1.default, {
      svg: search_inline_svg_1.default,
      className: (0, classnames_1.default)(
        WalletTokensSearch_scss_1.default.searchIcon,
        isSearchInputFocused &&
          WalletTokensSearch_scss_1.default.searchIconFocus
      ),
    }),
    react_1.default.createElement(Input_1.Input, {
      onFocus: () => setSearchInputFocused(true),
      onBlur: () => setSearchInputFocused(false),
      onChange: onSearch,
      value: searchValue,
      placeholder: intl.formatMessage(messages.placeholder),
    }),
    !!searchValue.length &&
      react_1.default.createElement(
        'button',
        {
          className: (0, classnames_1.default)([
            WalletTokensSearch_scss_1.default.clearButton,
            'flat',
          ]),
          onClick: () => onSearch(''),
        },
        react_1.default.createElement(react_svg_inline_1.default, {
          svg: close_cross_inline_svg_1.default,
        })
      )
  );
}
exports.default = (0, react_intl_1.injectIntl)(
  (0, mobx_react_1.observer)(WalletTokensSearch)
);
//# sourceMappingURL=WalletTokensSearch.js.map
