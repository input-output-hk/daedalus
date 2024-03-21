'use strict';
// @ts-nocheck
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.WalletSearch = void 0;
const react_1 = __importDefault(require('react'));
const react_intl_1 = require('react-intl');
const fp_1 = require('lodash/fp');
const Input_1 = require('@react-polymorph/components/Input');
const react_svg_inline_1 = __importDefault(require('react-svg-inline'));
const mobx_react_1 = require('mobx-react');
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/search.... Remove this comment to see the full error message
const search_inline_svg_1 = __importDefault(
  require('../../../assets/images/search.inline.svg')
);
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/close-c... Remove this comment to see the full error message
const close_cross_inline_svg_1 = __importDefault(
  require('../../../assets/images/close-cross.inline.svg')
);
const WalletSearch_scss_1 = __importDefault(require('./WalletSearch.scss'));
const messages = (0, react_intl_1.defineMessages)({
  placeholder: {
    id: 'sidebar.wallets.search.placeholder',
    defaultMessage: '!!!Filter',
    description: 'Search placeholder for the sidebar wallet menu',
  },
});
function WalletSearchComponent({
  searchValue = '',
  onSearch = fp_1.noop,
  intl,
}) {
  const hasValue = !!searchValue.length;
  return react_1.default.createElement(
    'label',
    {
      htmlFor: 'sidebarWalletSearch',
      className: WalletSearch_scss_1.default.component,
    },
    react_1.default.createElement(react_svg_inline_1.default, {
      svg: search_inline_svg_1.default,
      className: WalletSearch_scss_1.default.searchIcon,
    }),
    react_1.default.createElement(Input_1.Input, {
      id: 'sidebarWalletSearch',
      className: WalletSearch_scss_1.default.input,
      onChange: onSearch,
      spellCheck: false,
      value: searchValue,
      placeholder: intl.formatMessage(messages.placeholder),
    }),
    hasValue &&
      react_1.default.createElement(
        'button',
        {
          className: WalletSearch_scss_1.default.clearButton,
          onClick: () => onSearch(''),
        },
        react_1.default.createElement(react_svg_inline_1.default, {
          svg: close_cross_inline_svg_1.default,
        })
      )
  );
}
exports.WalletSearch = (0, react_intl_1.injectIntl)(
  (0, mobx_react_1.observer)(WalletSearchComponent)
);
//# sourceMappingURL=WalletSearch.js.map
