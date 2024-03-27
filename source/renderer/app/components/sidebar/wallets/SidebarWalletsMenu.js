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
const mobx_react_1 = require('mobx-react');
const react_intl_1 = require('react-intl');
const classnames_1 = __importDefault(require('classnames'));
const react_custom_scrollbars_1 = require('react-custom-scrollbars');
const lodash_1 = require('lodash');
const fuse_js_1 = __importDefault(require('fuse.js'));
const sidebarConfig_1 = require('../../../config/sidebarConfig');
const SidebarMenu_1 = __importDefault(require('../SidebarMenu'));
const SidebarWalletsMenu_scss_1 = __importDefault(
  require('./SidebarWalletsMenu.scss')
);
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/sidebar... Remove this comment to see the full error message
const add_wallet_ic_inline_svg_1 = __importDefault(
  require('../../../assets/images/sidebar/add-wallet-ic.inline.svg')
);
const SidebarWalletMenuItem_1 = __importDefault(
  require('./SidebarWalletMenuItem')
);
const sidebarTypes_1 = require('../../../types/sidebarTypes');
const WalletSortButton_1 = require('./WalletSortButton');
const WalletSearch_1 = require('./WalletSearch');
const messages = (0, react_intl_1.defineMessages)({
  addAdaWallet: {
    id: 'sidebar.wallets.addWallet',
    defaultMessage: '!!!Add wallet',
    description: 'Label for the "Add wallet" button in wallet sidebar menu.',
  },
  sortByDateButton: {
    id: 'sidebar.wallets.sortByDateButton',
    defaultMessage: '!!!Date',
    description: 'Label for the "Date" sort button',
  },
  sortByDateTooltip: {
    id: 'sidebar.wallets.sortByDateTooltip',
    defaultMessage: '!!!Sort wallets by creation date',
    description: 'Tooltip message for Date sort button',
  },
  sortByBalanceButton: {
    id: 'sidebar.wallets.sortByBalanceButton',
    defaultMessage: '!!!Balance',
    description: 'Label for the "Balance" sort button',
  },
  sortByBalanceTooltip: {
    id: 'sidebar.wallets.sortByBalanceTooltip',
    defaultMessage: '!!!Sort wallets by balance',
    description: 'Tooltip message for Balance sort button',
  },
  sortByNameButton: {
    id: 'sidebar.wallets.sortByNameButton',
    defaultMessage: '!!!A – Z',
    description: 'Label for the "Name" sort button',
  },
  sortByNameTooltip: {
    id: 'sidebar.wallets.sortByNameTooltip',
    defaultMessage: '!!!Sort wallets by name',
    description: 'Tooltip message for Name sort button',
  },
});
let SidebarWalletsMenu = class SidebarWalletsMenu extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  renderThumb = (props) =>
    react_1.default.createElement('div', {
      ...props,
      className: SidebarWalletsMenu_scss_1.default.scrollbarThumb,
    });
  walletSort = (sortBy) => {
    const {
      sortOrder = sidebarTypes_1.WalletSortOrder.asc,
      onWalletSortBy = lodash_1.noop,
    } = this.props;
    return onWalletSortBy({
      sortBy,
      sortOrder,
    });
  };
  filterWalletsBySearchValue = (searchValue, wallets) => {
    if (searchValue.length > 0) {
      const fuse = new fuse_js_1.default(
        wallets.map((w) => ({
          ...w,
          // fix encode issue on fuse lib related to empty space
          // https://github.com/krisk/Fuse/issues/610
          title: w.title.replace(/\s/g, ' '),
        })),
        {
          keys: ['title'],
          includeScore: true,
          ignoreLocation: true,
          threshold: sidebarConfig_1.FUZZY_SEARCH_THRESHOLD,
        }
      );
      return fuse.search(searchValue).map((r) => r.item);
    }
    return wallets;
  };
  render() {
    const { intl } = this.context;
    const {
      wallets,
      onAddWallet,
      isActiveWallet,
      onWalletItemClick,
      isAddWalletButtonActive,
      isShelleyActivated,
      sortBy = sidebarTypes_1.WalletSortBy.Date,
      sortOrder = sidebarTypes_1.WalletSortOrder.Asc,
      searchValue = '',
      onSearch = lodash_1.noop,
    } = this.props;
    const addWalletButtonStyles = (0, classnames_1.default)([
      SidebarWalletsMenu_scss_1.default.addWalletButton,
      isAddWalletButtonActive ? SidebarWalletsMenu_scss_1.default.active : null,
    ]);
    const filteredWallets = this.filterWalletsBySearchValue(
      searchValue,
      wallets
    );
    return react_1.default.createElement(
      SidebarMenu_1.default,
      { visible: this.props.visible },
      react_1.default.createElement(
        'div',
        { className: SidebarWalletsMenu_scss_1.default.walletSearchContainer },
        react_1.default.createElement(WalletSearch_1.WalletSearch, {
          searchValue: searchValue,
          onSearch: onSearch,
        })
      ),
      react_1.default.createElement(
        'div',
        { className: SidebarWalletsMenu_scss_1.default.wallets },
        react_1.default.createElement(
          react_custom_scrollbars_1.Scrollbars,
          {
            renderThumbHorizontal: () =>
              react_1.default.createElement('div', {
                className: SidebarWalletsMenu_scss_1.default.hideThumb,
              }),
            renderThumbVertical: this.renderThumb,
            hideTracksWhenNotNeeded: true,
          },
          (0, lodash_1.map)(filteredWallets, (wallet) =>
            react_1.default.createElement(SidebarWalletMenuItem_1.default, {
              title: wallet.title,
              // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
              amount: wallet.amount,
              active: isActiveWallet(wallet.id),
              onClick: () => onWalletItemClick(wallet.id),
              key: wallet.id,
              className: `Wallet_${wallet.id}`,
              isRestoreActive: wallet.isRestoreActive,
              isShelleyActivated: isShelleyActivated,
              restoreProgress: wallet.restoreProgress,
              isNotResponding: wallet.isNotResponding,
              isLegacy: wallet.isLegacy,
              // @ts-ignore ts-migrate(2339) FIXME: Property 'isHardwareWallet' does not exist on type... Remove this comment to see the full error message
              isHardwareWallet: wallet.isHardwareWallet,
              hasNotification: wallet.hasNotification,
              searchValue: searchValue,
              isHardwareWalletDisconnected:
                // @ts-ignore ts-migrate(2339) FIXME: Property 'isHardwareWalletDisconnected' does not e... Remove this comment to see the full error message
                wallet.isHardwareWalletDisconnected,
            })
          )
        )
      ),
      react_1.default.createElement(
        'div',
        { className: SidebarWalletsMenu_scss_1.default.walletSortControls },
        react_1.default.createElement(
          'div',
          { className: SidebarWalletsMenu_scss_1.default.walletSortOffset },
          react_1.default.createElement(WalletSortButton_1.WalletSortButton, {
            isActive: sortBy === sidebarTypes_1.WalletSortBy.Date,
            sortOrder: sortOrder,
            onClick: () => this.walletSort(sidebarTypes_1.WalletSortBy.Date),
            label: intl.formatMessage(messages.sortByDateButton),
            tooltip: intl.formatMessage(messages.sortByDateTooltip),
          })
        ),
        react_1.default.createElement(
          'div',
          { className: SidebarWalletsMenu_scss_1.default.walletSortOffset },
          react_1.default.createElement(WalletSortButton_1.WalletSortButton, {
            isActive: sortBy === sidebarTypes_1.WalletSortBy.Balance,
            sortOrder: sortOrder,
            onClick: () => this.walletSort(sidebarTypes_1.WalletSortBy.Balance),
            label: intl.formatMessage(messages.sortByBalanceButton),
            tooltip: intl.formatMessage(messages.sortByBalanceTooltip),
          })
        ),
        react_1.default.createElement(WalletSortButton_1.WalletSortButton, {
          isActive: sortBy === sidebarTypes_1.WalletSortBy.Name,
          sortOrder: sortOrder,
          onClick: () => this.walletSort(sidebarTypes_1.WalletSortBy.Name),
          label: intl.formatMessage(messages.sortByNameButton),
          tooltip: intl.formatMessage(messages.sortByNameTooltip),
        })
      ),
      react_1.default.createElement(
        'button',
        { className: addWalletButtonStyles, onClick: onAddWallet },
        react_1.default.createElement(react_svg_inline_1.default, {
          svg: add_wallet_ic_inline_svg_1.default,
          className: SidebarWalletsMenu_scss_1.default.icon,
        }),
        react_1.default.createElement(
          'span',
          null,
          intl.formatMessage(messages.addAdaWallet)
        )
      )
    );
  }
};
SidebarWalletsMenu = __decorate([mobx_react_1.observer], SidebarWalletsMenu);
exports.default = SidebarWalletsMenu;
//# sourceMappingURL=SidebarWalletsMenu.js.map
