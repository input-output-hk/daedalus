import React, { Component } from 'react';
import SVGInline from 'react-svg-inline';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import classNames from 'classnames';
import { Scrollbars } from 'react-custom-scrollbars';
import { map, noop } from 'lodash';
import Fuse from 'fuse.js';
import { FUZZY_SEARCH_THRESHOLD } from '../../../config/sidebarConfig';
import SidebarSubMenu from '../SidebarMenu';
import styles from './SidebarWalletsMenu.scss';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/sidebar... Remove this comment to see the full error message
import addWalletIcon from '../../../assets/images/sidebar/add-wallet-ic.inline.svg';
import SidebarWalletMenuItem from './SidebarWalletMenuItem';
import type {
  SidebarWalletType,
  WalletSortByOptions,
  WalletSortOrderOptions,
} from '../../../types/sidebarTypes';
import { WalletSortBy, WalletSortOrder } from '../../../types/sidebarTypes';
import { WalletSortButton } from './WalletSortButton';
import { WalletSearch } from './WalletSearch';

const messages = defineMessages({
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
type Props = {
  wallets: Array<SidebarWalletType>;
  isActiveWallet: (...args: Array<any>) => any;
  onAddWallet: (...args: Array<any>) => any;
  onWalletItemClick: (...args: Array<any>) => any;
  visible: boolean;
  isAddWalletButtonActive: boolean;
  isShelleyActivated: boolean;
  onWalletSortBy?: (arg0: {
    sortBy: WalletSortByOptions;
    sortOrder: WalletSortOrderOptions;
  }) => void;
  sortBy?: WalletSortByOptions;
  sortOrder?: WalletSortOrderOptions;
  searchValue?: string;
  onSearch?: (term: string) => void;
};

@observer
class SidebarWalletsMenu extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };
  renderThumb = (props: any) => (
    <div {...props} className={styles.scrollbarThumb} />
  );
  walletSort = (sortBy: WalletSortByOptions) => {
    const {
      sortOrder = WalletSortOrder.asc,
      onWalletSortBy = noop,
    } = this.props;
    return onWalletSortBy({
      sortBy,
      sortOrder,
    });
  };
  filterWalletsBySearchValue = (
    searchValue: string,
    wallets: SidebarWalletType[]
  ): SidebarWalletType[] => {
    if (searchValue.length > 0) {
      const fuse = new Fuse(
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
          threshold: FUZZY_SEARCH_THRESHOLD,
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
      sortBy = WalletSortBy.Date,
      sortOrder = WalletSortOrder.Asc,
      searchValue = '',
      onSearch = noop,
    } = this.props;
    const addWalletButtonStyles = classNames([
      styles.addWalletButton,
      isAddWalletButtonActive ? styles.active : null,
    ]);
    const filteredWallets = this.filterWalletsBySearchValue(
      searchValue,
      wallets
    );
    return (
      <SidebarSubMenu visible={this.props.visible}>
        <div className={styles.walletSearchContainer}>
          <WalletSearch searchValue={searchValue} onSearch={onSearch} />
        </div>
        <div className={styles.wallets}>
          <Scrollbars
            renderThumbHorizontal={() => <div className={styles.hideThumb} />}
            renderThumbVertical={this.renderThumb}
            hideTracksWhenNotNeeded
          >
            {map(filteredWallets, (wallet) => (
              <SidebarWalletMenuItem
                title={wallet.title}
                // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
                amount={wallet.amount}
                active={isActiveWallet(wallet.id)}
                onClick={() => onWalletItemClick(wallet.id)}
                key={wallet.id}
                className={`Wallet_${wallet.id}`}
                isRestoreActive={wallet.isRestoreActive}
                isShelleyActivated={isShelleyActivated}
                restoreProgress={wallet.restoreProgress}
                isNotResponding={wallet.isNotResponding}
                isLegacy={wallet.isLegacy}
                // @ts-ignore ts-migrate(2339) FIXME: Property 'isHardwareWallet' does not exist on type... Remove this comment to see the full error message
                isHardwareWallet={wallet.isHardwareWallet}
                hasNotification={wallet.hasNotification}
                searchValue={searchValue}
                isHardwareWalletDisconnected={
                  // @ts-ignore ts-migrate(2339) FIXME: Property 'isHardwareWalletDisconnected' does not e... Remove this comment to see the full error message
                  wallet.isHardwareWalletDisconnected
                }
              />
            ))}
          </Scrollbars>
        </div>
        <div className={styles.walletSortControls}>
          <div className={styles.walletSortOffset}>
            <WalletSortButton
              isActive={sortBy === WalletSortBy.Date}
              sortOrder={sortOrder}
              onClick={() => this.walletSort(WalletSortBy.Date)}
              label={intl.formatMessage(messages.sortByDateButton)}
              tooltip={intl.formatMessage(messages.sortByDateTooltip)}
            />
          </div>
          <div className={styles.walletSortOffset}>
            <WalletSortButton
              isActive={sortBy === WalletSortBy.Balance}
              sortOrder={sortOrder}
              onClick={() => this.walletSort(WalletSortBy.Balance)}
              label={intl.formatMessage(messages.sortByBalanceButton)}
              tooltip={intl.formatMessage(messages.sortByBalanceTooltip)}
            />
          </div>
          <WalletSortButton
            isActive={sortBy === WalletSortBy.Name}
            sortOrder={sortOrder}
            onClick={() => this.walletSort(WalletSortBy.Name)}
            label={intl.formatMessage(messages.sortByNameButton)}
            tooltip={intl.formatMessage(messages.sortByNameTooltip)}
          />
        </div>
        <button className={addWalletButtonStyles} onClick={onAddWallet}>
          <SVGInline svg={addWalletIcon} className={styles.icon} />
          <span>{intl.formatMessage(messages.addAdaWallet)}</span>
        </button>
      </SidebarSubMenu>
    );
  }
}

export default SidebarWalletsMenu;
