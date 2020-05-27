// @flow
import React, { Component, Children } from 'react';
import type { Node } from 'react';
import { observable, runInAction } from 'mobx';
import { observer, inject } from 'mobx-react';
import { get } from 'lodash';
import { action } from '@storybook/addon-actions';
import { boolean } from '@storybook/addon-knobs';
import { isIncentivizedTestnetTheme } from './utils';

// Assets and helpers
import { CATEGORIES_BY_NAME } from '../../../source/renderer/app/config/sidebarConfig';
import { formattedWalletAmount } from '../../../source/renderer/app/utils/formatters';
import NodeSyncStatusIcon from '../../../source/renderer/app/components/widgets/NodeSyncStatusIcon';
import Wallet, {
  WalletSyncStateStatuses,
} from '../../../source/renderer/app/domains/Wallet.js';
import NewsFeedIcon from '../../../source/renderer/app/components/widgets/NewsFeedIcon';
import type { SidebarMenus } from '../../../source/renderer/app/components/sidebar/Sidebar';
import type {
  SidebarHardwareWalletType,
  SidebarWalletType,
} from '../../../source/renderer/app/types/sidebarTypes';

// Empty screen elements
import TopBar from '../../../source/renderer/app/components/layout/TopBar';
import Sidebar from '../../../source/renderer/app/components/sidebar/Sidebar';
import SidebarLayout from '../../../source/renderer/app/components/layout/SidebarLayout';
import menuIconOpened from '../../../source/renderer/app/assets/images/menu-opened-ic.inline.svg';
import menuIconClosed from '../../../source/renderer/app/assets/images/menu-ic.inline.svg';

export type StoriesProps = {
  wallets: Array<Wallet>,
  activeWalletId: string,
  setActiveWalletId: Function,
};

type Props = {
  activeSidebarCategory: string,
  currentTheme: string,
  storiesProps: any | StoriesProps,
  story?: string,
  children?: any | Node,
  stores?: ?{},
};

const CATEGORIES_COUNTDOWN = [
  CATEGORIES_BY_NAME.WALLETS,
  CATEGORIES_BY_NAME.HARDWARE_WALLETS,
  CATEGORIES_BY_NAME.STAKING_DELEGATION_COUNTDOWN,
  CATEGORIES_BY_NAME.SETTINGS,
];

const CATEGORIES = [
  CATEGORIES_BY_NAME.WALLETS,
  CATEGORIES_BY_NAME.HARDWARE_WALLETS,
  CATEGORIES_BY_NAME.STAKING,
  CATEGORIES_BY_NAME.SETTINGS,
];

@inject('stores', 'storiesProps')
@observer
export default class StoryLayout extends Component<Props> {
  static defaultProps = { stores: null, storiesProps: null };

  render() {
    const {
      activeSidebarCategory,
      currentTheme,
      story = '',
      storiesProps = {},
      stores,
      children,
    } = this.props;
    const { wallets, activeWalletId, setActiveWalletId } = storiesProps;

    const activeWallet: Wallet = wallets[parseInt(activeWalletId, 10)];
    const activeNavItem = story.split(' ')[0].toLowerCase();
    const sidebarMenus = this.getSidebarMenus(
      this.getSidebarWallets(wallets),
      activeWalletId,
      setActiveWalletId
    );

    return (
      <div
        style={{
          minHeight: '100%',
          height: '100vh',
        }}
      >
        <SidebarLayout
          sidebar={this.getSidebar(
            story,
            activeSidebarCategory,
            sidebarMenus,
            currentTheme
          )}
          topbar={this.getTopbar(
            activeSidebarCategory,
            activeWallet,
            activeNavItem
          )}
        >
          {Children.map(children, child =>
            React.cloneElement(child, { stores, storiesProps })
          )}
        </SidebarLayout>
      </div>
    );
  }

  @observable isShowingSubMenus =
    this.props.activeSidebarCategory === '/wallets' && !!this.props.children;

  getSidebarWallets = (
    wallets: Array<Wallet>
  ): Array<SidebarWalletType | SidebarHardwareWalletType> =>
    wallets.map((wallet: Wallet) => ({
      id: wallet.id,
      title: wallet.name,
      info: `${wallet.amount} ADA`,
      isConnected: true,
      hasPassword: wallet.hasPassword,
      isNotResponding:
        get(wallet, 'syncState.status', WalletSyncStateStatuses.READY) ===
        WalletSyncStateStatuses.NOT_RESPONDING,
      isRestoreActive:
        get(wallet, 'syncState.status', WalletSyncStateStatuses.READY) ===
        WalletSyncStateStatuses.RESTORING,
      restoreProgress: get(wallet, 'syncState.progress.quantity', 0),
      isLegacy: wallet.isLegacy,
      hasNotification: false,
    }));

  getSidebarMenus = (
    items: Array<SidebarWalletType | SidebarHardwareWalletType>,
    activeWalletId: string,
    setActiveWalletId: Function
  ) => ({
    wallets: {
      items,
      activeWalletId,
      actions: {
        onAddWallet: action('toggleAddWallet'),
        onWalletItemClick: setActiveWalletId,
      },
    },
    hardwareWallets: {
      items,
      activeWalletId,
      actions: {
        onAddWallet: action('toggleAddWallet'),
        onHardwareWalletItemClick: setActiveWalletId,
      },
    },
  });

  getSidebar = (
    story: string,
    activeSidebarCategory: string,
    sidebarMenus: SidebarMenus,
    currentTheme: string
  ) => {
    const sidebarCategories =
      story === 'Decentralization Start Info'
        ? CATEGORIES_COUNTDOWN
        : CATEGORIES;

    return (
      <Sidebar
        categories={sidebarCategories}
        activeSidebarCategory={activeSidebarCategory}
        menus={sidebarMenus}
        isShowingSubMenus={this.isShowingSubMenus}
        onActivateCategory={action('onActivateCategory')}
        isDialogOpen={() => false}
        onAddWallet={action('onAddWallet')}
        onOpenDialog={action('onOpenDialog')}
        onSubmitSupportRequest={() => {}}
        pathname="/"
        currentTheme={currentTheme}
        network="testnet"
        isIncentivizedTestnet={isIncentivizedTestnetTheme(currentTheme)}
      />
    );
  };

  getTopbar = (
    activeSidebarCategory: string,
    activeWallet: Wallet,
    activeNavItem: string
  ) => (
    <TopBar
      onToggleSidebar={() => {
        runInAction(() => {
          this.isShowingSubMenus = !this.isShowingSubMenus;
        });
      }}
      formattedWalletAmount={formattedWalletAmount}
      currentRoute={`/wallets/${activeWallet.id}/${activeNavItem}`}
      activeWallet={
        activeSidebarCategory === '/wallets' && activeNavItem !== 'empty'
          ? activeWallet
          : null
      }
      showSubMenuToggle
      showSubMenus={this.isShowingSubMenus}
      leftIcon={this.isShowingSubMenus ? menuIconOpened : menuIconClosed}
      onTransferFunds={action('onTransferFunds')}
      onWalletAdd={action('onWalletAdd')}
      hasRewardsWallets={boolean('hasRewardsWallets', true)}
    >
      <NodeSyncStatusIcon
        isSynced
        syncPercentage={100}
        isProduction
        isMainnet
      />
      <NewsFeedIcon
        onNewsFeedIconClick={action('onNewsFeedIconClick')}
        showDot={false}
      />
    </TopBar>
  );
}
