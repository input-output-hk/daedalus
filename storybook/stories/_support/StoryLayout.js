// @flow
import React, { Component, Children } from 'react';
import type { Node } from 'react';
import { observable, runInAction } from 'mobx';
import { observer, inject } from 'mobx-react';
import { get } from 'lodash';
import { action } from '@storybook/addon-actions';

// Assets and helpers
import { CATEGORIES_BY_NAME } from '../../../source/renderer/app/config/sidebarConfig';
import NodeSyncStatusIcon from '../../../source/renderer/app/components/widgets/NodeSyncStatusIcon';
import Wallet from '../../../source/renderer/app/domains/Wallet.js';
import NewsFeedIcon from '../../../source/renderer/app/components/widgets/NewsFeedIcon';
import type { SidebarMenus } from '../../../source/renderer/app/components/sidebar/Sidebar';
import type { SidebarWalletType } from '../../../source/renderer/app/types/sidebarTypes';

// Empty screen elements
import TopBar from '../../../source/renderer/app/components/layout/TopBar';
import Sidebar from '../../../source/renderer/app/components/sidebar/Sidebar';
import SidebarLayout from '../../../source/renderer/app/components/layout/SidebarLayout';
import menuIconOpened from '../../../source/renderer/app/assets/images/menu-opened-ic.inline.svg';
import menuIconClosed from '../../../source/renderer/app/assets/images/menu-ic.inline.svg';
import { WalletRecoveryPhraseVerificationStatuses } from '../../../source/renderer/app/stores/WalletsStore';

export type StoriesProps = {
  wallets: Array<Wallet>,
  activeWalletId: string,
  setActiveWalletId: Function,
};

type Props = {
  activeSidebarCategory: string,
  storiesProps: any | StoriesProps,
  storyName?: string,
  children?: any | Node,
  stores?: ?{},
};

const CATEGORIES_COUNTDOWN = [
  CATEGORIES_BY_NAME.WALLETS,
  CATEGORIES_BY_NAME.STAKING_DELEGATION_COUNTDOWN,
  CATEGORIES_BY_NAME.SETTINGS,
];

const CATEGORIES = [
  CATEGORIES_BY_NAME.WALLETS,
  CATEGORIES_BY_NAME.STAKING,
  CATEGORIES_BY_NAME.SETTINGS,
];

let currentTheme = sessionStorage.getItem('themeName') || 'light-blue';
currentTheme = currentTheme.toLowerCase();

@inject('stores', 'storiesProps')
@observer
export default class StoryLayout extends Component<Props> {
  static defaultProps = { stores: null, storiesProps: null };

  render() {
    const {
      activeSidebarCategory,
      storyName = '',
      storiesProps = {},
      stores,
      children,
    } = this.props;

    const { wallets, activeWalletId, setActiveWalletId } = storiesProps;

    const activeWallet: Wallet = wallets[parseInt(activeWalletId, 10)];
    const activeNavItem = storyName.split(' ')[0].toLowerCase();
    const sidebarMenus = this.getSidebarMenus(
      this.getSidebarWallets(wallets),
      activeWalletId,
      setActiveWalletId
    );

    return (
      <div
        style={{
          height: '100vh',
        }}
      >
        <SidebarLayout
          sidebar={this.getSidebar(
            storyName,
            activeSidebarCategory,
            sidebarMenus
          )}
          topbar={this.getTopbar(
            activeSidebarCategory,
            activeWallet,
            activeNavItem
          )}
        >
          {Children.map(children, child =>
            React.cloneElement(child, { stores })
          )}
        </SidebarLayout>
      </div>
    );
  }

  @observable isShowingSubMenus =
    this.props.activeSidebarCategory === '/wallets' && !!this.props.children;

  getSidebarWallets = (wallets: Array<Wallet>): Array<SidebarWalletType> =>
    wallets.map((wallet: Wallet) => ({
      id: wallet.id,
      title: wallet.name,
      info: `${wallet.amount} ADA`,
      isConnected: true,
      isRestoreActive: get(wallet, 'syncState.tag', 'synced') === 'restoring',
      restoreProgress: get(wallet, 'syncState.data.percentage.quantity', 0),
      isLegacy: wallet.isLegacy,
      recoveryPhraseVerificationStatus:
        WalletRecoveryPhraseVerificationStatuses.OK,
    }));

  getSidebarMenus = (
    items: Array<SidebarWalletType>,
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
  });

  getSidebar = (
    storyName: string,
    activeSidebarCategory: string,
    sidebarMenus: SidebarMenus
  ) => {
    const sidebarCategories =
      storyName === 'Decentralization Start Info'
        ? CATEGORIES_COUNTDOWN
        : CATEGORIES;

    return (
      <Sidebar
        categories={sidebarCategories}
        activeSidebarCategory={activeSidebarCategory}
        menus={sidebarMenus}
        isShowingSubMenus={this.isShowingSubMenus}
        onCategoryClicked={action('onCategoryClicked')}
        isDialogOpen={() => false}
        onAddWallet={action('onAddWallet')}
        openDialogAction={action('openDialog')}
        onSubmitSupportRequest={() => {}}
        pathname="/"
        currentTheme={currentTheme}
      />
    );
  };

  getTopbar = (
    activeSidebarCategory: string,
    activeWallet: Wallet,
    activeNavItem: string
  ) => (
    <TopBar
      onLeftIconClick={() => {
        runInAction(() => {
          this.isShowingSubMenus = !this.isShowingSubMenus;
        });
      }}
      activeWallet={
        activeSidebarCategory === '/wallets' && activeNavItem !== 'empty'
          ? activeWallet
          : null
      }
      leftIcon={this.isShowingSubMenus ? menuIconOpened : menuIconClosed}
    >
      <NodeSyncStatusIcon
        networkStatus={{
          isSynced: true,
          syncPercentage: 100,
        }}
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
