// @flow
import React, { Component, Children } from 'react';
import type { Node } from 'react';
import { observable, runInAction } from 'mobx';
import { observer, inject } from 'mobx-react';
import { action } from '@storybook/addon-actions';

// Assets and helpers
import { CATEGORIES_BY_NAME } from '../../../source/renderer/app/config/sidebarConfig';
import { formattedWalletAmount } from '../../../source/renderer/app/utils/formatters';
import NodeSyncStatusIcon from '../../../source/renderer/app/components/widgets/NodeSyncStatusIcon';
import Wallet from '../../../source/renderer/app/domains/Wallet.js';
import type { SidebarMenus } from '../../../source/renderer/app/components/sidebar/Sidebar';
import type { SidebarWalletType } from '../../../source/renderer/app/types/sidebarTypes';
// import type { Wallet } from '../../../source/renderer/app/domains/WalletTransaction';

// Empty screen elements
import TopBar from '../../../source/renderer/app/components/layout/TopBar';
import Sidebar from '../../../source/renderer/app/components/sidebar/Sidebar';
import SidebarLayout from '../../../source/renderer/app/components/layout/SidebarLayout';

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

const sidebarCategories = [
  CATEGORIES_BY_NAME.WALLETS,
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
          sidebar={this.getSidebar(activeSidebarCategory, sidebarMenus)}
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

  @observable
  isShowingSubMenus =
    this.props.activeSidebarCategory === '/wallets' && !!this.props.children;

  getSidebarWallets = (wallets: Array<Wallet>): Array<SidebarWalletType> =>
    wallets.map((wallet: Wallet) => ({
      id: wallet.id,
      title: wallet.name,
      info: `${wallet.amount} ADA`,
      isConnected: true,
      isRestoreActive: false,
      restoreProgress: 0,
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

  getSidebar = (activeSidebarCategory: string, sidebarMenus: SidebarMenus) => (
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
    />
  );

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
    >
      <NodeSyncStatusIcon
        networkStatus={{
          isSynced: true,
          syncPercentage: 100,
        }}
        isProduction
        isMainnet
      />
    </TopBar>
  );
}
