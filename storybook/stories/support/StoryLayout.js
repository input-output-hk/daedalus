// @flow
import React, { Component } from 'react';
import type { Node } from 'react';
import { observable, runInAction } from 'mobx';
import { observer, inject } from 'mobx-react';
import { linkTo } from '@storybook/addon-links';
import { action } from '@storybook/addon-actions';
import startCase from 'lodash/startCase';

// Assets and helpers
import { formattedWalletAmount } from '../../../source/renderer/app/utils/ada/formatters';
import NodeSyncStatusIcon from '../../../source/renderer/app/components/widgets/NodeSyncStatusIcon';
import Wallet from '../../../source/renderer/app/domains/Wallet.js';
import type { SidebarMenus, SidebarCategories } from '../../../source/renderer/app/components/sidebar/Sidebar';

// Empty screen elements
import TopBar from '../../../source/renderer/app/components/layout/TopBar';
import Sidebar from '../../../source/renderer/app/components/sidebar/Sidebar';
import SidebarLayout from '../../../source/renderer/app/components/layout/SidebarLayout';
import WalletWithNavigation from '../../../source/renderer/app/components/wallet/layouts/WalletWithNavigation';

type StoriesProps = {
  wallets: Array<Wallet>,
  activeWalletId: number,
  sidebarMenus: SidebarMenus,
  sidebarCategories: Array<any>
}

type Props = {
  storiesProps: any | StoriesProps,
  storyName?: string,
  children?: any | Node,
};


@inject('actions', 'stores', 'storiesProps') @observer
export default class StoryLayout extends Component<Props> {

  static defaultProps = { actions: null, stores: null, storiesProps: null };

  render() {

    const {
      children,
      storyName = '',
      storiesProps = {}
    } = this.props;

    const {
      wallets,
      activeWalletId,
      sidebarMenus,
      sidebarCategories,
    } = storiesProps;

    const activeWallet: Wallet = wallets[activeWalletId];
    const activeNavItem = storyName.split(' ')[0].toLowerCase();

    return (
      <div
        style={{
          height: '100vh'
        }}
      >
        <SidebarLayout
          sidebar={this.getSidebar(sidebarMenus, sidebarCategories)}
          topbar={this.getTopbar(activeWallet, activeNavItem)}
        >
          {
            storyName !== 'Empty' &&
              (
                <WalletWithNavigation
                  isActiveScreen={item => item === activeNavItem}
                  onWalletNavItemClick={linkTo('WalletScreens', item => startCase(item))}
                >
                  {children}
                </WalletWithNavigation>
              )
          }
        </SidebarLayout>
      </div>
    );
  }

  @observable
  isShowingSubMenus = !!this.props.children;

  getSidebar = (sidebarMenus: SidebarMenus, sidebarCategories: SidebarCategories) => (
    <Sidebar
      categories={sidebarCategories}
      activeSidebarCategory={sidebarCategories[0].route}
      menus={sidebarMenus}
      isShowingSubMenus={this.isShowingSubMenus}
      onCategoryClicked={action('onCategoryClicked')}
      isDialogOpen={() => false}
      onAddWallet={action('onAddWallet')}
      openDialogAction={action('openDialog')}
      onSubmitSupportRequest={() => {}}
    />
  );

  getTopbar = (activeWallet: Wallet, activeNavItem: string) => (
    <TopBar
      onToggleSidebar={() => {
        runInAction(() => this.isShowingSubMenus = !this.isShowingSubMenus);
      }}
      formattedWalletAmount={formattedWalletAmount}
      currentRoute={`/wallets/${activeWallet.id}/${activeNavItem}`}
      activeWallet={activeNavItem !== 'empty' ? new Wallet(activeWallet) : null}
      showSubMenuToggle
      showSubMenus={this.isShowingSubMenus}
    >
      <NodeSyncStatusIcon
        networkStatus={{
          isSynced: true,
          syncPercentage: 100,
        }}
        isProduction
      />
    </TopBar>
  );
}
