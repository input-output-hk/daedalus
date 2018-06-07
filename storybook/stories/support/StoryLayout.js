// @flow
import React, { Component, Children } from 'react';
import type { Node } from 'react';
import { observable, runInAction } from 'mobx';
import { observer, inject } from 'mobx-react';
import { action } from '@storybook/addon-actions';

// Assets and helpers
import { formattedWalletAmount } from '../../../source/renderer/app/utils/ada/formatters';
import NodeSyncStatusIcon from '../../../source/renderer/app/components/widgets/NodeSyncStatusIcon';
import Wallet from '../../../source/renderer/app/domains/Wallet.js';
import type { SidebarMenus, SidebarCategories } from '../../../source/renderer/app/components/sidebar/Sidebar';

// Empty screen elements
import TopBar from '../../../source/renderer/app/components/layout/TopBar';
import Sidebar from '../../../source/renderer/app/components/sidebar/Sidebar';
import SidebarLayout from '../../../source/renderer/app/components/layout/SidebarLayout';

export type StoriesProps = {
  wallets: Array<Wallet>,
  activeWalletId: number,
  sidebarMenus: SidebarMenus,
  sidebarCategories: Array<any>,
};

type Props = {
  activeSidebarCategory: string,
  storiesProps: any | StoriesProps,
  storyName?: string,
  children?: any | Node,
  stores: {},
};


@inject('stores', 'storiesProps') @observer
export default class StoryLayout extends Component<Props> {

  static defaultProps = { stores: null, storiesProps: null };

  render() {

    const {
      activeSidebarCategory,
      storyName = '',
      storiesProps = {},
      stores,
      children
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
          sidebar={this.getSidebar(activeSidebarCategory, sidebarMenus, sidebarCategories)}
          topbar={this.getTopbar(activeSidebarCategory, activeWallet, activeNavItem)}
        >
          { Children.map(children, (child) => React.cloneElement(child, { stores })) }
        </SidebarLayout>
      </div>
    );
  }

  @observable
  isShowingSubMenus = this.props.activeSidebarCategory === '/wallets' && !!this.props.children;

  getSidebar = (
    activeSidebarCategory: string,
    sidebarMenus: SidebarMenus,
    sidebarCategories: SidebarCategories
  ) => (
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
    />
  );

  getTopbar = (activeSidebarCategory: string, activeWallet: Wallet, activeNavItem: string) => (
    <TopBar
      onToggleSidebar={() => {
        runInAction(() => this.isShowingSubMenus = !this.isShowingSubMenus);
      }}
      formattedWalletAmount={formattedWalletAmount}
      currentRoute={`/wallets/${activeWallet.id}/${activeNavItem}`}
      activeWallet={activeSidebarCategory === '/wallets' && activeNavItem !== 'empty' ? activeWallet : null}
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
