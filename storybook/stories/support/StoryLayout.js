// @flow
import React, { Component } from 'react';
import type { Node } from 'react';
import { observable, runInAction } from 'mobx';
import { observer, inject } from 'mobx-react';
import { storiesOf } from '@storybook/react';
import { linkTo } from '@storybook/addon-links';
import { action } from '@storybook/addon-actions';
import { withKnobs, text, boolean, number, select } from '@storybook/addon-knobs';
import BigNumber from 'bignumber.js';
import startCase from 'lodash/startCase';

// Assets and helpers
import { formattedWalletAmount } from '../../../source/renderer/app/utils/ada/formatters';
import NodeSyncStatusIcon from '../../../source/renderer/app/components/widgets/NodeSyncStatusIcon';
import Wallet from '../../../source/renderer/app/domains/Wallet.js'

// Empty screen elements
import TopBar from '../../../source/renderer/app/components/layout/TopBar';
import Sidebar from '../../../source/renderer/app/components/sidebar/Sidebar';
import SidebarLayout from '../../../source/renderer/app/components/layout/SidebarLayout';
import SidebarWalletsMenu from '../../../source/renderer/app/components/sidebar/wallets/SidebarWalletsMenu';
import WalletWithNavigation from '../../../source/renderer/app/components/wallet/layouts/WalletWithNavigation';

// Screens
import WalletSummary from '../../../source/renderer/app/components/wallet/summary/WalletSummary';
import WalletSendForm from '../../../source/renderer/app/components/wallet/WalletSendForm';
import WalletReceive from '../../../source/renderer/app/components/wallet/WalletReceive';
import WalletTransactionsList from '../../../source/renderer/app/components/wallet/transactions/WalletTransactionsList';
import WalletSettings from '../../../source/renderer/app/components/wallet/WalletSettings';

type Props = {
  storyName?: string,
  children?: any | Node
};

@inject('actions', 'stores', 'storiesProps') @observer
export default class StoryLayout extends Component<Props> {

  render() {

    const { children, storyName='', storiesProps={} } = this.props;
    const { wallets, sidebarWallets, activeWalletId, sidebarMenus, sidebarCategories } = storiesProps;

    const activeWallet = wallets[activeWalletId];
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

  getSidebar = (sidebarMenus: object, sidebarCategories: object) => (
    <Sidebar
      categories={sidebarCategories}
      activeSidebarCategory={sidebarCategories[0].route}
      menus={sidebarMenus}
      isShowingSubMenus={this.isShowingSubMenus}
      onCategoryClicked={action('onCategoryClicked')}
      isDialogOpen={() => false}
      onAddWallet={action('onAddWallet')}
      openDialogAction={action('openDialog')}
      onSubmitSupportRequest={()=>{}}
    />
  );

  getTopbar = (activeWallet: object, activeNavItem: string) => (
    <TopBar
      onToggleSidebar={() => {
        runInAction(() => this.isShowingSubMenus = !this.isShowingSubMenus);
      }}
      formattedWalletAmount={formattedWalletAmount}
      currentRoute={`/wallets/${activeWallet.id}/${activeNavItem}`}
      activeWallet={activeNavItem !== 'empty' ? new Wallet(activeWallet) : null}
      showSubMenuToggle={true}
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
