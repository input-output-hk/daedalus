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
import walletsIcon from '../../../source/renderer/app/assets/images/sidebar/wallet-ic.inline.svg';
import settingsIcon from '../../../source/renderer/app/assets/images/sidebar/settings-ic.inline.svg';
import adaIcon from '../../../source/renderer/app/assets/images/sidebar/ada-redemption-ic.inline.svg';
import paperCertificateIcon from '../../../source/renderer/app/assets/images/sidebar/paper-certificate-ic.inline.svg';

// Empty screen elements
import TopBar from '../../../source/renderer/app/components/layout/TopBar';
import Sidebar from '../../../source/renderer/app/components/sidebar/Sidebar';
import SidebarLayout from '../../../source/renderer/app/components/layout/SidebarLayout';

export type StoriesProps = {
  wallets: Array<Wallet>,
  activeWalletId: number,
  sidebarMenus: SidebarMenus,
};

type Props = {
  activeSidebarCategory: string,
  storiesProps: any | StoriesProps,
  storyName?: string,
  children?: any | Node,
  stores: {},
};

const sidebarCategories = [
  {
    name: 'WALLETS',
    route: '/wallets',
    icon: walletsIcon,
  },
  {
    name: 'ADA_REDEMPTION',
    route: '/ada-redemption',
    icon: adaIcon,
  },
  {
    name: 'PAPER_WALLET',
    route: '/paper-wallet-create-certificate',
    icon: paperCertificateIcon,
  },
  {
    name: 'SETTINGS',
    route: '/settings',
    icon: settingsIcon,
  },
];


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
      setActiveWalletId
    } = storiesProps;

    const activeWallet: Wallet = wallets[activeWalletId];
    const activeNavItem = storyName.split(' ')[0].toLowerCase();
    const sidebarMenus = this.getSidebarMenus(wallets, activeWalletId, setActiveWalletId);

    return (
      <div
        style={{
          height: '100vh'
        }}
      >
        <SidebarLayout
          sidebar={this.getSidebar(activeSidebarCategory, sidebarMenus)}
          topbar={this.getTopbar(activeSidebarCategory, activeWallet, activeNavItem)}
        >
          { Children.map(children, (child) => React.cloneElement(child, { stores })) }
        </SidebarLayout>
      </div>
    );
  }

  @observable
  isShowingSubMenus = this.props.activeSidebarCategory === '/wallets' && !!this.props.children;

  getSidebarMenus = (
    wallets: Array<Wallet>,
    activeWalletId: number,
    setActiveWalletId: Function
  ) => ({
    wallets: {
      items: [
        { id: wallets[0].id, title: wallets[0].name, info: `${wallets[0].amount} ADA`, isConnected: true },
        { id: wallets[1].id, title: wallets[1].name, info: `${wallets[1].amount} ADA`, isConnected: true },
      ],
      activeWalletId,
      actions: {
        onAddWallet: action('toggleAddWallet'),
        onWalletItemClick: setActiveWalletId
      }
    }
  })

  getSidebar = (
    activeSidebarCategory: string,
    sidebarMenus: SidebarMenus,
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
