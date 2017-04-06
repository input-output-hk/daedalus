import React from 'react';
import { observable, runInAction } from 'mobx';
import { storiesOf, action } from '@kadira/storybook';
import StoryDecorator from './support/StoryDecorator';
import Sidebar from '../app/components/sidebar/Sidebar';

const SIDEBAR_CATEGORIES = {
  WALLETS: '/wallets',
  ADA_REDEMPTION: '/ada-redemption',
  SETTINGS: '/settings',
};

const sidebarMenus = observable({
  wallets: {
    items: [
      { id: '1', title: 'First', info: '100 ADA', isConnected: true },
      { id: '2', title: 'Second', info: '200 ADA', isConnected: true },
      { id: '3', title: 'Third', info: '300 ADA', isConnected: true },
    ],
    activeWalletId: '1',
    actions: {
      onAddWallet: action('toggleAddWallet'),
      onWalletItemClick: (walletId: string) => {
        runInAction(() => sidebarMenus.wallets.activeWalletId = walletId);
      }
    }
  }
});

storiesOf('Sidebar', module)

  .addDecorator((story) => (
    <StoryDecorator>
      {story()}
    </StoryDecorator>
  ))

  // ====== Stories ======

  .add('no category', () => (
    <Sidebar
      categories={SIDEBAR_CATEGORIES}
      onCategoryClicked={action('onCategoryClicked')}
    />
  ))

  .add('wallets category', () => (
    <Sidebar
      categories={SIDEBAR_CATEGORIES}
      activeSidebarCategory={SIDEBAR_CATEGORIES.WALLETS}
      onCategoryClicked={action('onCategoryClicked')}
    />
  ))

  .add('wallets / sub', () => (
    <Sidebar
      categories={SIDEBAR_CATEGORIES}
      activeSidebarCategory={SIDEBAR_CATEGORIES.WALLETS}
      menus={sidebarMenus}
      onCategoryClicked={action('onCategoryClicked')}
      isShowingSubMenus
    />
  ))

  .add('hidden', () => (
    <Sidebar
      categories={SIDEBAR_CATEGORIES}
      route=""
      hidden
    />
  ));
