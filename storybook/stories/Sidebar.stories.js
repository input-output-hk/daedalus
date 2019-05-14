// @flow
import React from 'react';
import { observable, runInAction } from 'mobx';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import StoryDecorator from './support/StoryDecorator';
import Sidebar from '../../source/renderer/app/components/sidebar/Sidebar';
import { CATEGORIES_FOR_STORYBOARD } from '../../source/renderer/app/config/sidebarConfig';

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
        runInAction(() => {
          sidebarMenus.wallets.activeWalletId = walletId;
        });
      },
    },
  },
});

let emptyMenus;

storiesOf('Sidebar', module)
  .addDecorator(story => <StoryDecorator>{story()}</StoryDecorator>)
  // ====== Stories ======

  .add('no category', () => (
    <Sidebar
      menus={emptyMenus}
      categories={CATEGORIES_FOR_STORYBOARD}
      activeSidebarCategory=""
      onCategoryClicked={action('onCategoryClicked')}
      isDialogOpen={() => false}
      onAddWallet={action('onAddWallet')}
      openDialogAction={action('openDialog')}
      onSubmitSupportRequest={() => {}}
      pathname="/"
    />
  ))
  .add('wallets category', () => (
    <Sidebar
      menus={emptyMenus}
      categories={CATEGORIES_FOR_STORYBOARD}
      activeSidebarCategory={CATEGORIES_FOR_STORYBOARD[0].route}
      onCategoryClicked={action('onCategoryClicked')}
      isDialogOpen={() => false}
      onAddWallet={action('onAddWallet')}
      openDialogAction={action('openDialog')}
      onSubmitSupportRequest={() => {}}
      pathname="/"
    />
  ))
  .add('wallets / sub', () => (
    <Sidebar
      categories={CATEGORIES_FOR_STORYBOARD}
      activeSidebarCategory={CATEGORIES_FOR_STORYBOARD[0].route}
      menus={sidebarMenus}
      onCategoryClicked={action('onCategoryClicked')}
      isShowingSubMenus
      isDialogOpen={() => false}
      onAddWallet={action('onAddWallet')}
      openDialogAction={action('openDialog')}
      onSubmitSupportRequest={() => {}}
      pathname="/"
    />
  ))
  .add('decentralisation category', () => (
    <Sidebar
      menus={emptyMenus}
      categories={CATEGORIES_FOR_STORYBOARD}
      activeSidebarCategory={CATEGORIES_FOR_STORYBOARD[1].route}
      onCategoryClicked={action('onCategoryClicked')}
      isDialogOpen={() => false}
      onAddWallet={action('onAddWallet')}
      openDialogAction={action('openDialog')}
      onSubmitSupportRequest={() => {}}
      pathname="/"
    />
  ));
