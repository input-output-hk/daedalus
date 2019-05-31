// @flow
import React from 'react';
import { observable, runInAction } from 'mobx';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import StoryDecorator from './support/StoryDecorator';
import Sidebar from '../../source/renderer/app/components/sidebar/Sidebar';
import { CATEGORIES_BY_NAME } from '../../source/renderer/app/config/sidebarConfig';

const CATEGORIES_WITH_DELEGATION_COUNTDOWN = [
  CATEGORIES_BY_NAME.WALLETS,
  CATEGORIES_BY_NAME.STAKING_WITH_DELEGATION_COUNTDOWN,
  CATEGORIES_BY_NAME.SETTINGS,
];

const CATEGORIES_WITHOUT_DELEGATION_COUNTDOWN = [
  CATEGORIES_BY_NAME.WALLETS,
  CATEGORIES_BY_NAME.STAKING_WITHOUT_DELEGATION_COUNTDOWN,
  CATEGORIES_BY_NAME.SETTINGS,
];

const sidebarMenus = observable({
  wallets: {
    items: [
      {
        id: '1',
        title: 'First',
        info: '100 ADA',
        isConnected: true,
        isLegacy: false,
      },
      {
        id: '2',
        title: 'Second',
        info: '200 ADA',
        isConnected: true,
        isLegacy: false,
      },
      {
        id: '3',
        title: 'Third',
        info: '300 ADA',
        isConnected: true,
        isLegacy: false,
      },
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
      categories={CATEGORIES_WITH_DELEGATION_COUNTDOWN}
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
      categories={CATEGORIES_WITH_DELEGATION_COUNTDOWN}
      activeSidebarCategory={CATEGORIES_WITH_DELEGATION_COUNTDOWN[0].route}
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
      categories={CATEGORIES_WITH_DELEGATION_COUNTDOWN}
      activeSidebarCategory={CATEGORIES_WITH_DELEGATION_COUNTDOWN[0].route}
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
  .add('delegation category', () => (
    <Sidebar
      menus={emptyMenus}
      categories={CATEGORIES_WITH_DELEGATION_COUNTDOWN}
      activeSidebarCategory={CATEGORIES_WITH_DELEGATION_COUNTDOWN[1].route}
      onCategoryClicked={action('onCategoryClicked')}
      isDialogOpen={() => false}
      onAddWallet={action('onAddWallet')}
      openDialogAction={action('openDialog')}
      onSubmitSupportRequest={() => {}}
      pathname="/"
    />
  ))
  .add('decentralization-progress', () => (
    <Sidebar
      menus={emptyMenus}
      categories={CATEGORIES_WITHOUT_DELEGATION_COUNTDOWN}
      activeSidebarCategory={CATEGORIES_WITHOUT_DELEGATION_COUNTDOWN[1].route}
      onCategoryClicked={action('onCategoryClicked')}
      isDialogOpen={() => false}
      onAddWallet={action('onAddWallet')}
      openDialogAction={action('openDialog')}
      onSubmitSupportRequest={() => {}}
      pathname="/"
    />
  ));
