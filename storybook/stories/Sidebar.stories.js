// @flow
import React from 'react';
import { observable, runInAction } from 'mobx';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import StoryDecorator from './support/StoryDecorator';
import Sidebar from '../../source/renderer/app/components/sidebar/Sidebar';
import { WalletRecoveryPhraseVerificationStatuses } from '../../source/renderer/app/stores/WalletsStore';
import {
  CATEGORIES_WITH_DELEGATION_COUNTDOWN,
  CATEGORIES_WITHOUT_DELEGATION_COUNTDOWN,
} from '../../source/renderer/app/config/sidebarConfig';

const sidebarMenus = observable({
  wallets: {
    items: [
      {
        id: '1',
        title: 'First',
        info: '100 ADA',
        isConnected: true,
        isLegacy: false,
        recoveryPhraseVerificationStatus:
          WalletRecoveryPhraseVerificationStatuses.OK,
      },
      {
        id: '2',
        title: 'Second',
        info: '200 ADA',
        isConnected: true,
        isLegacy: false,
        recoveryPhraseVerificationStatus:
          WalletRecoveryPhraseVerificationStatuses.OK,
      },
      {
        id: '3',
        title: 'Third',
        info: '300 ADA',
        isConnected: true,
        isLegacy: false,
        recoveryPhraseVerificationStatus:
          WalletRecoveryPhraseVerificationStatuses.OK,
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

let currentTheme = sessionStorage.getItem('themeName') || 'light-blue';
currentTheme = currentTheme.toLowerCase();

storiesOf('Sidebar', module)
  .addDecorator(story => <StoryDecorator>{story()}</StoryDecorator>)
  // ====== Stories ======
  .add('no category', () => (
    <Sidebar
      menus={emptyMenus}
      categories={CATEGORIES_WITH_DELEGATION_COUNTDOWN}
      activeSidebarCategory=""
      onActivateCategory={action('onActivateCategory')}
      isDialogOpen={() => false}
      onAddWallet={action('onAddWallet')}
      onOpenDialog={action('openDialog')}
      onSubmitSupportRequest={() => {}}
      pathname="/"
      currentTheme={currentTheme}
      network="testnet"
      isIncentivizedTestnet
    />
  ))
  .add('wallets category', () => (
    <Sidebar
      menus={emptyMenus}
      categories={CATEGORIES_WITH_DELEGATION_COUNTDOWN}
      activeSidebarCategory={CATEGORIES_WITH_DELEGATION_COUNTDOWN[0].route}
      onActivateCategory={action('onActivateCategory')}
      isDialogOpen={() => false}
      onAddWallet={action('onAddWallet')}
      onOpenDialog={action('openDialog')}
      onSubmitSupportRequest={() => {}}
      pathname="/"
      currentTheme={currentTheme}
      network="testnet"
      isIncentivizedTestnet
    />
  ))
  .add('wallets / sub', () => (
    <Sidebar
      categories={CATEGORIES_WITH_DELEGATION_COUNTDOWN}
      activeSidebarCategory={CATEGORIES_WITH_DELEGATION_COUNTDOWN[0].route}
      menus={sidebarMenus}
      onActivateCategory={action('onActivateCategory')}
      isShowingSubMenus
      isDialogOpen={() => false}
      onAddWallet={action('onAddWallet')}
      onOpenDialog={action('openDialog')}
      onSubmitSupportRequest={() => {}}
      pathname="/"
      currentTheme={currentTheme}
      network="testnet"
      isIncentivizedTestnet
    />
  ))
  .add('delegation category', () => (
    <Sidebar
      menus={emptyMenus}
      categories={CATEGORIES_WITH_DELEGATION_COUNTDOWN}
      activeSidebarCategory={CATEGORIES_WITH_DELEGATION_COUNTDOWN[1].route}
      onActivateCategory={action('onActivateCategory')}
      isDialogOpen={() => false}
      onAddWallet={action('onAddWallet')}
      onOpenDialog={action('openDialog')}
      onSubmitSupportRequest={() => {}}
      pathname="/"
      currentTheme={currentTheme}
      network="testnet"
      isIncentivizedTestnet
    />
  ))
  .add('decentralization-progress', () => (
    <Sidebar
      menus={emptyMenus}
      categories={CATEGORIES_WITHOUT_DELEGATION_COUNTDOWN}
      activeSidebarCategory={CATEGORIES_WITHOUT_DELEGATION_COUNTDOWN[1].route}
      onActivateCategory={action('onActivateCategory')}
      isDialogOpen={() => false}
      onAddWallet={action('onAddWallet')}
      onOpenDialog={action('openDialog')}
      onSubmitSupportRequest={() => {}}
      pathname="/"
      currentTheme={currentTheme}
      network="testnet"
      isIncentivizedTestnet
    />
  ));
