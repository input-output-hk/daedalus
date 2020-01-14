// @flow
import React from 'react';
import { observable, runInAction } from 'mobx';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import { withKnobs, select } from '@storybook/addon-knobs';
import {
  DEVELOPMENT,
  TESTNET,
  ITN_REWARDS_V1,
  STAGING,
} from '../../../source/common/types/environment.types';
import StoryDecorator from '../_support/StoryDecorator';
import { isIncentivizedTestnetTheme } from '../_support/utils';
import Sidebar from '../../../source/renderer/app/components/sidebar/Sidebar';
import { WalletRecoveryPhraseVerificationStatuses } from '../../../source/renderer/app/stores/WalletsStore';
import {
  CATEGORIES_WITH_DELEGATION_COUNTDOWN,
  CATEGORIES_WITHOUT_DELEGATION_COUNTDOWN,
} from '../../../source/renderer/app/config/sidebarConfig';

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

storiesOf('Navigation|Sidebar', module)
  .addDecorator(story => <StoryDecorator>{story()}</StoryDecorator>)
  .addDecorator(withKnobs)
  // ====== Stories ======
  .add('No Category', (props: { currentTheme: string }) => (
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
      currentTheme={props.currentTheme}
      network="testnet"
      isIncentivizedTestnet={isIncentivizedTestnetTheme(props.currentTheme)}
    />
  ))
  .add('Wallets Category', (props: { currentTheme: string }) => (
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
      currentTheme={props.currentTheme}
      network="testnet"
      isIncentivizedTestnet={isIncentivizedTestnetTheme(props.currentTheme)}
    />
  ))
  .add('Wallet Selected', (props: { currentTheme: string }) => (
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
      currentTheme={props.currentTheme}
      network="testnet"
      isIncentivizedTestnet={isIncentivizedTestnetTheme(props.currentTheme)}
    />
  ))
  .add('Delegation Category', (props: { currentTheme: string }) => (
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
      currentTheme={props.currentTheme}
      network="testnet"
      isIncentivizedTestnet={isIncentivizedTestnetTheme(props.currentTheme)}
    />
  ))
  .add(
    'Decentralization Progress Category',
    (props: { currentTheme: string }) => (
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
        currentTheme={props.currentTheme}
        network="testnet"
        isIncentivizedTestnet={isIncentivizedTestnetTheme(props.currentTheme)}
      />
    )
  )
  .add('Network label', (props: { currentTheme: string }) => (
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
      currentTheme={props.currentTheme}
      network={select(
        'Netork badge',
        {
          Development: DEVELOPMENT,
          Test: TESTNET,
          'Incentivized Testnet v1 - Rewards': ITN_REWARDS_V1,
          Stagiing: STAGING,
        },
        TESTNET
      )}
      isIncentivizedTestnet={isIncentivizedTestnetTheme(props.currentTheme)}
    />
  ));
