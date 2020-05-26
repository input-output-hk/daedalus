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
        isNotResponding: false,
        isConnected: true,
        isLegacy: false,
        hasNotification: false,
      },
      {
        id: '2',
        title: 'Second',
        info: '200 ADA',
        isNotResponding: false,
        isConnected: true,
        isLegacy: false,
        hasNotification: false,
      },
      {
        id: '3',
        title: 'Third',
        info: '300 ADA',
        isNotResponding: false,
        isConnected: true,
        isLegacy: false,
        hasNotification: false,
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
  hardwareWallets: null,
});

const sidebarMenusHardware = observable({
  wallets: null,
  hardwareWallets: {
    items: [
      {
        id: '1',
        title: 'BTC wallet',
        info: '2.41824 BTC',
        isNotResponding: false,
        isConnected: false,
        isLegacy: false,
        hasNotification: false,
      },
      {
        id: '2',
        title: 'ETC wallet',
        info: '12M ETC',
        isNotResponding: false,
        isConnected: false,
        isLegacy: false,
        hasNotification: false,
      },
      {
        id: '3',
        title: 'ADA wallet',
        info: '9,800 ADA',
        isNotResponding: false,
        isConnected: true,
        isLegacy: false,
        hasNotification: false,
      },
    ],
    activeWalletId: '3',
    actions: {
      onAddWallet: action('toggleAddWallet'),
      onHardwareWalletItemClick: (walletId: string) => {
        runInAction(() => {
          sidebarMenusHardware.hardwareWallets.activeWalletId = walletId;
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
  .add('Hardware Wallet Selected', (props: { currentTheme: string }) => (
    <Sidebar
      categories={CATEGORIES_WITH_DELEGATION_COUNTDOWN}
      activeSidebarCategory={CATEGORIES_WITH_DELEGATION_COUNTDOWN[1].route}
      menus={sidebarMenusHardware}
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
      activeSidebarCategory={CATEGORIES_WITH_DELEGATION_COUNTDOWN[2].route}
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
