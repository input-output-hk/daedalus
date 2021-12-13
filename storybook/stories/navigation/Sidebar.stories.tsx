import React from 'react';
import { observable, runInAction } from 'mobx';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import { withKnobs, select } from '@storybook/addon-knobs';
import BigNumber from 'bignumber.js';
import {
  DEVELOPMENT,
  TESTNET,
  STAGING,
} from '../../../source/common/types/environment.types';
import StoryDecorator from '../_support/StoryDecorator';
import StoryProvider from '../_support/StoryProvider';
import { isShelleyTestnetTheme } from '../_support/utils';
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
        amount: new BigNumber(100),
        isNotResponding: false,
        isConnected: true,
        isLegacy: false,
        hasNotification: false,
      },
      {
        id: '2',
        title: 'Second',
        amount: new BigNumber(200),
        isNotResponding: false,
        isConnected: true,
        isLegacy: false,
        hasNotification: false,
      },
      {
        id: '3',
        title: 'Third',
        amount: new BigNumber(300),
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
  .addDecorator((story) => (
    <StoryProvider>
      <StoryDecorator>{story()}</StoryDecorator>
    </StoryProvider>
  ))
  .addDecorator(withKnobs) // ====== Stories ======
  // @ts-ignore ts-migrate(2345) FIXME: Argument of type '(props: {    currentTheme: strin... Remove this comment to see the full error message
  .add('No Category', (props: { currentTheme: string }) => (
    <Sidebar
      menus={emptyMenus}
      categories={CATEGORIES_WITH_DELEGATION_COUNTDOWN}
      activeSidebarCategory=""
      onActivateCategory={action('onActivateCategory')}
      // @ts-ignore ts-migrate(2322) FIXME: Type '{ menus: any; categories: { name: string; ic... Remove this comment to see the full error message
      isDialogOpen={() => false}
      onAddWallet={action('onAddWallet')}
      onOpenDialog={action('openDialog')}
      onSubmitSupportRequest={() => {}}
      pathname="/"
      currentTheme={props.currentTheme}
      network="testnet"
      isShelleyActivated={isShelleyTestnetTheme(props.currentTheme)}
    />
  ))
  // @ts-ignore ts-migrate(2345) FIXME: Argument of type '(props: {    currentTheme: strin... Remove this comment to see the full error message
  .add('Wallets Category', (props: { currentTheme: string }) => (
    <Sidebar
      menus={emptyMenus}
      categories={CATEGORIES_WITH_DELEGATION_COUNTDOWN}
      activeSidebarCategory={CATEGORIES_WITH_DELEGATION_COUNTDOWN[0].route}
      onActivateCategory={action('onActivateCategory')}
      // @ts-ignore ts-migrate(2322) FIXME: Type '{ menus: any; categories: { name: string; ic... Remove this comment to see the full error message
      isDialogOpen={() => false}
      onAddWallet={action('onAddWallet')}
      onOpenDialog={action('openDialog')}
      onSubmitSupportRequest={() => {}}
      pathname="/"
      currentTheme={props.currentTheme}
      network="testnet"
      isShelleyActivated={isShelleyTestnetTheme(props.currentTheme)}
    />
  ))
  // @ts-ignore ts-migrate(2345) FIXME: Argument of type '(props: {    currentTheme: strin... Remove this comment to see the full error message
  .add('Wallet Selected', (props: { currentTheme: string }) => (
    <Sidebar
      categories={CATEGORIES_WITH_DELEGATION_COUNTDOWN}
      activeSidebarCategory={CATEGORIES_WITH_DELEGATION_COUNTDOWN[0].route}
      menus={sidebarMenus}
      onActivateCategory={action('onActivateCategory')}
      isShowingSubMenus
      // @ts-ignore ts-migrate(2322) FIXME: Type '{ categories: { name: string; icon: any; rou... Remove this comment to see the full error message
      isDialogOpen={() => false}
      onAddWallet={action('onAddWallet')}
      onOpenDialog={action('openDialog')}
      onSubmitSupportRequest={() => {}}
      pathname="/"
      currentTheme={props.currentTheme}
      network="testnet"
      isShelleyActivated={isShelleyTestnetTheme(props.currentTheme)}
    />
  ))
  // @ts-ignore ts-migrate(2345) FIXME: Argument of type '(props: {    currentTheme: strin... Remove this comment to see the full error message
  .add('Hardware Wallet Selected', (props: { currentTheme: string }) => (
    <Sidebar
      categories={CATEGORIES_WITH_DELEGATION_COUNTDOWN}
      activeSidebarCategory={CATEGORIES_WITH_DELEGATION_COUNTDOWN[1].route}
      menus={sidebarMenusHardware}
      onActivateCategory={action('onActivateCategory')}
      isShowingSubMenus
      // @ts-ignore ts-migrate(2322) FIXME: Type '{ categories: { name: string; icon: any; rou... Remove this comment to see the full error message
      isDialogOpen={() => false}
      onAddWallet={action('onAddWallet')}
      onOpenDialog={action('openDialog')}
      onSubmitSupportRequest={() => {}}
      pathname="/"
      currentTheme={props.currentTheme}
      network="testnet"
      isShelleyActivated={isShelleyTestnetTheme(props.currentTheme)}
    />
  ))
  // @ts-ignore ts-migrate(2345) FIXME: Argument of type '(props: {    currentTheme: strin... Remove this comment to see the full error message
  .add('Delegation Category', (props: { currentTheme: string }) => (
    <Sidebar
      menus={emptyMenus}
      categories={CATEGORIES_WITH_DELEGATION_COUNTDOWN}
      activeSidebarCategory={CATEGORIES_WITH_DELEGATION_COUNTDOWN[2].route}
      onActivateCategory={action('onActivateCategory')}
      // @ts-ignore ts-migrate(2322) FIXME: Type '{ menus: any; categories: { name: string; ic... Remove this comment to see the full error message
      isDialogOpen={() => false}
      onAddWallet={action('onAddWallet')}
      onOpenDialog={action('openDialog')}
      onSubmitSupportRequest={() => {}}
      pathname="/"
      currentTheme={props.currentTheme}
      network="testnet"
      isShelleyActivated={isShelleyTestnetTheme(props.currentTheme)}
    />
  ))
  .add(
    'Decentralization Progress Category',
    // @ts-ignore ts-migrate(2345) FIXME: Argument of type '(props: {    currentTheme: strin... Remove this comment to see the full error message
    (props: { currentTheme: string }) => (
      <Sidebar
        menus={emptyMenus}
        categories={CATEGORIES_WITHOUT_DELEGATION_COUNTDOWN}
        activeSidebarCategory={CATEGORIES_WITHOUT_DELEGATION_COUNTDOWN[1].route}
        onActivateCategory={action('onActivateCategory')}
        // @ts-ignore ts-migrate(2322) FIXME: Type '{ menus: any; categories: { name: string; ic... Remove this comment to see the full error message
        isDialogOpen={() => false}
        onAddWallet={action('onAddWallet')}
        onOpenDialog={action('openDialog')}
        onSubmitSupportRequest={() => {}}
        pathname="/"
        currentTheme={props.currentTheme}
        network="testnet"
        isShelleyActivated={isShelleyTestnetTheme(props.currentTheme)}
      />
    )
  )
  // @ts-ignore ts-migrate(2345) FIXME: Argument of type '(props: {    currentTheme: strin... Remove this comment to see the full error message
  .add('Network label', (props: { currentTheme: string }) => (
    <Sidebar
      menus={emptyMenus}
      categories={CATEGORIES_WITH_DELEGATION_COUNTDOWN}
      activeSidebarCategory=""
      onActivateCategory={action('onActivateCategory')}
      // @ts-ignore ts-migrate(2322) FIXME: Type '{ menus: any; categories: { name: string; ic... Remove this comment to see the full error message
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
          Staging: STAGING,
        },
        TESTNET
      )}
      isShelleyActivated={isShelleyTestnetTheme(props.currentTheme)}
    />
  ));
