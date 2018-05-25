// @flow
import React from 'react';
import type { Node } from 'react';
import { observable, runInAction } from 'mobx';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import BigNumber from 'bignumber.js';
import StoryDecorator from './support/StoryDecorator';

// Assets
import walletsIcon from '../../source/renderer/app/assets/images/sidebar/wallet-ic.inline.svg';
import settingsIcon from '../../source/renderer/app/assets/images/sidebar/settings-ic.inline.svg';
import adaIcon from '../../source/renderer/app/assets/images/sidebar/ada-redemption-ic.inline.svg';
import paperCertificateIcon from '../../source/renderer/app/assets/images/sidebar/paper-certificate-ic.inline.svg';
import { formattedWalletAmount } from '../../source/renderer/app/utils/ada/formatters';
import NodeSyncStatusIcon from '../../source/renderer/app/components/widgets/NodeSyncStatusIcon';

// Empty screen elements
import TopBar from '../../source/renderer/app/components/layout/TopBar';
import Sidebar from '../../source/renderer/app/components/sidebar/Sidebar';
import SidebarLayout from '../../source/renderer/app/components/layout/SidebarLayout';
import SidebarWalletsMenu from '../../source/renderer/app/components/sidebar/wallets/SidebarWalletsMenu';
import WalletWithNavigation from '../../source/renderer/app/components/wallet/layouts/WalletWithNavigation';

// Screens
import WalletSummary from '../../source/renderer/app/components/wallet/summary/WalletSummary';
import WalletSend from '../../source/renderer/app/components/wallet/WalletSendForm';
// import WalletReceive from '../../source/renderer/app/components/wallet/WalletReceive';
// import WalletTransactions from '../../source/renderer/app/components/wallet/transactions/WalletTransactionsList';
// import WalletSettings from '../../source/renderer/app/components/wallet/WalletSettings';

type Props = {
  activeNavItem?: string,
  children?: any | Node
};

const sidebarCategories = [
  {
    name: 'WALLETS',
    route: '/wallets',
    icon: walletsIcon,
  },
  {
    name: 'SETTINGS',
    route: '/settings',
    icon: settingsIcon,
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
  }
];

const sidebarMenus = observable({
  wallets: {
    items: [
      { id: '1', title: 'First', info: '100 ADA', isConnected: true },
      { id: '2', title: 'Second', info: '200 ADA', isConnected: true },
      { id: '3', title: 'Third', info: '300 ADA', isConnected: true },
    ],
    activeWalletId: '2',
    actions: {
      onAddWallet: action('toggleAddWallet'),
      onWalletItemClick: (walletId: string) => {
        runInAction(() => sidebarMenus.wallets.activeWalletId = walletId);
      }
    }
  }
});

// TODO: Find out how to active the `topBarTitle` on TopBar
const topbar = (
  <TopBar
    formattedWalletAmount={formattedWalletAmount}
    currentRoute="summary"
    showSubMenuToggle
    showSubMenus
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

const sidebar = isShowingSubMenus => (
  <Sidebar
    categories={sidebarCategories}
    activeSidebarCategory={sidebarCategories[0].route}
    menus={sidebarMenus}
    isShowingSubMenus={isShowingSubMenus}
    onCategoryClicked={action('onCategoryClicked')}
    isDialogOpen={() => false}
    onAddWallet={action('onAddWallet')}
    openDialogAction={action('openDialog')}
    onSubmitSupportRequest={() => {}}
  />
);

const WalletScreen = ({ activeNavItem, children }: Props) => (
  <div>
    <SidebarLayout
      sidebar={sidebar(!!children)}
      topbar={topbar}
    >
      {
        children &&
          (
            <WalletWithNavigation
              isActiveScreen={item => item === activeNavItem}
              onWalletNavItemClick={e => console.log('onWalletNavItemClick', e)}
            >
              {children}
            </WalletWithNavigation>
          )
      }
    </SidebarLayout>
  </div>
);

storiesOf('WalletScreens', module)

  .addDecorator((story) => (
    <StoryDecorator>
      {story()}
    </StoryDecorator>
  ))

  // ====== Stories ======

  .add('empty', () => (
    <WalletScreen/>
  ))

  .add('Summary', () => (
    <WalletScreen
      activeNavItem="summary"
    >
      <WalletSummary
        walletName="Shopping wallet"
        amount="45119903750165.23"
        pendingAmount={{
          incoming: new BigNumber(1),
          outgoing: new BigNumber(2),
          total: new BigNumber(3)
        }}
        numberOfTransactions={20303585}
        isLoadingTransactions={false}
      />
    </WalletScreen>
  ))

  .add('Send', () => (
    <WalletScreen
      activeNavItem="send"
    >
      <WalletSend
        currencyUnit="Ada"
        currencyMaxFractionalDigits={ 6}
        currencyMaxIntegerDigits={11}
        validateAmount={() => true}
        calculateTransactionFee={() => {}}
        addressValidator={() => {}}
        openDialogAction={() => {}}
        isDialogOpen={() => {}}
        isRestoreActive={false}
      />
    </WalletScreen>
  ))

  .add('Receive', () => (
    <WalletScreen
      activeNavItem="receive"
    >
      Receive screen
    </WalletScreen>
  ))

  .add('Transactions', () => (
    <WalletScreen
      activeNavItem="transactions"
    >
      Transactions screen
    </WalletScreen>
  ))

  .add('Settings', () => (
    <WalletScreen
      activeNavItem="settings"
    >
      Settings screen
    </WalletScreen>
  ));


