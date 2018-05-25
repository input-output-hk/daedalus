import React from 'react';
import { observable, runInAction } from 'mobx';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import BigNumber from 'bignumber.js';
import StoryDecorator from './support/StoryDecorator';
import Sidebar from '../../source/renderer/app/components/sidebar/Sidebar';
import walletsIcon from '../../source/renderer/app/assets/images/sidebar/wallet-ic.inline.svg';
import settingsIcon from '../../source/renderer/app/assets/images/sidebar/settings-ic.inline.svg';
import adaIcon from '../../source/renderer/app/assets/images/sidebar/ada-redemption-ic.inline.svg';
import paperCertificateIcon from '../../source/renderer/app/assets/images/sidebar/paper-certificate-ic.inline.svg';

import NodeSyncStatusIcon from '../../source/renderer/app/components/widgets/NodeSyncStatusIcon';
import SidebarLayout from '../../source/renderer/app/components/layout/SidebarLayout';
import SidebarWalletsMenu from '../../source/renderer/app/components/sidebar/wallets/SidebarWalletsMenu';
import TopBar from '../../source/renderer/app/components/layout/TopBar';
import WalletSummary from '../../source/renderer/app/components/wallet/summary/WalletSummary';
import WalletWithNavigation from '../../source/renderer/app/components/wallet/layouts/WalletWithNavigation';
import { formattedWalletAmount } from '../../source/renderer/app/utils/ada/formatters';

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
    activeWalletId: '1',
    actions: {
      onAddWallet: action('toggleAddWallet'),
      onWalletItemClick: (walletId: string) => {
        runInAction(() => sidebarMenus.wallets.activeWalletId = walletId);
      }
    }
  }
});

const topbar = (
  <TopBar formattedWalletAmount={formattedWalletAmount}>
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
  />
);


const WalletScreen = ({children}) => (
  <div>
    <SidebarLayout
      sidebar={sidebar(!!children)}
      topbar={topbar}
    >
      {
        children &&
          (
            <WalletWithNavigation
              isActiveScreen={e => console.log('isActiveScreen', e)}
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
    <WalletScreen>
      <WalletSummary
        walletName="Shopping wallet"
        amount={45119903750165.23}
        pendingAmount={{
          incoming: new BigNumber(1),
          outgoing: new BigNumber(2),
        }}
        numberOfTransactions={20303585}
      />
    </WalletScreen>
  ));
