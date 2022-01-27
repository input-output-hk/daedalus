// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import { withKnobs } from '@storybook/addon-knobs';
import StoryDecorator from '../_support/StoryDecorator';
import StoryProvider from '../_support/StoryProvider';
import { isShelleyTestnetTheme } from '../_support/utils';
import {
  WalletSortBy,
  WalletSortOrder,
} from '../../../source/renderer/app/types/sidebarTypes';
import SidebarWalletsMenu from '../../../source/renderer/app/components/sidebar/wallets/SidebarWalletsMenu';

const wallets = [
  {
    id: '1',
    title: 'Main wallet',
    amount: 0,
    isConnected: false,
    isRestoreActive: false,
    restoreProgress: 0,
    isNotResponding: false,
    isLegacy: false,
    createdAt: new Date(),
    recoveryPhraseVerificationDate: new Date(),
    hasNotification: false,
  },
  {
    id: '2',
    title: 'House rent',
    amount: 274912874.35,
    isConnected: false,
    isRestoreActive: false,
    restoreProgress: 0,
    isNotResponding: false,
    isLegacy: false,
    createdAt: new Date(),
    recoveryPhraseVerificationDate: new Date(),
    hasNotification: false,
  },
  {
    id: '3',
    title: 'Mining',
    amount: 0.0004924712,
    isConnected: false,
    isRestoreActive: false,
    restoreProgress: 0,
    isNotResponding: false,
    isLegacy: false,
    createdAt: new Date(),
    recoveryPhraseVerificationDate: new Date(),
    hasNotification: false,
  },
  {
    id: '4',
    title: 'Shopping wallet',
    amount: 0,
    isConnected: false,
    isRestoreActive: false,
    restoreProgress: 0,
    isNotResponding: false,
    isLegacy: false,
    createdAt: new Date(),
    recoveryPhraseVerificationDate: new Date(),
    hasNotification: false,
  },
];

storiesOf('Navigation|Wallets Menu', module)
  .addDecorator(withKnobs)
  .addDecorator((story) => (
    <StoryDecorator>
      <StoryProvider>
        <div style={{ width: '100%', height: '100%' }}>{story()}</div>
      </StoryProvider>
    </StoryDecorator>
  ))

  // // ====== Stories ======

  .add('Empty', (props: { currentTheme: string }) => (
    <SidebarWalletsMenu
      wallets={[]}
      onAddWallet={action('addWallet')}
      onWalletItemClick={() => {}}
      isActiveWallet={() => false}
      isAddWalletButtonActive={false}
      isShelleyActivated={isShelleyTestnetTheme(props.currentTheme)}
      visible={false}
      sortBy={WalletSortBy.Date}
      sortOrder={WalletSortOrder.Desc}
    />
  ))

  .add('With Wallets', (props: { currentTheme: string }) => (
    <div style={{ display: 'flex', height: '100%' }}>
      <div style={{ position: 'relative', height: '100%', width: '196px' }}>
        <SidebarWalletsMenu
          wallets={wallets}
          isActiveWallet={(id) => id === '2'}
          onWalletItemClick={action('walletItemClick')}
          onAddWallet={action('addWallet')}
          isAddWalletButtonActive={false}
          isShelleyActivated={isShelleyTestnetTheme(props.currentTheme)}
          visible
          sortBy={'DATE'}
          sortOrder={WalletSortOrder.Asc}
        />
      </div>
      <div style={{ position: 'relative', height: '100%', width: '196px' }}>
        <SidebarWalletsMenu
          wallets={wallets}
          isActiveWallet={(id) => id === '2'}
          onWalletItemClick={action('walletItemClick')}
          onAddWallet={action('addWallet')}
          isAddWalletButtonActive={false}
          isShelleyActivated={isShelleyTestnetTheme(props.currentTheme)}
          visible
          sortBy={'DATE'}
          sortOrder={WalletSortOrder.Asc}
          searchValue="in"
        />
      </div>
    </div>
  ));
