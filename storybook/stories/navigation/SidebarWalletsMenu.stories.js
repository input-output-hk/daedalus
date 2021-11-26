// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import StoryDecorator from '../_support/StoryDecorator';
import { isShelleyTestnetTheme } from '../_support/utils';
import WalletsWrapper from '../wallets/_utils/WalletsWrapper';
import SidebarWalletsMenu from '../../../source/renderer/app/components/sidebar/wallets/SidebarWalletsMenu';

storiesOf('Navigation|Wallets Menu', module)
  .addDecorator((story) => (
    <StoryDecorator>
      <div style={{ width: '200px' }}>{story()}</div>
    </StoryDecorator>
  ))
  .addDecorator(WalletsWrapper)

  // ====== Stories ======

  .add('Empty', (props: { currentTheme: string }) => (
    <SidebarWalletsMenu
      wallets={[]}
      onAddWallet={action('addWallet')}
      onWalletItemClick={() => {}}
      isActiveWallet={() => false}
      isAddWalletButtonActive={false}
      isShelleyActivated={isShelleyTestnetTheme(props.currentTheme)}
      visible
    />
  ))

  .add('With Wallets', (props: { currentTheme: string }) => (
    <SidebarWalletsMenu
      wallets={[
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
      ]}
      isActiveWallet={(id) => id === '2'}
      onWalletItemClick={action('walletItemClick')}
      onAddWallet={action('addWallet')}
      isAddWalletButtonActive={false}
      isShelleyActivated={isShelleyTestnetTheme(props.currentTheme)}
      visible
    />
  ));
