// @flow
import React from 'react';
import { storiesOf, action } from '@storybook/react';
import StoryDecorator from './support/StoryDecorator';
import SidebarWalletsMenu from '../../source/renderer/app/components/sidebar/wallets/SidebarWalletsMenu';

storiesOf('SidebarWalletsMenu', module)

  .addDecorator((story) => (
    <StoryDecorator>
      <div style={{ width: '200px' }}>{story()}</div>
    </StoryDecorator>
  ))

  // ====== Stories ======

  .add('empty', () => (
    <SidebarWalletsMenu
      wallets={[]}
      onAddWallet={action('addWallet')}
      onWalletItemClick={() => {}}
      isActiveWallet={() => false}
      visible
    />
  ))

  .add('with items', () => (
    <SidebarWalletsMenu
      wallets={[
        { id: '1', title: 'Main wallet', info: 'ADA', isConnected: false, isRestoreActive: false, restoreProgress: 0 },
        { id: '2', title: 'House rent', info: '274912874,35 ADA', isConnected: false, isRestoreActive: false, restoreProgress: 0 },
        { id: '3', title: 'Mining', info: '0,0004924712 BTC', isConnected: false, isRestoreActive: false, restoreProgress: 0 },
        { id: '4', title: 'Shopping wallet', info: 'ADA', isConnected: false, isRestoreActive: false, restoreProgress: 0 },
      ]}
      isActiveWallet={(id) => id === '2'}
      onWalletItemClick={action('walletItemClick')}
      onAddWallet={action('addWallet')}
      visible
    />
  ));
