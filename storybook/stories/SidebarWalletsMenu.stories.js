// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import StoryDecorator from './support/StoryDecorator';
import SidebarWalletsMenu from '../../source/renderer/app/components/sidebar/wallets/SidebarWalletsMenu';

storiesOf('SidebarWalletsMenu', module)
  .addDecorator(story => (
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
      isAddWalletButtonActive={false}
      visible
    />
  ))

  .add('with items', () => (
    <SidebarWalletsMenu
      wallets={[
        {
          id: '1',
          title: 'Main wallet',
          info: 'ADA',
          isConnected: false,
          isRestoreActive: false,
          restoreProgress: 0,
          isLegacy: false,
          createdAt: new Date(),
          mnemonicsConfirmationDate: new Date(),
          mnemonicsConfirmationStatus: 'ok',
          mnemonicsConfirmationStatusType: 'neverChecked',
        },
        {
          id: '2',
          title: 'House rent',
          info: '274912874,35 ADA',
          isConnected: false,
          isRestoreActive: false,
          restoreProgress: 0,
          isLegacy: false,
          createdAt: new Date(),
          mnemonicsConfirmationDate: new Date(),
          mnemonicsConfirmationStatus: 'ok',
          mnemonicsConfirmationStatusType: 'neverChecked',
        },
        {
          id: '3',
          title: 'Mining',
          info: '0,0004924712 BTC',
          isConnected: false,
          isRestoreActive: false,
          restoreProgress: 0,
          isLegacy: false,
          createdAt: new Date(),
          mnemonicsConfirmationDate: new Date(),
          mnemonicsConfirmationStatus: 'ok',
          mnemonicsConfirmationStatusType: 'neverChecked',
        },
        {
          id: '4',
          title: 'Shopping wallet',
          info: 'ADA',
          isConnected: false,
          isRestoreActive: false,
          restoreProgress: 0,
          isLegacy: false,
          createdAt: new Date(),
          mnemonicsConfirmationDate: new Date(),
          mnemonicsConfirmationStatus: 'ok',
          mnemonicsConfirmationStatusType: 'neverChecked',
        },
      ]}
      isActiveWallet={id => id === '2'}
      onWalletItemClick={action('walletItemClick')}
      onAddWallet={action('addWallet')}
      isAddWalletButtonActive={false}
      visible
    />
  ));
