// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import WalletsWrapper from '../_utils/WalletsWrapper';
import ExportWalletToFileDialog from '../../../../source/renderer/app/components/wallet/settings/ExportWalletToFileDialog';

storiesOf('Wallets|Export to File', module)
  .addDecorator(WalletsWrapper)

  // ====== Stories ======

  .add('default', () => (
    <div>
      <ExportWalletToFileDialog
        walletName="Test Wallet"
        isSubmitting={false}
        onSubmit={action('onSubmit')}
        onClose={action('onClose')}
      />
    </div>
  ))

  .add('submitting', () => (
    <div>
      <ExportWalletToFileDialog
        walletName="Test Wallet"
        isSubmitting
        onSubmit={action('onSubmit')}
        onClose={action('onClose')}
      />
    </div>
  ))

  .add('spending password', () => (
    <div>
      <ExportWalletToFileDialog
        walletName="Test Wallet"
        isSubmitting={false}
        onSubmit={action('onSubmit')}
        onClose={action('onClose')}
      />
    </div>
  ));
