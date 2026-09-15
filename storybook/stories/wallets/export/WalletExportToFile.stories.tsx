import React from 'react';
import { action } from '@storybook/addon-actions';
import WalletsWrapper from '../_utils/WalletsWrapper';
import ExportWalletToFileDialog from '../../../../source/renderer/app/components/wallet/settings/ExportWalletToFileDialog';

export default {
  title: 'Wallets / Export to File',
  decorators: [WalletsWrapper],
};

export const Default = {
  render: () => (
    <div>
      <ExportWalletToFileDialog
        walletName="Test Wallet"
        isSubmitting={false}
        // @ts-ignore ts-migrate(2322) FIXME: Type 'HandlerFunction' is not assignable to type '... Remove this comment to see the full error message
        onSubmit={action('onSubmit')}
        onClose={action('onClose')}
      />
    </div>
  ),

  name: 'default',
};

export const Submitting = {
  render: () => (
    <div>
      <ExportWalletToFileDialog
        walletName="Test Wallet"
        isSubmitting
        // @ts-ignore ts-migrate(2322) FIXME: Type 'HandlerFunction' is not assignable to type '... Remove this comment to see the full error message
        onSubmit={action('onSubmit')}
        onClose={action('onClose')}
      />
    </div>
  ),

  name: 'submitting',
};

export const SpendingPassword = {
  render: () => (
    <div>
      <ExportWalletToFileDialog
        walletName="Test Wallet"
        isSubmitting={false}
        // @ts-ignore ts-migrate(2322) FIXME: Type 'HandlerFunction' is not assignable to type '... Remove this comment to see the full error message
        onSubmit={action('onSubmit')}
        onClose={action('onClose')}
      />
    </div>
  ),

  name: 'spending password',
};
