import React from 'react';
import { action } from '@storybook/addon-actions';
// Assets and helpers
import WalletsWrapper from '../_utils/WalletsWrapper';
import WalletFileImportDialog from '../../../../source/renderer/app/components/wallet/file-import/WalletFileImportDialog';

export default {
  title: 'Wallets / Add Wallet',
  decorators: [WalletsWrapper],
};

export const Import = () => (
  <WalletFileImportDialog
    isSubmitting={false}
    onSubmit={action('onSubmit')}
    onClose={action('onClose')}
    error={null}
  />
);
