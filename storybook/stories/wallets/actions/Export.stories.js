// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';

// Helpers
import WalletsWrapper from '../utils/WalletsWrapper';
import { defaultProps } from '../utils/defaultWalletProps';

// Screens
import ExportWalletToFileDialog from '../../../../source/renderer/app/components/wallet/settings/ExportWalletToFileDialog';
import WalletSettings from '../../../../source/renderer/app/components/wallet/settings/WalletSettings';

storiesOf('Wallets|Actions', module)
  .addDecorator(WalletsWrapper)
  .add('Export', () => (
    <WalletSettings
      {...defaultProps}
      isDialogOpen={dialog => {
        if (dialog === ExportWalletToFileDialog) {
          return true;
        }
        return false;
      }}
    />
  ));
