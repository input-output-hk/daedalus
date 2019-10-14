// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';

// Helpers
import WalletsWrapper from '../utils/WalletsWrapper';
import { defaultProps } from '../utils/defaultWalletProps';

// Screens
import DeleteWalletConfirmationDialog from '../../../../source/renderer/app/components/wallet/settings/DeleteWalletConfirmationDialog';
import WalletSettings from '../../../../source/renderer/app/components/wallet/settings/WalletSettings';

storiesOf('Wallets|Actions', module)
  .addDecorator(WalletsWrapper)
  .add('Delete', () => (
    <WalletSettings
      {...defaultProps}
      isDialogOpen={dialog => {
        if (dialog === DeleteWalletConfirmationDialog) {
          return true;
        }
        return false;
      }}
    />
  ));
