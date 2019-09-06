// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';

// Helpers
import WalletsWrapper from '../utils/WalletsWrapper';
import { defaultProps } from '../utils/defaultWalletProps';

// Screens
import WalletSettings from '../../../../source/renderer/app/components/wallet/settings/WalletSettings';
import ChangeSpendingPasswordDialog from '../../../../source/renderer/app/components/wallet/settings/ChangeSpendingPasswordDialog';

/* eslint-disable consistent-return */
storiesOf('Wallets|Settings', module)
  .addDecorator(WalletsWrapper)

  .add('Index', () => <WalletSettings {...defaultProps} />)

  .add('Change Password', () => (
    <WalletSettings
      {...defaultProps}
      isDialogOpen={dialog => {
        if (dialog === ChangeSpendingPasswordDialog) {
          return true;
        }
        return false;
      }}
    />
  ));
