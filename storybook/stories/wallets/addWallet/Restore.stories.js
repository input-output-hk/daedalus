// @flow
import React from 'react';
import { action } from '@storybook/addon-actions';
import { select } from '@storybook/addon-knobs';
import { storiesOf } from '@storybook/react';

// Helpers
import WalletsWrapper from '../_utils/WalletsWrapper';
import {
  WALLET_KINDS,
  WALLET_DAEDALUS_KINDS,
  WALLET_YOROI_KINDS,
  WALLET_HARDWARE_KINDS,
} from '../../../../source/renderer/app/config/walletRestoreConfig';

// Screens
import StepWalletTypeDialog from '../../../../source/renderer/app/components/wallet/wallet-restore/StepWalletTypeDialog';

storiesOf('Wallets|Add Wallet', module)
  .addDecorator(WalletsWrapper)
  .add('Restore - Step 1', () => (
    <StepWalletTypeDialog
      onContinue={action('onContinue')}
      onClose={action('onClose')}
      onSetWalletKind={action('onSetWalletKind')}
      walletKind={select('Wallet Kind', WALLET_KINDS, WALLET_KINDS.DAEDALUS)}
      walletKindDaedalus={select(
        'Daedalus Wallet Kind',
        WALLET_DAEDALUS_KINDS,
        null
      )}
      walletKindYoroi={select('Yoroi Wallet Kind', WALLET_YOROI_KINDS, null)}
      walletKindHardware={select('Hardware Kind', WALLET_HARDWARE_KINDS, null)}
    />
  ));
