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
import WalletTypeDialog from '../../../../source/renderer/app/components/wallet/wallet-restore/WalletTypeDialog';
import MnemonicsDialog from '../../../../source/renderer/app/components/wallet/wallet-restore/MnemonicsDialog';
import ConfigurationDialog from '../../../../source/renderer/app/components/wallet/wallet-restore/ConfigurationDialog';

storiesOf('Wallets|Add Wallet', module)
  .addDecorator(WalletsWrapper)
  .add('Restore - Step 1', () => {
    const walletKindSelect = select(
      'Wallet Kind',
      { '-': null, ...WALLET_KINDS },
      null
    );
    let selectItems;
    if (walletKindSelect === WALLET_KINDS.YOROI)
      selectItems = WALLET_YOROI_KINDS;
    else if (walletKindSelect === WALLET_KINDS.HARDWARE)
      selectItems = WALLET_HARDWARE_KINDS;
    else selectItems = WALLET_DAEDALUS_KINDS;

    let walletKindSpecificSelect;
    if (walletKindSelect)
      walletKindSpecificSelect = select(
        `Wallet Kind - ${walletKindSelect || 'Daedalus'}`,
        {
          '-': null,
          ...selectItems,
        },
        null
      );

    return (
      <WalletTypeDialog
        onContinue={action('onContinue')}
        onClose={action('onClose')}
        onSetWalletKind={action('onSetWalletKind')}
        walletKind={walletKindSelect}
        walletKindDaedalus={walletKindSpecificSelect}
        walletKindYoroi={walletKindSpecificSelect}
        walletKindHardware={walletKindSpecificSelect}
      />
    );
  })
  .add('Restore - Step 3', () => (
    <ConfigurationDialog
      onContinue={action('onContinue')}
      onClose={action('onClose')}
      onBack={action('onSetWalletKind')}
    />
  ));
