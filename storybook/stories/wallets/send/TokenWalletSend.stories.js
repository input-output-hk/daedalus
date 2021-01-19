// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import { boolean } from '@storybook/addon-knobs';
import BigNumber from 'bignumber.js';
import { generateWallet, promise } from '../../_support/utils';

// Assets and helpers
import WalletsWrapper from '../_utils/WalletsWrapper';
import { NUMBER_OPTIONS } from '../../../../source/renderer/app/config/profileConfig';
import { HwDeviceStatuses } from '../../../../source/renderer/app/domains/Wallet';

// Screens
import WalletTokenSendForm from '../../../../source/renderer/app/components/wallet/WalletTokenSendForm';

storiesOf('Token Wallets|Send', module)
  .addDecorator(WalletsWrapper)
  .add('Send', () => (
    <WalletTokenSendForm
      currencyUnit="USDC"
      currencyMaxFractionalDigits={6}
      currencyMaxIntegerDigits={11}
      currentNumberFormat={NUMBER_OPTIONS[0].value}
      validateAmount={promise(true)}
      calculateTransactionFee={promise(true)}
      addressValidator={action('addressValidator')}
      openDialogAction={action('openDialogAction')}
      isDialogOpen={() => boolean('isDialogOpen', false)}
      isRestoreActive={boolean('isRestoreActive', false)}
      walletAmount={new BigNumber(123)}
      hwDeviceStatus={HwDeviceStatuses.READY}
      isHardwareWallet={boolean('isHardwareWallet', false)}
      nativeTokens={[
        generateWallet('ADA', '55119903750165'),
        generateWallet('Tether', '25119903750165'),
        generateWallet('TrueUSD', '15119903750165'),
        generateWallet('USD Coin', '0'),
      ]}
    />
  ));
