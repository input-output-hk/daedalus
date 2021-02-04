// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import { boolean } from '@storybook/addon-knobs';
import BigNumber from 'bignumber.js';
import {
  generateHash,
  generateAsset,
  generateWallet,
  promise, generatePolicyIdHash,
} from '../../_support/utils';

// Assets and helpers
import WalletsWrapper from '../_utils/WalletsWrapper';
import { NUMBER_OPTIONS } from '../../../../source/renderer/app/config/profileConfig';
import {
  HwDeviceStatuses,
} from '../../../../source/renderer/app/domains/Wallet';

// Screens
import WalletSendForm from '../../../../source/renderer/app/components/wallet/WalletSendForm';
import WalletTokenSendForm from '../../../../source/renderer/app/components/wallet/WalletTokenSendForm';
import WalletTokenSendConfirmationDialog from '../../../../source/renderer/app/components/wallet/WalletTokenSendConfirmationDialog';
import { DECIMAL_PLACES_IN_ADA } from '../../../../source/renderer/app/config/numbersConfig';
import { formattedAmountToNaturalUnits } from '../../../../source/renderer/app/utils/formatters';

const assets = {
  available: [
    {
      id: generateHash(),
      policyId: generatePolicyIdHash(),
      assetName: '',
      quantity: 200,
    },
    {
      id: generateHash(),
      policyId: generatePolicyIdHash(),
      assetName: '',
      quantity: 200,
    },
  ],
  total: [
    {
      id: generateHash(),
      policyId: generatePolicyIdHash(),
      assetName: '',
      quantity: 200,
    },
    {
      id: generateHash(),
      policyId: generatePolicyIdHash(),
      assetName: '',
      quantity: 200,
    },
  ]
};

const nativeTokens = [
  generateAsset(
    '65bc72542b0ca20391caaf66a4d4d7897d281f9c136cd3513136945b',
    '',
    {
      name: 'TrueUSD',
      acronym: 'TUSD',
      description: 'Test description',
      unit: {
        name: 'TUSD',
        decimals: 6
      },
      url: 'http://example.com',
      logo: ''
    },
  ),
  generateAsset(
    '65ac82542b0ca20391caaf66a4d4d7897d281f9c136cd3513136945b',
    '',
    {
      name: 'Tether',
      acronym: 'USDT',
      description: 'Test description',
      unit: {
        name: 'USDT',
        decimals: 6
      },
      url: 'http://example.com',
      logo: ''
    },
  ),
  generateAsset(
    '65cn72542b0ca10391caaf66a4d4d2897d281f3c136cd3513136945b',
    '',
    {
      name: 'USD Coin',
      acronym: 'USDC',
      description: 'Test description',
      unit: {
        name: 'USDC',
        decimals: 6
      },
      url: 'http://example.com',
      logo: ''
    },
  ),
  generateAsset(
    '65bc72542b0ca20391caaf66a4d4e7897d282f9c136cd3513136945c',
    '',
    {
      name: 'MakerDAO',
      acronym: 'DAI',
      description: 'Test description',
      unit: {
        name: 'DAI',
        decimals: 6
      },
      url: 'http://example.com',
      logo: ''
    },
  ),
];

storiesOf('Wallets|Send', module)
  .addDecorator(WalletsWrapper)
  .add('Send', () => (
    <WalletSendForm
      currencyUnit="Ada"
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
    />
  ))
  .add('Send - Hardware wallet verifying transaction', () => (
    <WalletSendForm
      currencyUnit="Ada"
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
      hwDeviceStatus={HwDeviceStatuses.VERIFYING_TRANSACTION}
      isHardwareWallet={boolean('isHardwareWallet', true)}
    />
  ))
  .add('Send - Hardware wallet verifying transaction succeeded', () => (
    <WalletSendForm
      currencyUnit="Ada"
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
      hwDeviceStatus={HwDeviceStatuses.VERIFYING_TRANSACTION_SUCCEEDED}
      isHardwareWallet={boolean('isHardwareWallet', true)}
    />
  ))
  .add('Send - Hardware wallet verifying transaction failed', () => (
    <WalletSendForm
      currencyUnit="Ada"
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
      hwDeviceStatus={HwDeviceStatuses.VERIFYING_TRANSACTION_FAILED}
      isHardwareWallet={boolean('isHardwareWallet', true)}
    />
  ))
  .add('Tokens Wallet Send', () => (
    <WalletTokenSendForm
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
      selectedWallet={generateWallet('Wallet name', '45119903750165', assets)}
      nativeTokens={nativeTokens}
    />
  ))
  .add('Tokens Wallet Send Confirmation Dialog', () => (
    <div>
      <WalletTokenSendConfirmationDialog
        amount={new BigNumber(100100).toFormat(DECIMAL_PLACES_IN_ADA)}
        sender={generateWallet('Wallet name', '45119903750165', assets).id}
        receiver={generateHash()}
        receivers={[
          generateHash(),
          generateHash(),
          generateHash(),
          generateHash(),
        ]}
        transactionFee={new BigNumber(0.101).toFormat(DECIMAL_PLACES_IN_ADA)}
        amountToNaturalUnits={formattedAmountToNaturalUnits}
        onSubmit={() => null}
        isSubmitting={false}
        error={null}
        isFlight={false}
        onCancel={() => null}
        currencyUnit="USDC"
        onExternalLinkClick={() => null}
        hwDeviceStatus={HwDeviceStatuses.CONNECTING}
        isHardwareWallet={false}
        onInitiateTransaction={() => null}
        walletName={generateWallet('TrueUSD', '15119903750165', assets).name}
      />
    </div>
  ));
