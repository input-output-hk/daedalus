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
  promise,
} from '../../_support/utils';

// Assets and helpers
import WalletsWrapper from '../_utils/WalletsWrapper';
import { NUMBER_OPTIONS } from '../../../../source/renderer/app/config/profileConfig';
import { HwDeviceStatuses } from '../../../../source/renderer/app/domains/Wallet';

// Screens
import WalletAssetsSendForm from '../../../../source/renderer/app/components/wallet/WalletAssetsSendForm';
import WalletAssetsSendConfirmationDialog from '../../../../source/renderer/app/components/wallet/WalletAssetsSendConfirmationDialog';
import { DECIMAL_PLACES_IN_ADA } from '../../../../source/renderer/app/config/numbersConfig';
import { formattedAmountToNaturalUnits } from '../../../../source/renderer/app/utils/formatters';

const allAssets = [
  generateAsset(
    '65bc72542b0ca20391caaf66a4d4e7897d282f9c136cd3513136945c',
    '',
    'tokenb0ca20391caaf66a4d4e7897d282f9c136cd3513136945c2542',
    {
      name: 'MakerDAO',
      acronym: 'DAI',
      description: 'Test description',
      unit: {
        name: 'DAI',
        decimals: 6,
      },
      url: 'http://example.com',
      logo: '',
    }
  ),
  generateAsset(
    '65bc72542b0ca20391caaf66a4d4d7897d281f9c136cd3513136945b',
    '',
    'tokenb0ca20391caaf66a4d4d7897d281f9c136cd3513136945b2342'
  ),
  generateAsset(
    '65ac82542b0ca20391caaf66a4d4d7897d281f9c136cd3513136945b',
    '',
    'tokenb0ca20391caaf66a4d4d7897d281f9c136cd3513136945b2542',
    {
      name: 'Tether',
      acronym: 'USDT',
      description: 'Test description',
      unit: {
        name: 'USDT',
        decimals: 6,
      },
      url: 'http://example.com',
      logo: '',
    }
  ),
  generateAsset(
    '65cn72542b0ca10391caaf66a4d4d2897d281f3c136cd3513136945b',
    '',
    'tokenb0ca10391caaf66a4d4d2897d281f3c136cd3513136945b2542',
    {
      name: 'USD Coin',
      acronym: 'USDC',
      description: 'Test description',
      unit: {
        name: 'USDC',
        decimals: 6,
      },
      url: 'http://example.com',
      logo: '',
    }
  ),
];

const assets = {
  available: [
    {
      id: generateHash(),
      policyId: '65bc72542b0ca20391caaf66a4d4e7897d282f9c136cd3513136945c',
      assetName: '',
      quantity: 400,
    },
    {
      id: generateHash(),
      policyId: '65bc72542b0ca20391caaf66a4d4d7897d281f9c136cd3513136945b',
      assetName: '',
      quantity: 100,
    },
    {
      id: generateHash(),
      policyId: '65ac82542b0ca20391caaf66a4d4d7897d281f9c136cd3513136945b',
      assetName: '',
      quantity: 200,
    },
    {
      id: generateHash(),
      policyId: '65cn72542b0ca10391caaf66a4d4d2897d281f3c136cd3513136945b',
      assetName: '',
      quantity: 300,
    },
  ],
  total: [
    {
      id: generateHash(),
      policyId: '65bc72542b0ca20391caaf66a4d4e7897d282f9c136cd3513136945c',
      assetName: '',
      quantity: 400,
    },
    {
      id: generateHash(),
      policyId: '65bc72542b0ca20391caaf66a4d4d7897d281f9c136cd3513136945b',
      assetName: '',
      quantity: 100,
    },
    {
      id: generateHash(),
      policyId: '65ac82542b0ca20391caaf66a4d4d7897d281f9c136cd3513136945b',
      assetName: '',
      quantity: 200,
    },
    {
      id: generateHash(),
      policyId: '65cn72542b0ca10391caaf66a4d4d2897d281f3c136cd3513136945b',
      assetName: '',
      quantity: 300,
    },
  ],
};

const confirmationAssets = assets.total.map((assetTotal) => {
  const assetData = allAssets.find(
    (item) => item.policyId === assetTotal.policyId
  );
  let fingerprint;
  if (!assetData || !assetData.fingerprint) {
    fingerprint = `token${assetTotal.policyId}${assetTotal.assetName}`.substr(
      0,
      44
    );
  } else {
    fingerprint = assetData.fingerprint;
  }

  return {
    policyId: assetTotal.policyId,
    assetName: assetTotal.assetName,
    fingerprint,
    quantity: assetTotal.quantity,
    metadata: assetData
      ? assetData.metadata
      : {
          name: '',
          acronym: '',
          description: '',
        },
  };
});

const sendFormAssetData = assets.total.map((assetTotal) => {
  const assetData = allAssets.find(
    (item) => item.policyId === assetTotal.policyId
  );
  let fingerprint;
  if (!assetData || !assetData.fingerprint) {
    fingerprint = `token${assetTotal.policyId}${assetTotal.assetName}`.substr(
      0,
      44
    );
  } else {
    fingerprint = assetData.fingerprint;
  }

  return {
    policyId: assetTotal.policyId,
    assetName: assetTotal.assetName,
    fingerprint,
    quantity: assetTotal.quantity,
    metadata: assetData
      ? assetData.metadata
      : {
          name: '',
          acronym: '',
          description: '',
        },
  };
});

storiesOf('Wallets|Send', module)
  .addDecorator(WalletsWrapper)
  .add('Send - No Assets', () => (
    <WalletAssetsSendForm
      currencyUnit="Ada"
      currencyMaxFractionalDigits={6}
      currencyMaxIntegerDigits={11}
      currentNumberFormat={NUMBER_OPTIONS[0].value}
      validateAmount={promise(true)}
      calculateTransactionFee={promise(true)}
      walletAmount={new BigNumber(123)}
      assets={sendFormAssetData}
      addressValidator={action('addressValidator')}
      openDialogAction={action('openDialogAction')}
      isDialogOpen={() => boolean('isDialogOpen', false)}
      isRestoreActive={boolean('isRestoreActive', false)}
      hwDeviceStatus={HwDeviceStatuses.READY}
      isHardwareWallet={boolean('isHardwareWallet', false)}
      onExternalLinkClick={action('onExternalLinkClick')}
      hasAssets={boolean('hasAssets', false)}
    />
  ))
  .add('Send - Hardware wallet verifying transaction', () => (
    <WalletAssetsSendForm
      currencyUnit="Ada"
      currencyMaxFractionalDigits={6}
      currencyMaxIntegerDigits={11}
      currentNumberFormat={NUMBER_OPTIONS[0].value}
      validateAmount={promise(true)}
      calculateTransactionFee={promise(true)}
      assets={sendFormAssetData}
      addressValidator={action('addressValidator')}
      openDialogAction={action('openDialogAction')}
      isDialogOpen={() => boolean('isDialogOpen', false)}
      isRestoreActive={boolean('isRestoreActive', false)}
      walletAmount={new BigNumber(123)}
      hwDeviceStatus={HwDeviceStatuses.VERIFYING_TRANSACTION}
      isHardwareWallet={boolean('isHardwareWallet', true)}
      onExternalLinkClick={action('onExternalLinkClick')}
      hasAssets={boolean('hasAssets', false)}
    />
  ))
  .add('Send - Hardware wallet verifying transaction succeeded', () => (
    <WalletAssetsSendForm
      currencyUnit="Ada"
      currencyMaxFractionalDigits={6}
      currencyMaxIntegerDigits={11}
      currentNumberFormat={NUMBER_OPTIONS[0].value}
      validateAmount={promise(true)}
      calculateTransactionFee={promise(true)}
      assets={sendFormAssetData}
      addressValidator={action('addressValidator')}
      openDialogAction={action('openDialogAction')}
      isDialogOpen={() => boolean('isDialogOpen', false)}
      isRestoreActive={boolean('isRestoreActive', false)}
      walletAmount={new BigNumber(123)}
      hwDeviceStatus={HwDeviceStatuses.VERIFYING_TRANSACTION_SUCCEEDED}
      isHardwareWallet={boolean('isHardwareWallet', true)}
      onExternalLinkClick={action('onExternalLinkClick')}
      hasAssets={boolean('hasAssets', false)}
    />
  ))
  .add('Send - Hardware wallet verifying transaction failed', () => (
    <WalletAssetsSendForm
      currencyUnit="Ada"
      currencyMaxFractionalDigits={6}
      currencyMaxIntegerDigits={11}
      currentNumberFormat={NUMBER_OPTIONS[0].value}
      validateAmount={promise(true)}
      calculateTransactionFee={promise(true)}
      assets={sendFormAssetData}
      addressValidator={action('addressValidator')}
      openDialogAction={action('openDialogAction')}
      isDialogOpen={() => boolean('isDialogOpen', false)}
      isRestoreActive={boolean('isRestoreActive', false)}
      walletAmount={new BigNumber(123)}
      hwDeviceStatus={HwDeviceStatuses.VERIFYING_TRANSACTION_FAILED}
      isHardwareWallet={boolean('isHardwareWallet', true)}
      onExternalLinkClick={action('onExternalLinkClick')}
      hasAssets={boolean('hasAssets', false)}
    />
  ))
  .add('Send - With Assets', () => (
    <WalletAssetsSendForm
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
      hwDeviceStatus={HwDeviceStatuses.READY}
      isHardwareWallet={boolean('isHardwareWallet', false)}
      assets={sendFormAssetData}
      walletAmount={new BigNumber(123)}
      onExternalLinkClick={action('onExternalLinkClick')}
      hasAssets={boolean('hasAssets', true)}
    />
  ))
  .add('Wallet Send Confirmation Dialog With Assets', () => (
    <div>
      <WalletAssetsSendConfirmationDialog
        amount={new BigNumber(100100).toFormat(DECIMAL_PLACES_IN_ADA)}
        sender={generateWallet('Wallet name', '45119903750165', assets).id}
        receivers={[
          generateHash(),
          generateHash(),
          generateHash(),
          generateHash(),
        ]}
        assets={confirmationAssets}
        transactionFee={new BigNumber(1).toFormat(DECIMAL_PLACES_IN_ADA)}
        amountToNaturalUnits={formattedAmountToNaturalUnits}
        onSubmit={() => null}
        isSubmitting={false}
        error={null}
        isFlight={false}
        onCancel={() => null}
        onExternalLinkClick={() => null}
        hwDeviceStatus={HwDeviceStatuses.CONNECTING}
        isHardwareWallet={false}
        onInitiateTransaction={() => null}
        walletName={generateWallet('TrueUSD', '15119903750165', assets).name}
      />
    </div>
  ));
