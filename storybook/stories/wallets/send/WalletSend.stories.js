// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import { boolean, number } from '@storybook/addon-knobs';
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
import WalletSendForm from '../../../../source/renderer/app/components/wallet/WalletSendForm';
import WalletSendAssetsConfirmationDialog from '../../../../source/renderer/app/components/wallet/send-form/WalletSendAssetsConfirmationDialog';
import WalletSendConfirmationDialog from '../../../../source/renderer/app/components/wallet/send-form/WalletSendConfirmationDialog';
import { formattedAmountToNaturalUnits } from '../../../../source/renderer/app/utils/formatters';

const allAssets = [
  generateAsset(
    '65bc72542b0ca20391caaf66a4d4e7897d282f9c136cd3513136945c',
    '',
    'tokenb0ca20391caaf66a4d4e7897d282f9c136cd3513136945c2542',
    {
      name: 'MakerDAO',
      ticker: 'DAI',
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
      ticker: 'USDT',
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
      ticker: 'USDC',
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
      quantity: new BigNumber(400),
    },
    {
      id: generateHash(),
      policyId: '65bc72542b0ca20391caaf66a4d4d7897d281f9c136cd3513136945b',
      assetName: '',
      quantity: new BigNumber(100),
    },
    {
      id: generateHash(),
      policyId: '65ac82542b0ca20391caaf66a4d4d7897d281f9c136cd3513136945b',
      assetName: '',
      quantity: new BigNumber(200),
    },
    {
      id: generateHash(),
      policyId: '65cn72542b0ca10391caaf66a4d4d2897d281f3c136cd3513136945b',
      assetName: '',
      quantity: new BigNumber(300),
    },
  ],
  total: [
    {
      id: generateHash(),
      policyId: '65bc72542b0ca20391caaf66a4d4e7897d282f9c136cd3513136945c',
      assetName: '',
      quantity: new BigNumber(400),
    },
    {
      id: generateHash(),
      policyId: '65bc72542b0ca20391caaf66a4d4d7897d281f9c136cd3513136945b',
      assetName: '',
      quantity: new BigNumber(100),
    },
    {
      id: generateHash(),
      policyId: '65ac82542b0ca20391caaf66a4d4d7897d281f9c136cd3513136945b',
      assetName: '',
      quantity: new BigNumber(200),
    },
    {
      id: generateHash(),
      policyId: '65cn72542b0ca10391caaf66a4d4d2897d281f3c136cd3513136945b',
      assetName: '',
      quantity: new BigNumber(300),
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
          ticker: '',
          description: '',
        },
  };
});

const confirmationAssetsAmounts = confirmationAssets.map(
  (asset) => `${asset.quantity}`
);

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
          ticker: '',
          description: '',
        },
  };
});

storiesOf('Wallets|Send', module)
  .addDecorator(WalletsWrapper)
  .add('Send - No Assets', () => (
    <WalletSendForm
      currencyUnit="Ada"
      currencyMaxFractionalDigits={6}
      currencyMaxIntegerDigits={11}
      currentNumberFormat={NUMBER_OPTIONS[0].value}
      validateAmount={promise(true)}
      validateAssetAmount={promise(true)}
      calculateTransactionFee={promise(true)}
      walletAmount={new BigNumber(123)}
      assets={sendFormAssetData}
      addressValidator={() => true}
      onOpenDialogAction={action('onOpenDialogAction')}
      isDialogOpen={() => boolean('isDialogOpen', false)}
      isRestoreActive={boolean('isRestoreActive', false)}
      hwDeviceStatus={HwDeviceStatuses.READY}
      isHardwareWallet={boolean('isHardwareWallet', false)}
      isLoadingAssets={boolean('isLoadingAssets', false)}
      onExternalLinkClick={action('onExternalLinkClick')}
      hasAssets={boolean('hasAssets', false)}
      selectedAsset={null}
      onUnsetActiveAssetFingerprint={() => {}}
    />
  ))
  .add('Send - Hardware wallet verifying transaction', () => (
    <WalletSendForm
      currencyUnit="Ada"
      currencyMaxFractionalDigits={6}
      currencyMaxIntegerDigits={11}
      currentNumberFormat={NUMBER_OPTIONS[0].value}
      validateAmount={promise(true)}
      validateAssetAmount={promise(true)}
      calculateTransactionFee={promise(true)}
      assets={sendFormAssetData}
      addressValidator={() => true}
      onOpenDialogAction={action('onOpenDialogAction')}
      isDialogOpen={() => boolean('isDialogOpen', false)}
      isRestoreActive={boolean('isRestoreActive', false)}
      walletAmount={new BigNumber(123)}
      hwDeviceStatus={HwDeviceStatuses.VERIFYING_TRANSACTION}
      isHardwareWallet={boolean('isHardwareWallet', true)}
      isLoadingAssets={boolean('isLoadingAssets', false)}
      onExternalLinkClick={action('onExternalLinkClick')}
      hasAssets={boolean('hasAssets', false)}
      selectedAsset={null}
      onUnsetActiveAssetFingerprint={() => {}}
    />
  ))
  .add('Send - Hardware wallet verifying transaction succeeded', () => (
    <WalletSendForm
      currencyUnit="Ada"
      currencyMaxFractionalDigits={6}
      currencyMaxIntegerDigits={11}
      currentNumberFormat={NUMBER_OPTIONS[0].value}
      validateAmount={promise(true)}
      validateAssetAmount={promise(true)}
      calculateTransactionFee={promise(true)}
      assets={sendFormAssetData}
      addressValidator={() => true}
      onOpenDialogAction={action('onOpenDialogAction')}
      isDialogOpen={() => boolean('isDialogOpen', false)}
      isRestoreActive={boolean('isRestoreActive', false)}
      walletAmount={new BigNumber(123)}
      hwDeviceStatus={HwDeviceStatuses.VERIFYING_TRANSACTION_SUCCEEDED}
      isHardwareWallet={boolean('isHardwareWallet', true)}
      isLoadingAssets={boolean('isLoadingAssets', false)}
      onExternalLinkClick={action('onExternalLinkClick')}
      hasAssets={boolean('hasAssets', false)}
      selectedAsset={null}
      onUnsetActiveAssetFingerprint={() => {}}
    />
  ))
  .add('Send - Hardware wallet verifying transaction failed', () => (
    <WalletSendForm
      currencyUnit="Ada"
      currencyMaxFractionalDigits={6}
      currencyMaxIntegerDigits={11}
      currentNumberFormat={NUMBER_OPTIONS[0].value}
      validateAmount={promise(true)}
      validateAssetAmount={promise(true)}
      calculateTransactionFee={promise(true)}
      assets={sendFormAssetData}
      addressValidator={() => true}
      onOpenDialogAction={action('onOpenDialogAction')}
      isDialogOpen={() => boolean('isDialogOpen', false)}
      isRestoreActive={boolean('isRestoreActive', false)}
      walletAmount={new BigNumber(123)}
      hwDeviceStatus={HwDeviceStatuses.VERIFYING_TRANSACTION_FAILED}
      isHardwareWallet={boolean('isHardwareWallet', true)}
      isLoadingAssets={boolean('isLoadingAssets', false)}
      onExternalLinkClick={action('onExternalLinkClick')}
      hasAssets={boolean('hasAssets', false)}
      selectedAsset={null}
      onUnsetActiveAssetFingerprint={() => {}}
    />
  ))
  .add('Send - With Assets', () => (
    <WalletSendForm
      currencyUnit="Ada"
      currencyMaxFractionalDigits={6}
      currencyMaxIntegerDigits={11}
      currentNumberFormat={NUMBER_OPTIONS[0].value}
      validateAmount={promise(true)}
      validateAssetAmount={promise(true)}
      calculateTransactionFee={promise({
        fee: new BigNumber(number('fee', 1)),
        minimumAda: new BigNumber(number('minimumAda', 1)),
      })}
      addressValidator={() => true}
      onOpenDialogAction={action('onOpenDialogAction')}
      isDialogOpen={() => boolean('isDialogOpen', false)}
      isRestoreActive={boolean('isRestoreActive', false)}
      hwDeviceStatus={HwDeviceStatuses.READY}
      isHardwareWallet={boolean('isHardwareWallet', false)}
      isLoadingAssets={boolean('isLoadingAssets', false)}
      assets={sendFormAssetData}
      walletAmount={new BigNumber(123)}
      onExternalLinkClick={action('onExternalLinkClick')}
      hasAssets={boolean('hasAssets', true)}
      selectedAsset={null}
      onUnsetActiveAssetFingerprint={() => {}}
    />
  ))
  .add('Wallet Send Confirmation Dialog With Assets', () => (
    <div>
      <WalletSendAssetsConfirmationDialog
        currencyUnit="Ada"
        amount="20.000000"
        totalAmount="21.000000"
        sender={generateWallet('Wallet name', '45119903750165', assets).id}
        receiver={generateHash()}
        assets={confirmationAssets}
        assetsAmounts={confirmationAssetsAmounts}
        transactionFee="1.000000"
        amountToNaturalUnits={formattedAmountToNaturalUnits}
        onSubmit={() => null}
        isSubmitting={false}
        error={null}
        isFlight={false}
        onCancel={() => null}
        onExternalLinkClick={() => null}
        hwDeviceStatus={HwDeviceStatuses.CONNECTING}
        isHardwareWallet={boolean('isHardwareWallet', false)}
        onInitiateTransaction={() => null}
        walletName={generateWallet('TrueUSD', '15119903750165', assets).name}
        onCopyAssetItem={() => {}}
        isTrezor={boolean('isTrezor', false)}
      />
    </div>
  ))
  .add('Wallet Send Confirmation Dialog With No Assets', () => (
    <div>
      <WalletSendConfirmationDialog
        amount="20.000000"
        totalAmount="21.000000"
        currencyUnit="ADA"
        sender={generateWallet('Wallet name', '45119903750165', assets).id}
        receiver={generateHash()}
        transactionFee="1.000000"
        amountToNaturalUnits={formattedAmountToNaturalUnits}
        onSubmit={() => null}
        isSubmitting={false}
        error={null}
        isFlight={false}
        onCancel={() => null}
        onExternalLinkClick={() => null}
        hwDeviceStatus={HwDeviceStatuses.CONNECTING}
        isHardwareWallet={boolean('isHardwareWallet', false)}
        onInitiateTransaction={() => null}
        walletName={generateWallet('TrueUSD', '15119903750165', assets).name}
        isTrezor={boolean('isTrezor', false)}
      />
    </div>
  ));
