import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import { boolean, number } from '@storybook/addon-knobs';
import BigNumber from 'bignumber.js';
import {
  generateHash,
  generateAssetToken,
  generateWallet,
  promise,
} from '../../_support/utils';
// Assets and helpers
import WalletsWrapper from '../_utils/WalletsWrapper';
import { NUMBER_OPTIONS } from '../../../../source/renderer/app/config/profileConfig';
import Wallet, {
  HwDeviceStatuses,
} from '../../../../source/renderer/app/domains/Wallet';
// Screens
import WalletSendForm from '../../../../source/renderer/app/components/wallet/WalletSendForm';
import type { WalletTokens } from '../../../../source/renderer/app/api/assets/types';
import { WalletSendConfirmationDialogView } from '../../../../source/renderer/app/containers/wallet/dialogs/send-confirmation/SendConfirmation.view';
import { noopAnalyticsTracker as analyticsTracker } from '../../../../source/renderer/app/analytics';

const allAssets = [
  generateAssetToken(
    '65bc72542b0ca20391caaf66a4d4e7897d282f9c136cd3513136945c',
    '',
    'tokenb0ca20391caaf66a4d4e7897d282f9c136cd3513136945c2542',
    100,
    {
      name: 'MakerDAO',
      ticker: 'DAI',
      description: 'Test description',
      url: 'http://example.com',
      logo: '',
    }
  ),
  generateAssetToken(
    '65bc72542b0ca20391caaf66a4d4d7897d281f9c136cd3513136945b',
    '',
    'tokenb0ca20391caaf66a4d4d7897d281f9c136cd3513136945b2342',
    400
  ),
  generateAssetToken(
    '65ac82542b0ca20391caaf66a4d4d7897d281f9c136cd3513136945b',
    '',
    'tokenb0ca20391caaf66a4d4d7897d281f9c136cd3513136945b2542',
    100,
    {
      name: 'Tether',
      ticker: 'USDT',
      description: 'Test description',
      url: 'http://example.com',
      logo: '',
    }
  ),
  generateAssetToken(
    '65cn72542b0ca10391caaf66a4d4d2897d281f3c136cd3513136945b',
    '',
    'tokenb0ca10391caaf66a4d4d2897d281f3c136cd3513136945b2542',
    100,
    {
      name: 'USD Coin',
      ticker: 'USDC',
      description: 'Test description',
      url: 'http://example.com',
      logo: '',
    }
  ),
];
const walletTokens: WalletTokens = {
  available: [
    {
      // @ts-ignore ts-migrate(2322) FIXME: Type '{ id: string; policyId: string; assetName: s... Remove this comment to see the full error message
      id: generateHash(),
      policyId: '65bc72542b0ca20391caaf66a4d4e7897d282f9c136cd3513136945c',
      assetName: '',
      quantity: new BigNumber(400),
      uniqueId: '65bc72542b0ca20391caaf66a4d4e7897d282f9c136cd3513136945c',
    },
    {
      // @ts-ignore ts-migrate(2322) FIXME: Type '{ id: string; policyId: string; assetName: s... Remove this comment to see the full error message
      id: generateHash(),
      policyId: '65bc72542b0ca20391caaf66a4d4d7897d281f9c136cd3513136945b',
      assetName: '',
      quantity: new BigNumber(100),
      uniqueId: '65bc72542b0ca20391caaf66a4d4d7897d281f9c136cd3513136945b',
    },
    {
      // @ts-ignore ts-migrate(2322) FIXME: Type '{ id: string; policyId: string; assetName: s... Remove this comment to see the full error message
      id: generateHash(),
      policyId: '65ac82542b0ca20391caaf66a4d4d7897d281f9c136cd3513136945b',
      assetName: '',
      quantity: new BigNumber(200),
      uniqueId: '65ac82542b0ca20391caaf66a4d4d7897d281f9c136cd3513136945b',
    },
    {
      // @ts-ignore ts-migrate(2322) FIXME: Type '{ id: string; policyId: string; assetName: s... Remove this comment to see the full error message
      id: generateHash(),
      policyId: '65cn72542b0ca10391caaf66a4d4d2897d281f3c136cd3513136945b',
      assetName: '',
      quantity: new BigNumber(300),
      uniqueId: '65cn72542b0ca10391caaf66a4d4d2897d281f3c136cd3513136945b',
    },
  ],
  total: [
    {
      // @ts-ignore ts-migrate(2322) FIXME: Type '{ id: string; policyId: string; assetName: s... Remove this comment to see the full error message
      id: generateHash(),
      policyId: '65bc72542b0ca20391caaf66a4d4e7897d282f9c136cd3513136945c',
      assetName: '',
      quantity: new BigNumber(400),
      uniqueId: '65bc72542b0ca20391caaf66a4d4e7897d282f9c136cd3513136945c',
    },
    {
      // @ts-ignore ts-migrate(2322) FIXME: Type '{ id: string; policyId: string; assetName: s... Remove this comment to see the full error message
      id: generateHash(),
      policyId: '65bc72542b0ca20391caaf66a4d4d7897d281f9c136cd3513136945b',
      assetName: '',
      quantity: new BigNumber(100),
      uniqueId: '65bc72542b0ca20391caaf66a4d4d7897d281f9c136cd3513136945b',
    },
    {
      // @ts-ignore ts-migrate(2322) FIXME: Type '{ id: string; policyId: string; assetName: s... Remove this comment to see the full error message
      id: generateHash(),
      policyId: '65ac82542b0ca20391caaf66a4d4d7897d281f9c136cd3513136945b',
      assetName: '',
      quantity: new BigNumber(200),
      uniqueId: '65ac82542b0ca20391caaf66a4d4d7897d281f9c136cd3513136945b',
    },
    {
      // @ts-ignore ts-migrate(2322) FIXME: Type '{ id: string; policyId: string; assetName: s... Remove this comment to see the full error message
      id: generateHash(),
      policyId: '65cn72542b0ca10391caaf66a4d4d2897d281f3c136cd3513136945b',
      assetName: '',
      quantity: new BigNumber(300),
      uniqueId: '65cn72542b0ca10391caaf66a4d4d2897d281f3c136cd3513136945b',
    },
  ],
};
const confirmationTokens = walletTokens.total.map((assetTotal) => {
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
    uniqueId: assetTotal.policyId + assetTotal.assetName,
    fingerprint,
    quantity: assetTotal.quantity,
    decimals: 0,
    recommendedDecimals: null,
    metadata: assetData
      ? assetData.metadata
      : {
          name: '',
          ticker: '',
          description: '',
        },
  };
});
const confirmationTokensAmounts = confirmationTokens.map(
  (token) => `${token.quantity}`
);
const sendFormAssetData = walletTokens.total.map((assetTotal) => {
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
    uniqueId: assetTotal.policyId + assetTotal.assetName,
    fingerprint,
    quantity: assetTotal.quantity,
    decimals: 0,
    recommendedDecimals: null,
    metadata: assetData
      ? assetData.metadata
      : {
          name: '',
          ticker: '',
          description: '',
        },
  };
});
storiesOf('Wallets / Send', module)
  .addDecorator(WalletsWrapper)
  .add('Send - No Assets', () => (
    <WalletSendForm
      currencyMaxFractionalDigits={6}
      currencyMaxIntegerDigits={11}
      currentNumberFormat={NUMBER_OPTIONS[0].value}
      validateAmount={promise(true)}
      validateAssetAmount={promise(true)}
      calculateTransactionFee={promise(true)}
      walletAmount={new BigNumber(123)}
      assets={sendFormAssetData}
      addressValidator={() => true}
      onSubmit={action('onSubmit')}
      isDialogOpen={() => boolean('isDialogOpen', false)}
      isRestoreActive={boolean('isRestoreActive', false)}
      hwDeviceStatus={HwDeviceStatuses.READY}
      isHardwareWallet={boolean('isHardwareWallet', false)}
      isLoadingAssets={boolean('isLoadingAssets', false)}
      onExternalLinkClick={action('onExternalLinkClick')}
      hasAssets={boolean('hasAssets', false)}
      selectedAsset={null}
      onUnsetActiveAsset={() => {}}
      isAddressFromSameWallet={boolean('isAddressFromSameWallet', false)}
      tokenFavorites={{}}
      walletName="My wallet"
      onTokenPickerDialogClose={action('onTokenPickerDialogClose')}
      onTokenPickerDialogOpen={action('onTokenPickerDialogOpen')}
      analyticsTracker={analyticsTracker}
    />
  ))
  .add('Send - Hardware wallet verifying transaction', () => (
    <WalletSendForm
      currencyMaxFractionalDigits={6}
      currencyMaxIntegerDigits={11}
      currentNumberFormat={NUMBER_OPTIONS[0].value}
      validateAmount={promise(true)}
      validateAssetAmount={promise(true)}
      calculateTransactionFee={promise(true)}
      assets={sendFormAssetData}
      addressValidator={() => true}
      onSubmit={action('onSubmit')}
      isDialogOpen={() => boolean('isDialogOpen', false)}
      isRestoreActive={boolean('isRestoreActive', false)}
      walletAmount={new BigNumber(123)}
      hwDeviceStatus={HwDeviceStatuses.VERIFYING_TRANSACTION}
      isHardwareWallet={boolean('isHardwareWallet', true)}
      isLoadingAssets={boolean('isLoadingAssets', false)}
      onExternalLinkClick={action('onExternalLinkClick')}
      hasAssets={boolean('hasAssets', false)}
      selectedAsset={null}
      onUnsetActiveAsset={() => {}}
      isAddressFromSameWallet={boolean('isAddressFromSameWallet', false)}
      tokenFavorites={{}}
      walletName="My wallet"
      onTokenPickerDialogClose={action('onTokenPickerDialogClose')}
      onTokenPickerDialogOpen={action('onTokenPickerDialogOpen')}
      analyticsTracker={analyticsTracker}
    />
  ))
  .add('Send - Hardware wallet verifying transaction succeeded', () => (
    <WalletSendForm
      currencyMaxFractionalDigits={6}
      currencyMaxIntegerDigits={11}
      currentNumberFormat={NUMBER_OPTIONS[0].value}
      validateAmount={promise(true)}
      validateAssetAmount={promise(true)}
      calculateTransactionFee={promise(true)}
      assets={sendFormAssetData}
      addressValidator={() => true}
      onSubmit={action('onSubmit')}
      isDialogOpen={() => boolean('isDialogOpen', false)}
      isRestoreActive={boolean('isRestoreActive', false)}
      walletAmount={new BigNumber(123)}
      hwDeviceStatus={HwDeviceStatuses.VERIFYING_TRANSACTION_SUCCEEDED}
      isHardwareWallet={boolean('isHardwareWallet', true)}
      isLoadingAssets={boolean('isLoadingAssets', false)}
      onExternalLinkClick={action('onExternalLinkClick')}
      hasAssets={boolean('hasAssets', false)}
      selectedAsset={null}
      onUnsetActiveAsset={() => {}}
      isAddressFromSameWallet={boolean('isAddressFromSameWallet', false)}
      tokenFavorites={{}}
      walletName="My wallet"
      onTokenPickerDialogClose={action('onTokenPickerDialogClose')}
      onTokenPickerDialogOpen={action('onTokenPickerDialogOpen')}
      analyticsTracker={analyticsTracker}
    />
  ))
  .add('Send - Hardware wallet verifying transaction failed', () => (
    <WalletSendForm
      currencyMaxFractionalDigits={6}
      currencyMaxIntegerDigits={11}
      currentNumberFormat={NUMBER_OPTIONS[0].value}
      validateAmount={promise(true)}
      validateAssetAmount={promise(true)}
      calculateTransactionFee={promise(true)}
      assets={sendFormAssetData}
      addressValidator={() => true}
      onSubmit={action('onSubmit')}
      isDialogOpen={() => boolean('isDialogOpen', false)}
      isRestoreActive={boolean('isRestoreActive', false)}
      walletAmount={new BigNumber(123)}
      hwDeviceStatus={HwDeviceStatuses.VERIFYING_TRANSACTION_FAILED}
      isHardwareWallet={boolean('isHardwareWallet', true)}
      isLoadingAssets={boolean('isLoadingAssets', false)}
      onExternalLinkClick={action('onExternalLinkClick')}
      hasAssets={boolean('hasAssets', false)}
      selectedAsset={null}
      onUnsetActiveAsset={() => {}}
      isAddressFromSameWallet={boolean('isAddressFromSameWallet', false)}
      tokenFavorites={{}}
      walletName="My wallet"
      onTokenPickerDialogClose={action('onTokenPickerDialogClose')}
      onTokenPickerDialogOpen={action('onTokenPickerDialogOpen')}
      analyticsTracker={analyticsTracker}
    />
  ))
  .add('Send - With Assets', () => (
    <WalletSendForm
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
      onSubmit={action('onSubmit')}
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
      onUnsetActiveAsset={() => {}}
      isAddressFromSameWallet={boolean('isAddressFromSameWallet', false)}
      tokenFavorites={{}}
      walletName="My wallet"
      onTokenPickerDialogClose={action('onTokenPickerDialogClose')}
      onTokenPickerDialogOpen={action('onTokenPickerDialogOpen')}
      analyticsTracker={analyticsTracker}
    />
  ))
  .add('Wallet Send Confirmation Dialog With Assets', () => {
    // @ts-ignore[prop-missing]
    const wallet: Wallet = {
      name: generateWallet('TrueUSD', '15119903750165', walletTokens).name,
      amount: new BigNumber(100),
      isDelegating: true,
    };
    return (
      <div>
        <WalletSendConfirmationDialogView
          amount="20.000000"
          areTermsAccepted={boolean('areTermsAccepted', true)}
          wallet={wallet}
          totalAmount={new BigNumber('21.000000')}
          receiver={generateHash()}
          selectedAssets={confirmationTokens}
          assetTokens={confirmationTokens}
          assetsAmounts={confirmationTokensAmounts}
          transactionFee="1.000000"
          hwDeviceStatus={HwDeviceStatuses.CONNECTING}
          isFlight={boolean('isFlight', false)}
          isHardwareWallet={boolean('isHardwareWallet', false)}
          isSubmitting={boolean('isSubmitting', false)}
          isTrezor={boolean('isTrezor', false)}
          formattedTotalAmount="21.000000"
          error={null}
          onCancel={action('onCancel')}
          onSubmitCb={action('onSubmitCb')}
          onTermsCheckboxClick={action('onTermsCheckboxClick')}
          onCopyAssetParam={action('onCopyAssetParam')}
          onExternalLinkClick={action('onExternalLinkClick')}
        />
      </div>
    );
  })
  .add('Wallet Send Confirmation Dialog With No Assets', () => {
    // @ts-ignore[prop-missing]
    const wallet: Wallet = {
      name: generateWallet('TrueUSD', '15119903750165', walletTokens).name,
      amount: new BigNumber(100),
      isDelegating: true,
    };
    return (
      <div>
        <WalletSendConfirmationDialogView
          wallet={wallet}
          receiver={generateHash()}
          amount="20.000000"
          totalAmount={new BigNumber('21.000000')}
          transactionFee="1.000000"
          selectedAssets={[]}
          assetTokens={confirmationTokens}
          assetsAmounts={confirmationTokensAmounts}
          hwDeviceStatus={HwDeviceStatuses.CONNECTING}
          areTermsAccepted={boolean('areTermsAccepted', true)}
          isFlight={boolean('isFlight', false)}
          isTrezor={boolean('isTrezor', false)}
          isSubmitting={boolean('isSubmitting', false)}
          isHardwareWallet={boolean('isHardwareWallet', false)}
          formattedTotalAmount="21.000000"
          error={null}
          onCancel={action('onCancel')}
          onSubmitCb={action('onSubmitCb')}
          onTermsCheckboxClick={action('onTermsCheckboxClick')}
          onCopyAssetParam={action('onCopyAssetParam')}
          onExternalLinkClick={action('onExternalLinkClick')}
        />
      </div>
    );
  });
