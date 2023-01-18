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

const coinSelectionResponse = {
  inputs: {
    address: '',
    amount: {
      quantity: 1,
      unit: 'lovelace',
    },
    id: '001',
    index: 1,
    derivationPath: [''],
  },
  outputs: {
    address: '',
    amount: {
      quantity: 1,
      unit: 'lovelace',
    },
    derivationPath: [''],
    assets: [
      {
        policyId: '',
        assetName: 'NFT',
        quantity: 1,
      },
    ],
  },
  certificates: [
    {
      pool: '',
      certificateType: 'register_reward_account',
      rewardAccountPath: [''],
    },
  ],
  deposits: new BigNumber(10),
  depositsReclaimed: new BigNumber(1),
  withdrawals: [],
  fee: new BigNumber(0.2),
  metadata: null,
};

const formData = {
  coinSelection: coinSelectionResponse,
  receiver:
    'addr1qzhk85furdn6r9tlyp2q23q9vq7nfl420j7y0yqp3hf6yw7jar5rnzqr4h3g9whm0zjh65utc2ty5uqtcpm0rm7ahj0qq75een',
  selectedAssets: [
    {
      assetName: 'coin',
      fingerprint: 'urdn6r9tlyp2q23q9vq7nfl420',
      uniqueId: 'u000',
      policyId: '003',
      quantity: new BigNumber(2),
    },
  ],
  assetsAmounts: [''],
  amount: new BigNumber(20),
  totalAmount: new BigNumber(20),
  transactionFee: new BigNumber(20),
  adaAmount: 20,
  selectedAddress:
    'addr1qzhk85furdn6r9tlyp2q23q9vq7nfl420j7y0yqp3hf6yw7jar5rnzqr4h3g9whm0zjh65utc2ty5uqtcpm0rm7ahj0qq75ytr',
};

const selectedAsset = {
  assetName: '',
  assetNameASCII: '',
  decimals: 0,
  fingerprint: 'tokenb0ca20391caaf66a4d4e7897d282f9c136cd3513136945c2542',
  metadata: {
    name: 'MakerDAO',
    ticker: 'DAI',
    description: 'Test description',
    url: 'http://example.com',
    logo: '',
  },
  policyId: '65bc72542b0ca20391caaf66a4d4e7897d282f9c136cd3513136945c',
  quantity: new BigNumber(4),
  recommendedDecimals: null,
  uniqueId: '65bc72542b0ca20391caaf66a4d4e7897d282f9c136cd3513136945c',
  update: () => {},
};

storiesOf('Wallets / Send', module)
  .addDecorator(WalletsWrapper)
  .add('Send - Send screen', () => (
    <WalletSendForm
      currencyMaxFractionalDigits={6}
      currencyMaxIntegerDigits={11}
      currentNumberFormat={NUMBER_OPTIONS[0].value}
      validateAmount={promise(true)}
      validateAssetAmount={promise(true)}
      calculateTransactionFee={promise(true)}
      walletAmount={new BigNumber(123)}
      addressValidator={() => true}
      onSubmit={action('onSubmit')}
      isRestoreActive={boolean('isRestoreActive', false)}
      hwDeviceStatus={HwDeviceStatuses.READY}
      isDialogOpen={() => false}
      hasAssets={false}
      selectedAsset={null}
      assets={[]}
      isHardwareWallet
      isLoadingAssets={boolean('isLoadingAssets', false)}
      onExternalLinkClick={action('onExternalLinkClick')}
      onUnsetActiveAsset={() => {}}
      isAddressFromSameWallet={false}
      tokenFavorites={{}}
      walletName="My wallet"
      onTokenPickerDialogClose={action('onTokenPickerDialogClose')}
      onTokenPickerDialogOpen={action('onTokenPickerDialogOpen')}
      analyticsTracker={analyticsTracker}
      confirmationDialogData={formData}
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
      isRestoreActive={false}
      walletAmount={new BigNumber(123)}
      isDialogOpen={(view) => view === WalletSendConfirmationDialogView}
      hwDeviceStatus={HwDeviceStatuses.VERIFYING_TRANSACTION}
      isHardwareWallet
      isLoadingAssets={false}
      onExternalLinkClick={action('onExternalLinkClick')}
      hasAssets
      selectedAsset={null}
      onUnsetActiveAsset={() => {}}
      isAddressFromSameWallet={false}
      tokenFavorites={{}}
      walletName="My wallet"
      onTokenPickerDialogClose={action('onTokenPickerDialogClose')}
      onTokenPickerDialogOpen={action('onTokenPickerDialogOpen')}
      analyticsTracker={analyticsTracker}
      confirmationDialogData={formData}
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
      isDialogOpen={(view) => view === WalletSendConfirmationDialogView}
      isRestoreActive={false}
      walletAmount={new BigNumber(123)}
      hwDeviceStatus={HwDeviceStatuses.VERIFYING_TRANSACTION_SUCCEEDED}
      isHardwareWallet
      isLoadingAssets={false}
      onExternalLinkClick={action('onExternalLinkClick')}
      hasAssets
      selectedAsset={null}
      onUnsetActiveAsset={() => {}}
      isAddressFromSameWallet={boolean('isAddressFromSameWallet', false)}
      tokenFavorites={{}}
      walletName="My wallet"
      onTokenPickerDialogClose={action('onTokenPickerDialogClose')}
      onTokenPickerDialogOpen={action('onTokenPickerDialogOpen')}
      analyticsTracker={analyticsTracker}
      confirmationDialogData={formData}
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
      isDialogOpen={(view) => view === WalletSendConfirmationDialogView}
      isRestoreActive={false}
      walletAmount={new BigNumber(123)}
      hwDeviceStatus={HwDeviceStatuses.VERIFYING_TRANSACTION_FAILED}
      isHardwareWallet
      isLoadingAssets={false}
      onExternalLinkClick={action('onExternalLinkClick')}
      hasAssets
      selectedAsset={null}
      onUnsetActiveAsset={() => {}}
      isAddressFromSameWallet={boolean('isAddressFromSameWallet', false)}
      tokenFavorites={{}}
      walletName="My wallet"
      onTokenPickerDialogClose={action('onTokenPickerDialogClose')}
      onTokenPickerDialogOpen={action('onTokenPickerDialogOpen')}
      analyticsTracker={analyticsTracker}
      confirmationDialogData={formData}
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
