// @flow
import React from 'react';
import { select, boolean, number } from '@storybook/addon-knobs';
import BigNumber from 'bignumber.js';
import { action } from '@storybook/addon-actions';

// Screens
import Step1ConfigurationDialog from '../../../source/renderer/app/components/staking/redeem-itn-rewards/Step1ConfigurationDialog';
import Step2ConfirmationDialog from '../../../source/renderer/app/components/staking/redeem-itn-rewards/Step2ConfirmationDialog';
import Step3SuccessDialog from '../../../source/renderer/app/components/staking/redeem-itn-rewards/Step3SuccessDialog';
import Step3FailureDialog from '../../../source/renderer/app/components/staking/redeem-itn-rewards/Step3FailureDialog';
import NoWalletsDialog from '../../../source/renderer/app/components/staking/redeem-itn-rewards/NoWalletsDialog';
import RedemptionUnavailableDialog from '../../../source/renderer/app/components/staking/redeem-itn-rewards/RedemptionUnavailableDialog';

// Helpers
import { isValidMnemonic } from '../../../source/common/config/crypto/decrypt';
import validWords from '../../../source/common/config/crypto/valid-words.en';
import {
  generateHash,
  generatePolicyIdHash,
  generateWallet,
} from '../_support/utils';

const assets = {
  available: [
    {
      id: generateHash(),
      policyId: generatePolicyIdHash(),
      uniqueId: generatePolicyIdHash(),
      assetName: '',
      quantity: new BigNumber(200),
    },
    {
      id: generateHash(),
      policyId: generatePolicyIdHash(),
      uniqueId: generatePolicyIdHash(),
      assetName: '',
      quantity: new BigNumber(200),
    },
  ],
  total: [
    {
      id: generateHash(),
      policyId: generatePolicyIdHash(),
      uniqueId: generatePolicyIdHash(),
      assetName: '',
      quantity: new BigNumber(200),
    },
    {
      id: generateHash(),
      policyId: generatePolicyIdHash(),
      uniqueId: generatePolicyIdHash(),
      assetName: '',
      quantity: new BigNumber(200),
    },
  ],
};

const WALLETS = [
  generateWallet('First Wallet', '1000000000', assets),
  generateWallet(
    'Second Wallet',
    '500000000',
    assets,
    0,
    undefined,
    true,
    'syncing'
  ),
  generateWallet('Third Wallet', '100000000', assets),
  generateWallet('Fourth Wallet', '50000000', assets),
  generateWallet('Fifth Wallet', '7000000', assets),
];

// 'Dummy2',
// '2000000000000',
// assets,
// 0,
// undefined,
// true,
// WalletSyncStateStatuses.SYNCING

export const Step1ConfigurationDialogStory = () => {
  const redeemWallet = select(
    'Redeem Wallet',
    WALLETS.reduce((obj, wallet) => {
      obj[wallet.name] = wallet;
      return obj;
    }, {}),
    WALLETS[0]
  );
  return (
    <Step1ConfigurationDialog
      key="Step1ConfigurationDialog"
      wallets={WALLETS}
      wallet={redeemWallet}
      isWalletValid={boolean('isWalletValid')}
      isCalculatingReedemFees={boolean('isCalculatingReedemFees')}
      syncPercentage={99.55}
      mnemonicValidator={isValidMnemonic}
      onSelectWallet={action('onSelectWallet')}
      onClose={action('onClose')}
      onContinue={action('onContinue')}
      onBack={action('onBack')}
      openExternalLink={action('openExternalLink')}
      suggestedMnemonics={validWords}
    />
  );
};
export const Step2ConfirmationDialogStory = () => {
  const redeemWallet = select(
    'Redeem Wallet',
    WALLETS.reduce((obj, wallet) => {
      obj[wallet.name] = wallet;
      return obj;
    }, {}),
    WALLETS[0]
  );
  return (
    <Step2ConfirmationDialog
      key="Step2ConfirmationDialog"
      wallet={redeemWallet}
      transactionFees={new BigNumber(number('transactionFees', 100000))}
      redeemedRewards={new BigNumber(number('redeemedRewards', 100000))}
      onContinue={action('onContinue')}
      onClose={action('onClose')}
      onBack={action('onBack')}
      isSubmitting={boolean('isSubmitting', false)}
    />
  );
};
export const Step3SuccessDialogStory = () => {
  const redeemWallet = select(
    'Redeem Wallet',
    WALLETS.reduce((obj, wallet) => {
      obj[wallet.name] = wallet;
      return obj;
    }, {}),
    WALLETS[0]
  );
  return (
    <Step3SuccessDialog
      key="Step2ConfirmationDialog"
      wallet={redeemWallet}
      transactionFees={new BigNumber(number('transactionFees', 100000))}
      redeemedRewards={new BigNumber(number('redeemedRewards', 100000))}
      onContinue={action('onContinue')}
      onClose={action('onClose')}
    />
  );
};
export const Step3FailureDialogStory = () => {
  return (
    <Step3FailureDialog onClose={action('onClose')} onBack={action('onBack')} />
  );
};

export const NoWalletsDialogDialogStory = () => {
  return (
    <NoWalletsDialog
      onClose={action('onClose')}
      onAddWallet={action('onAddWallet')}
    />
  );
};

export const RedemptionUnavailableDialogDialogStory = () => {
  return (
    <RedemptionUnavailableDialog
      onClose={action('onClose')}
      syncPercentage={number('syncPercentage', 37, {
        range: true,
        min: 0,
        max: 100,
        step: 1,
      })}
    />
  );
};
