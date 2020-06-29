// @flow
import React from 'react';
import { select, boolean, number } from '@storybook/addon-knobs';
import BigNumber from 'bignumber.js';
import { action } from '@storybook/addon-actions';

// Screens
import Step1ConfigurationDialog from '../../../source/renderer/app/components/staking/redeem-itn-rewards/Step1ConfigurationDialog';
import Step2ConfirmationDialog from '../../../source/renderer/app/components/staking/redeem-itn-rewards/Step2ConfirmationDialog';

// Helpers
import { isValidMnemonic } from '../../../source/common/config/crypto/decrypt';
import validWords from '../../../source/common/config/crypto/valid-words.en';
import { generateWallet } from '../_support/utils';

const WALLETS = [
  generateWallet('First Wallet', '1000000000'),
  generateWallet('Second Wallet', '500000000'),
  generateWallet('Third Wallet', '100000000'),
  generateWallet('Fourth Wallet', '50000000'),
  generateWallet('Fifth Wallet', '7000000'),
];

export const Step1ConfigurationDialogStory = () => {
  const redeemWallet = select(
    'Redeem Wallet',
    WALLETS.reduce((obj, wallet, index) => {
      // obj[wallet.name] = wallet
      obj[`WALLET - ${index}`] = wallet;
      return obj;
    }, {}),
    WALLETS[0]
  );
  console.log('redeemWallet', redeemWallet);
  console.log('WALLETS[0]', WALLETS[0]);
  return (
    <Step1ConfigurationDialog
      key="Step1ConfigurationDialog"
      wallets={WALLETS}
      redeemWallet={WALLETS[0]}
      isWalletValid={boolean('isWalletValid')}
      isSubmitting={boolean('isSubmitting')}
      mnemonicValidator={isValidMnemonic}
      onSelectWallet={action('onSelectWallet')}
      onClose={action('onClose')}
      onContinue={action('onContinue')}
      onBack={action('onBack')}
      suggestedMnemonics={validWords}
    />
  );
};
export const Step2ConfirmationDialogStory = () => {
  const walletName = select(
    'Redeem Wallet',
    WALLETS.reduce((obj, { name }) => {
      obj[name] = name;
      return obj;
    }, {}),
    WALLETS[0]
  );
  return (
    <Step2ConfirmationDialog
      key="Step2ConfirmationDialog"
      walletName={walletName}
      rewardsTotal={new BigNumber(number('rewardsTotal', 100000))}
      transactionFees={new BigNumber(number('transactionFees', 100000))}
      finalTotal={new BigNumber(number('finalTotal', 100000))}
      onContinue={action('onContinue')}
      onClose={action('onClose')}
      onBack={action('onBack')}
      isSubmitting={boolean('isSubmitting', false)}
    />
  );
};
