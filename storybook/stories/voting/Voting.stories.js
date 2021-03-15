// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import { withKnobs, boolean, number } from '@storybook/addon-knobs';
import BigNumber from 'bignumber.js';
import StoryDecorator from '../_support/StoryDecorator';
import VotingRegistrationStepsChooseWallet from '../../../source/renderer/app/components/voting/voting-registration-wizard-steps/VotingRegistrationStepsChooseWallet';
import VotingRegistrationStepsRegister from '../../../source/renderer/app/components/voting/voting-registration-wizard-steps/VotingRegistrationStepsRegister';
import VotingRegistrationStepsConfirm from '../../../source/renderer/app/components/voting/voting-registration-wizard-steps/VotingRegistrationStepsConfirm';
import VotingRegistrationStepsEnterPinCode from '../../../source/renderer/app/components/voting/voting-registration-wizard-steps/VotingRegistrationStepsEnterPinCode';
import VotingRegistrationStepsQrCode from '../../../source/renderer/app/components/voting/voting-registration-wizard-steps/VotingRegistrationStepsQrCode';
import VotingInfo from '../../../source/renderer/app/components/voting/VotingInfo';
import {
  LANGUAGE_OPTIONS,
  DATE_ENGLISH_OPTIONS,
  TIME_OPTIONS,
} from '../../../source/renderer/app/config/profileConfig';
import {
  VOTING_REGISTRATION_MIN_TRANSACTION_CONFIRMATIONS,
  VOTING_REGISTRATION_MIN_WALLET_FUNDS,
} from '../../../source/renderer/app/config/votingConfig';
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
      assetName: '',
      quantity: new BigNumber(200),
    },
    {
      id: generateHash(),
      policyId: generatePolicyIdHash(),
      assetName: '',
      quantity: new BigNumber(200),
    },
  ],
  total: [
    {
      id: generateHash(),
      policyId: generatePolicyIdHash(),
      assetName: '',
      quantity: new BigNumber(200),
    },
    {
      id: generateHash(),
      policyId: generatePolicyIdHash(),
      assetName: '',
      quantity: new BigNumber(200),
    },
  ],
};

const WALLETS = [
  generateWallet('Wallet 1', '100000000000', assets, 0),
  generateWallet('Wallet 2', '100', assets, 0),
];

const stepsList = ['Wallet', 'Sign', 'Confirm', 'PIN code', 'QR code'];

storiesOf('Voting|Voting Registration Wizard', module)
  .addDecorator((story) => <StoryDecorator>{story()}</StoryDecorator>)
  .addDecorator(withKnobs)

  // ====== Stories ======

  .add('Voting Registration - Step 1', () => (
    <VotingRegistrationStepsChooseWallet
      onClose={action('onClose')}
      stepsList={stepsList}
      activeStep={1}
      numberOfStakePools={number('numberOfStakePools', 100)}
      onSelectWallet={action('onSelectWallet')}
      wallets={WALLETS}
      minVotingRegistrationFunds={VOTING_REGISTRATION_MIN_WALLET_FUNDS}
      selectedWalletId={WALLETS[0].id}
      isWalletAcceptable={action('isWalletAcceptable')}
      getStakePoolById={action('getStakePoolById')}
    />
  ))

  .add('Voting Registration - Step 2', () => (
    <VotingRegistrationStepsRegister
      onClose={action('onClose')}
      onBack={action('onBack')}
      stepsList={stepsList}
      activeStep={2}
      transactionFee={
        new BigNumber(
          number('transactionFee', 0.3, {
            min: 0,
            max: 1000000,
          })
        )
      }
      isSubmitting={boolean('isSubmitting')}
      onConfirm={action('onConfirm')}
      onExternalLinkClick={action('onExternalLinkClick')}
    />
  ))

  .add('Voting Registration - Step 3', () => (
    <VotingRegistrationStepsConfirm
      onClose={action('onClose')}
      stepsList={stepsList}
      activeStep={3}
      isTransactionPending={boolean('isTransactionPending', true)}
      isTransactionConfirmed={boolean('isTransactionConfirmed', false)}
      transactionConfirmations={number('transactionConfirmations', 0, {
        range: true,
        max: VOTING_REGISTRATION_MIN_TRANSACTION_CONFIRMATIONS,
      })}
      onConfirm={action('onConfirm')}
      onRestart={action('onRestart')}
      transactionError={boolean('transactionError', false)}
    />
  ))

  .add('Voting Registration - Step 4', () => (
    <VotingRegistrationStepsEnterPinCode
      onClose={action('onClose')}
      stepsList={stepsList}
      activeStep={4}
      onSetPinCode={action('onSetPinCode')}
    />
  ))

  .add('Voting Registration - Step 5', () => (
    <VotingRegistrationStepsQrCode
      onClose={action('onClose')}
      onDownloadPDF={action('onDownloadPDF')}
      stepsList={stepsList}
      activeStep={2}
      qrCode="djkhfkwdjhfkwdhfkwjdhfkwdhf9wdyf9wdh9u3h03hd0f3hd0h30hf30dhf03dhf03dhf03dhf03dhf0u3dhf0u3dhf0u3dfh30uhfd30uh"
    />
  ));

storiesOf('Voting|Voting Info', module)
  .addDecorator((story) => <StoryDecorator>{story()}</StoryDecorator>)
  .addDecorator(withKnobs)

  // ====== Stories ======

  .add('Voting Info', () => (
    <VotingInfo
      currentLocale={LANGUAGE_OPTIONS[0].value}
      currentDateFormat={DATE_ENGLISH_OPTIONS[0].value}
      currentTimeFormat={TIME_OPTIONS[0].value}
      isRegistrationEnded={boolean('isRegistrationEnded', false)}
      onRegisterToVoteClick={action('onRegisterToVoteClick')}
      onExternalLinkClick={action('onExternalLinkClick')}
    />
  ));
