import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import { withKnobs, boolean, number, select } from '@storybook/addon-knobs';
import BigNumber from 'bignumber.js';
import StoryDecorator from '../_support/StoryDecorator';
import StoryProvider from '../_support/StoryProvider';
import VotingRegistrationStepsChooseWallet from '../../../source/renderer/app/components/voting/voting-registration-wizard-steps/VotingRegistrationStepsChooseWallet';
import VotingRegistrationStepsRegister from '../../../source/renderer/app/components/voting/voting-registration-wizard-steps/VotingRegistrationStepsRegister';
import VotingRegistrationStepsConfirm from '../../../source/renderer/app/components/voting/voting-registration-wizard-steps/VotingRegistrationStepsConfirm';
import VotingRegistrationStepsEnterPinCode from '../../../source/renderer/app/components/voting/voting-registration-wizard-steps/VotingRegistrationStepsEnterPinCode';
import VotingRegistrationStepsQrCode from '../../../source/renderer/app/components/voting/voting-registration-wizard-steps/VotingRegistrationStepsQrCode';
import VotingInfo from '../../../source/renderer/app/components/voting/voting-info/VotingInfo';
import { FundPhase } from '../../../source/renderer/app/stores/VotingStore';
import { CatalystFund } from '../../../source/renderer/app/api/voting/types';
import { VotingFooterLinks } from '../../../source/renderer/app/components/voting/VotingFooterLinks';
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
import { HwDeviceStatuses } from '../../../source/renderer/app/domains/Wallet';
import VerticalFlexContainer from '../../../source/renderer/app/components/layout/VerticalFlexContainer';
import { Locale } from '../../../source/common/types/locales.types';

const mockFundInfo: CatalystFund = {
  current: {
    number: 7,
    startTime: new Date('Jan 20, 2022, 11:00 UTC'),
    endTime: new Date('Feb 3, 2022, 11:00 UTC'),
    resultsTime: new Date('Feb 10, 2022'),
    registrationSnapshotTime: new Date('Jan 6, 2022, 11:00 UTC'),
  },
  next: {
    number: 8,
    startTime: new Date('Jan 6, 2022, 11:00 UTC'),
    registrationSnapshotTime: new Date('Apr 7, 2022, 11:00 UTC'),
  },
};

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
  generateWallet('Wallet 1', '100000000000000', assets, 0),
  generateWallet(
    'Wallet 2',
    '500000000',
    assets,
    0,
    undefined,
    true,
    'syncing'
  ),
];
const stepsList = ['Wallet', 'Sign', 'Confirm', 'PIN code', 'QR code'];

const votingInfo = {
  fundInfo: mockFundInfo,
  currentLocale: LANGUAGE_OPTIONS[0].value as Locale,
  currentDateFormat: DATE_ENGLISH_OPTIONS[0].value,
  currentTimeFormat: TIME_OPTIONS[0].value,
  onRegisterToVoteClick: action('onRegisterToVoteClick'),
  onExternalLinkClick: action('onExternalLinkClick'),
};

storiesOf('Voting / Voting Registration Wizard', module)
  .addDecorator((story) => (
    <StoryProvider>
      <StoryDecorator>{story()}</StoryDecorator>
    </StoryProvider>
  ))
  .addDecorator(withKnobs) // ====== Stories ======
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
      nextFundNumber={8}
    />
  ))
  .add('Voting Registration - Step 2', () => (
    <VotingRegistrationStepsRegister
      onClose={action('onClose')}
      onBack={action('onBack')}
      stepsList={stepsList}
      activeStep={2}
      nextFundNumber={8}
      transactionFee={
        new BigNumber(
          number('transactionFee', 0.3, {
            min: 0,
            max: 1000000,
          })
        )
      }
      // @ts-ignore ts-migrate(2554) FIXME: Expected 2-3 arguments, but got 1.
      isSubmitting={boolean('isSubmitting')}
      onConfirm={action('onConfirm')}
      onExternalLinkClick={action('onExternalLinkClick')}
      hwDeviceStatus={HwDeviceStatuses.CONNECTING}
      isHardwareWallet={boolean('isHardwareWallet', false)}
      isTrezor={boolean('isTrezor', false)}
      selectedWallet={WALLETS[0]}
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
      nextFundNumber={8}
    />
  ))
  .add('Voting Registration - Step 4', () => (
    <VotingRegistrationStepsEnterPinCode
      onClose={action('onClose')}
      stepsList={stepsList}
      activeStep={4}
      onSetPinCode={action('onSetPinCode')}
      nextFundNumber={8}
    />
  ))
  .add('Voting Registration - Step 5', () => (
    <VotingRegistrationStepsQrCode
      onClose={action('onClose')}
      onDownloadPDF={action('onDownloadPDF')}
      stepsList={stepsList}
      activeStep={2}
      qrCode="djkhfkwdjhfkwdhfkwjdhfkwdhf9wdyf9wdh9u3h03hd0f3hd0h30hf30dhf03dhf03dhf03dhf03dhf0u3dhf0u3dhf0u3dfh30uhfd30uh"
      nextFundNumber={8}
    />
  ));
storiesOf('Voting / Voting Info', module)
  .addDecorator((story) => (
    <StoryDecorator>
      <VerticalFlexContainer>
        {story()}
        <VotingFooterLinks />
      </VerticalFlexContainer>
    </StoryDecorator>
  ))
  .addDecorator(withKnobs) // ====== Stories ======
  .add('Snapshot phase', () => (
    <VotingInfo {...votingInfo} fundPhase={FundPhase.SNAPSHOT} />
  ))
  .add('Voting phase', () => (
    <VotingInfo {...votingInfo} fundPhase={FundPhase.VOTING} />
  ))
  .add('Tallying phase', () => (
    <VotingInfo {...votingInfo} fundPhase={FundPhase.TALLYING} />
  ))
  .add('Results phase', () => (
    <VotingInfo {...votingInfo} fundPhase={FundPhase.RESULTS} />
  ))
  .add('API error', () => <VotingInfo {...votingInfo} fundPhase={null} />);
