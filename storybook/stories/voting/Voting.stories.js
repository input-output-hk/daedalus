// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import { withKnobs, boolean, number, text } from '@storybook/addon-knobs';
import { observable } from 'mobx';
import StoryDecorator from '../_support/StoryDecorator';
// import VotingRegistrationStepsChooseWallet from '../../../source/renderer/app/components/voting/voting-registration-wizar-steps/VotingRegistrationStepsChooseWallet';
// import VotingRegistrationStepsSign from '../../../source/renderer/app/components/voting/voting-registration-wizar-steps/VotingRegistrationStepsSign';
import VotingRegistrationStepsConfirm from '../../../source/renderer/app/components/voting/voting-registration-wizar-steps/VotingRegistrationStepsConfirm';
// import VotingRegistrationStepsEnterPinCode from '../../../source/renderer/app/components/voting/voting-registration-wizar-steps/VotingRegistrationStepsEnterPinCode';
// import VotingRegistrationStepsQrCode from '../../../source/renderer/app/components/voting/voting-registration-wizar-steps/VotingRegistrationStepsQrCode';

const stepsList = ['Wallet', 'Sign', 'Confirm', 'PIN code', 'QR code'];

storiesOf('Voting|Voting Registration Wizard', module)
  .addDecorator((story) => <StoryDecorator>{story()}</StoryDecorator>)
  .addDecorator(withKnobs)

  // ====== Stories ======

  .add('Voting Registration - Step 3', () => (
    <VotingRegistrationStepsConfirm
      onClose={action('onClose')}
      stepsList={stepsList}
      activeStep={3}
      isTransactionPending={boolean('isTransactionPending', true)}
      isTransactionConfirmed={boolean('isTransactionConfirmed', false)}
      transactionConfirmations={number('transactionConfirmations', 0)}
      onConfirm={action('onConfirm')}
      onRestart={action('onRestart')}
    />
  ));
