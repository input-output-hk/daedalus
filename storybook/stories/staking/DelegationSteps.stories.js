// @flow
import React, { Component } from 'react';
import { BigNumber } from 'bignumber.js';
import { number } from '@storybook/addon-knobs';
import { action } from '@storybook/addon-actions';

import DelegationStepsIntroDialog from '../../../source/renderer/app/components/staking/delegation-setup-wizard/DelegationStepsIntroDialog';
import DelegationStepsChooseWalletDialog from '../../../source/renderer/app/components/staking/delegation-setup-wizard/DelegationStepsChooseWalletDialog';
import DelegationStepsChooseStakePoolDialog from '../../../source/renderer/app/components/staking/delegation-setup-wizard/DelegationStepsChooseStakePoolDialog';
import DelegationStepsNotAvailableDialog from '../../../source/renderer/app/components/staking/delegation-setup-wizard/DelegationStepsNotAvailableDialog';
import DelegationStepsConfirmationDialog from '../../../source/renderer/app/components/staking/delegation-setup-wizard/DelegationStepsConfirmationDialog';
import DelegationStepsActivationDialog from '../../../source/renderer/app/components/staking/delegation-setup-wizard/DelegationStepsActivationDialog';

import { MIN_DELEGATION_FUNDS } from '../../../source/renderer/app/config/stakingConfig';
import translations from '../../../source/renderer/app/i18n/translations';
import STAKE_POOLS from '../../../source/renderer/app/config/stakingStakePools.dummy.json';
import { generateWallet } from '../_support/utils';

const WALLETS = [
  generateWallet('First Wallet', '1000000', 0, STAKE_POOLS[0]),
  generateWallet('Second Wallet', '500000', 0, STAKE_POOLS[100]),
  generateWallet('Third Wallet', '10000', 0, STAKE_POOLS[150]),
  generateWallet('Fourth Wallet', '5000', 0, STAKE_POOLS[290]),
  generateWallet('Fifth Wallet', '0'),
];

const getDelegationWizardStepsList = locale => [
  translations[locale]['staking.delegationSetup.steps.step.1.label'],
  translations[locale]['staking.delegationSetup.steps.step.2.label'],
  translations[locale]['staking.delegationSetup.steps.step.3.label'],
  translations[locale]['staking.delegationSetup.steps.step.4.label'],
];

type Props = {
  currentTheme: string,
  locale: string,
};

type State = {
  currentStep: number,
};

const NUMBER_OF_STEPS = 6;

export class StakingDelegationSteps extends Component<Props, State> {
  state = {
    currentStep: 0,
  };

  get dialogs() {
    const stakePoolsList = STAKE_POOLS.slice(
      0,
      number('Pools', 100, {
        range: true,
        min: 37,
        max: 300,
        step: 1,
      })
    );
    return [
      <DelegationStepsIntroDialog
        key="DelegationStepsIntroDialog"
        onClose={action('onClose')}
        onContinue={this.onContinue}
        onLearnMoreClick={action('onLearnMoreClick')}
      />,
      <DelegationStepsChooseWalletDialog
        key="DelegationStepsChooseWalletDialog"
        numberOfStakePools={stakePoolsList.length}
        stepsList={getDelegationWizardStepsList(this.props.locale)}
        onClose={action('onClose')}
        onSelectWallet={this.onContinue}
        onBack={action('onBack')}
        wallets={WALLETS}
        minDelegationFunds={MIN_DELEGATION_FUNDS}
        selectedWalletId={null}
        isWalletAcceptable={amount => amount.gte(MIN_DELEGATION_FUNDS)}
      />,
      <DelegationStepsChooseStakePoolDialog
        key="DelegationStepsChooseStakePoolDialog"
        stepsList={getDelegationWizardStepsList(this.props.locale)}
        stakePoolsList={stakePoolsList}
        stakePoolsDelegatingList={[
          STAKE_POOLS[0],
          STAKE_POOLS[13],
          STAKE_POOLS[36],
        ]}
        onOpenExternalLink={() => {}}
        currentTheme={this.props.currentTheme}
        onClose={action('onClose')}
        onBack={action('onBack')}
        onSelectPool={this.onContinue}
        selectedPool={null}
      />,
      <DelegationStepsConfirmationDialog
        fees={new BigNumber(0.172081)}
        key="DelegationStepsConfirmationDialog"
        stepsList={getDelegationWizardStepsList(this.props.locale)}
        isSpendingPasswordSet
        onClose={action('onClose')}
        onConfirm={this.onContinue}
        onBack={action('onBack')}
      />,
      <DelegationStepsActivationDialog
        amount={new BigNumber(3)}
        fees={new BigNumber(0.172081)}
        key="DelegationStepsActivationDialog"
        stepsList={getDelegationWizardStepsList(this.props.locale)}
        isSpendingPasswordSet
        onClose={action('onClose')}
        onActivate={this.onContinue}
        onBack={action('onBack')}
      />,
      <DelegationStepsNotAvailableDialog
        key="DelegationStepsNotAvailableDialog"
        minDelegationFunds={MIN_DELEGATION_FUNDS}
        onClose={this.onContinue}
      />,
    ];
  }

  onContinue = () => {
    const { currentStep } = this.state;
    let nextStep = currentStep + 1;
    if (nextStep > NUMBER_OF_STEPS - 1) nextStep = 0;
    this.setState({ currentStep: nextStep });
  };

  render() {
    const { currentStep } = this.state;
    return this.dialogs[currentStep];
  }
}
