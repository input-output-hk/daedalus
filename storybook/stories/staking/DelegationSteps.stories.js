// @flow
import React, { Component } from 'react';
import { number } from '@storybook/addon-knobs';
import { action } from '@storybook/addon-actions';

import DelegationStepsIntroDialog from '../../../source/renderer/app/components/staking/delegation-setup-wizard/DelegationStepsIntroDialog';
import DelegationStepsChooseWalletDialog from '../../../source/renderer/app/components/staking/delegation-setup-wizard/DelegationStepsChooseWalletDialog';
import DelegationStepsChooseStakePoolDialog from '../../../source/renderer/app/components/staking/delegation-setup-wizard/DelegationStepsChooseStakePoolDialog';
import DelegationStepsNotAvailableDialog from '../../../source/renderer/app/components/staking/delegation-setup-wizard/DelegationStepsNotAvailableDialog';
import DelegationStepsConfirmationDialog from '../../../source/renderer/app/components/staking/delegation-setup-wizard/DelegationStepsConfirmationDialog';
import DelegationStepsActivationDialog from '../../../source/renderer/app/components/staking/delegation-setup-wizard/DelegationStepsActivationDialog';

import translations from '../../../source/renderer/app/i18n/translations';
import STAKE_POOLS from '../../../source/renderer/app/config/stakingStakePools.dummy.json';

const WALLETS = [
  {
    id: '1',
    value: '1.0001 ADA',
    label: 'First Wallet',
    isAcceptableSetupWallet: true,
    hasPassword: true,
  },
  {
    id: '2',
    value: '2 ADA',
    label: 'Second Wallet',
    isAcceptableSetupWallet: true,
    hasPassword: true,
  },
  {
    id: '3',
    value: '0.0001 ADA',
    label: 'Third Wallet',
    isAcceptableSetupWallet: false,
    hasPassword: true,
  },
];

const locales = {
  English: 'en-US',
  Japanese: 'ja-JP',
};

// Delegation steps labels are translated outside components and we need to determine correct translations
const locale = sessionStorage.getItem('localeName') || 'English';
const currentTheme = sessionStorage.getItem('themeName') || 'light-blue';
const translationIndex = locales[locale];

// @TODO - improve locales GET once [DDW-711](https://github.com/input-output-hk/daedalus/pull/1426) is merged
const DELEGATION_WIZARD_STEPS_LIST = [
  translations[translationIndex]['staking.delegationSetup.steps.step.1.label'],
  translations[translationIndex]['staking.delegationSetup.steps.step.2.label'],
  translations[translationIndex]['staking.delegationSetup.steps.step.3.label'],
  translations[translationIndex]['staking.delegationSetup.steps.step.4.label'],
];

type State = {
  currentStep: number,
};

const NUMBER_OF_STEPS = 6;

export class StakingDelegationSteps extends Component<any, State> {
  state = {
    currentStep: 0,
  };

  get dialogs() {
    return [
      <DelegationStepsIntroDialog
        key="DelegationStepsIntroDialog"
        onClose={action('onClose')}
        onContinue={this.onContinue}
        onLearnMoreClick={action('onLearnMoreClick')}
      />,
      <DelegationStepsChooseWalletDialog
        key="DelegationStepsChooseWalletDialog"
        stepsList={DELEGATION_WIZARD_STEPS_LIST}
        onClose={action('onClose')}
        onSelectWallet={this.onContinue}
        onBack={action('onBack')}
        wallets={WALLETS}
        minDelegationFunds={1}
        selectedWallet={null}
      />,
      <DelegationStepsConfirmationDialog
        key="DelegationStepsConfirmationDialog"
        stepsList={DELEGATION_WIZARD_STEPS_LIST}
        isSpendingPasswordSet
        onClose={action('onClose')}
        onConfirm={this.onContinue}
        onBack={action('onBack')}
      />,
      <DelegationStepsActivationDialog
        key="DelegationStepsActivationDialog"
        stepsList={DELEGATION_WIZARD_STEPS_LIST}
        isSpendingPasswordSet
        onClose={action('onClose')}
        onActivate={this.onContinue}
        onBack={action('onBack')}
      />,
      <DelegationStepsChooseStakePoolDialog
        key="DelegationStepsChooseStakePoolDialog"
        stepsList={DELEGATION_WIZARD_STEPS_LIST}
        stakePoolsList={STAKE_POOLS.slice(
          0,
          number('Pools', 100, {
            range: true,
            min: 37,
            max: 300,
            step: 1,
          })
        )}
        stakePoolsDelegatingList={[
          STAKE_POOLS[0],
          STAKE_POOLS[13],
          STAKE_POOLS[36],
        ]}
        onOpenExternalLink={() => {}}
        currentTheme={currentTheme}
        onClose={action('onClose')}
        onBack={action('onBack')}
        onSelectPool={this.onContinue}
        selectedPool={null}
      />,
      <DelegationStepsNotAvailableDialog
        key="DelegationStepsNotAvailableDialog"
        minDelegationFunds={1}
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
