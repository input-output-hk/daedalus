// @flow
import React, { Component } from 'react';
import { BigNumber } from 'bignumber.js';
import { number } from '@storybook/addon-knobs';
import { action } from '@storybook/addon-actions';
import { find } from 'lodash';
import DelegationStepsIntroDialog from '../../../source/renderer/app/components/staking/delegation-setup-wizard/DelegationStepsIntroDialog';
import DelegationStepsChooseWalletDialog from '../../../source/renderer/app/components/staking/delegation-setup-wizard/DelegationStepsChooseWalletDialog';
import DelegationStepsChooseStakePoolDialog from '../../../source/renderer/app/components/staking/delegation-setup-wizard/DelegationStepsChooseStakePoolDialog';
import DelegationStepsNotAvailableDialog from '../../../source/renderer/app/components/staking/delegation-setup-wizard/DelegationStepsNotAvailableDialog';
import DelegationStepsConfirmationDialog from '../../../source/renderer/app/components/staking/delegation-setup-wizard/DelegationStepsConfirmationDialog';
import DelegationStepsSuccessDialog from '../../../source/renderer/app/components/staking/delegation-setup-wizard/DelegationStepsSuccessDialog';

import { MIN_DELEGATION_FUNDS } from '../../../source/renderer/app/config/stakingConfig';
import translations from '../../../source/renderer/app/i18n/translations';
import STAKE_POOLS from '../../../source/renderer/app/config/stakingStakePools.dummy.json';
import { generateWallet } from '../_support/utils';

const WALLETS = [
  generateWallet('First Wallet', '1000000000', 0, STAKE_POOLS[0]),
  generateWallet('Second Wallet', '500000000', 0, STAKE_POOLS[100]),
  generateWallet('Third Wallet', '100000000', 0, STAKE_POOLS[150]),
  generateWallet('Fourth Wallet', '50000000', 0, STAKE_POOLS[290]),
  generateWallet('Fifth Wallet', '7000000'),
];

const getDelegationWizardStepsList = locale => [
  translations[locale]['staking.delegationSetup.steps.step.1.label'],
  translations[locale]['staking.delegationSetup.steps.step.2.label'],
  translations[locale]['staking.delegationSetup.steps.step.3.label'],
];

type Props = {
  currentTheme: string,
  locale: string,
  isDisabled?: boolean,
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

    if (this.props.isDisabled) {
      return [
        <DelegationStepsNotAvailableDialog
          key="DelegationStepsNotAvailableDialog"
          minDelegationFunds={MIN_DELEGATION_FUNDS}
          onClose={action('onClose')}
        />,
      ];
    }
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
        getStakePoolById={poolId =>
          find(STAKE_POOLS, stakePool => stakePool.id === poolId)
        }
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
        recentStakePools={[STAKE_POOLS[0], STAKE_POOLS[13], STAKE_POOLS[36]]}
        onOpenExternalLink={action('onOpenExternalLink')}
        getPledgeAddressUrl={action('getPledgeAddressUrl')}
        currentTheme={this.props.currentTheme}
        onClose={action('onClose')}
        onBack={action('onBack')}
        onSelectPool={this.onContinue}
        selectedPool={STAKE_POOLS[0]}
        selectedWallet={WALLETS[0]}
      />,
      <DelegationStepsConfirmationDialog
        key="DelegationStepsConfirmationDialog"
        transactionFee={new BigNumber(0.172081)}
        stepsList={getDelegationWizardStepsList(this.props.locale)}
        selectedPool={STAKE_POOLS[0]}
        isSubmitting={false}
        selectedWallet={WALLETS[0]}
        onConfirm={this.onContinue}
        onClose={action('onClose')}
        onBack={action('onBack')}
        error={null}
      />,
      <DelegationStepsSuccessDialog
        key="DelegationStepsSuccessDialog"
        delegatedWallet={WALLETS[0]}
        delegatedStakePool={STAKE_POOLS[0]}
        currentLocale="en-US"
        nextEpochStartTime="2019-12-09T00:00:00.161Z"
        onClose={this.onReset}
      />,
    ];
  }

  onContinue = () => {
    const { currentStep } = this.state;
    let nextStep = currentStep + 1;
    if (nextStep > NUMBER_OF_STEPS - 1) nextStep = 0;
    this.setState({ currentStep: nextStep });
  };

  onReset = () => {
    this.setState({ currentStep: 0 });
  };

  render() {
    const { currentStep } = this.state;
    return this.dialogs[currentStep];
  }
}
