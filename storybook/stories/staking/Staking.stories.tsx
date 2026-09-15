import React from 'react';
import { withKnobs, number, boolean } from '@storybook/addon-knobs';
import { action } from '@storybook/addon-actions';
import { stakingDecorator } from './_support/decorator';
import DelegationCenterNoWallets from '../../../source/renderer/app/components/staking/delegation-center/DelegationCenterNoWallets';
import { StakePoolsStory } from './_support/StakePools';
import { StakingRewardsStory } from './_support/Rewards';
import { StakingDelegationCenterStory } from './_support/DelegationCenter';
import { StakingDelegationSteps } from './_support/DelegationSteps';
import {
  StakingUndelegateConfirmationStory,
  StakingUndelegateConfirmationResultStory,
} from './_support/Undelegate';
import { StakePoolsTableStory } from './_support/StakePoolsTable';
import { currentThemeOf, localeOf } from '../_support/globals';

export default {
  title: 'Decentralization / Staking',
  decorators: [stakingDecorator],
};

export const DelegationCenter = {
  render: (_args, context) => (
    <StakingDelegationCenterStory
      locale={localeOf(context)}
      currentTheme={currentThemeOf(context)}
      isLoading={false}
      isEpochsInfoAvailable
    />
  ),

  parameters: {
    id: 'delegation-center',
  },
};

export const DelegationCenterLoading = {
  render: (_args, context) => (
    <StakingDelegationCenterStory
      locale={localeOf(context)}
      currentTheme={currentThemeOf(context)}
      isLoading
      isEpochsInfoAvailable
    />
  ),

  name: 'Delegation Center - Loading',

  parameters: {
    id: 'delegation-center-loading',
  },
};

export const DelegationCenterNotAnShelleyEra = {
  render: (_args, context) => (
    <StakingDelegationCenterStory
      locale={localeOf(context)}
      currentTheme={currentThemeOf(context)}
      isLoading={false}
      isEpochsInfoAvailable={false}
    />
  ),

  name: 'Delegation Center - Not an Shelley era',

  parameters: {
    id: 'delegation-center-loading',
  },
};

export const _DelegationCenterNoWallets = {
  render: () => (
    <DelegationCenterNoWallets
      onGoToCreateWalletClick={action('onGoToCreateWalletClick')}
      minDelegationFunds={number('minDelegationFunds', 10)}
    />
  ),

  name: 'Delegation Center - No Wallets',
};

export const PoolsIndex = {
  render: (_args, context) => (
    <StakePoolsStory
      locale={localeOf(context)}
      currentTheme={currentThemeOf(context)}
      isLoading={false}
    />
  ),

  parameters: {
    id: 'stake-pools',
  },
};

export const PoolsIndexLoading = {
  render: (_args, context) => (
    <StakePoolsStory
      locale={localeOf(context)}
      currentTheme={currentThemeOf(context)}
      isLoading
    />
  ),

  name: 'Pools Index - Loading',

  parameters: {
    id: 'stake-pools-loading',
  },
};

export const StakePoolsList = {
  render: (_args, context) => (
    <StakePoolsTableStory currentTheme={currentThemeOf(context)} />
  ),

  parameters: {
    id: 'stake-pools-table',
  },
};

export const Rewards = {
  render: () => <StakingRewardsStory />,

  parameters: {
    id: 'rewards',
  },
};

export const DelegationWizard = {
  render: (_args, context) => {
    const oversaturationPercentage = number('Oversaturation Percentage', 0, {
      min: 0,
      max: 1000,
      step: 1,
      range: true,
    });
    return (
      <StakingDelegationSteps
        locale={localeOf(context)}
        currentTheme={currentThemeOf(context)}
        oversaturationPercentage={oversaturationPercentage}
      />
    );
  },

  parameters: {
    id: 'wizard',
  },
};

export const DelegationWizardDelegationNotAvailable = {
  render: (_args, context) => (
    <StakingDelegationSteps
      locale={localeOf(context)}
      currentTheme={currentThemeOf(context)}
      oversaturationPercentage={0}
      isDisabled
    />
  ),
  name: 'Delegation Wizard - Delegation Not Available',

  parameters: {
    id: 'wizard',
  },
};

export const UndelegateConfirmation = {
  render: () => (
    <StakingUndelegateConfirmationStory
      isHardwareWallet={boolean('isHardwareWallet', false)}
    />
  ),

  parameters: {
    id: 'undelegate-confirmation',
  },
};

export const UndelegateConfirmationUnknownnStakePool = {
  render: () => <StakingUndelegateConfirmationStory unknownStakePool />,

  name: 'Undelegate Confirmation - unknownn stake pool',

  parameters: {
    id: 'undelegate-confirmation-unknown-pool',
  },
};

export const UndelegateConfirmationResult = {
  render: (_args, context) => (
    <StakingUndelegateConfirmationResultStory locale={localeOf(context)} />
  ),

  parameters: {
    id: 'undelegate-confirmation-result',
  },
};
