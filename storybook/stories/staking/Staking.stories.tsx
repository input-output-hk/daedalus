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

export default {
  title: 'Decentralization / Staking',
  decorators: [stakingDecorator],
};

export const DelegationCenter = {
  render: (_, props) => (
    // @ts-ignore ts-migrate(2739) FIXME: Type '{ isEpochsInfoAvailable: true; id: string; n... Remove this comment to see the full error message
    <StakingDelegationCenterStory {...props} isEpochsInfoAvailable />
  ),

  parameters: {
    id: 'delegation-center',
  },
};

export const DelegationCenterLoading = {
  render: (_, props) => (
    // @ts-ignore ts-migrate(2739) FIXME: Type '{ isLoading: true; isEpochsInfoAvailable: tr... Remove this comment to see the full error message
    <StakingDelegationCenterStory {...props} isLoading isEpochsInfoAvailable />
  ),

  name: 'Delegation Center - Loading',

  parameters: {
    id: 'delegation-center-loading',
  },
};

export const DelegationCenterNotAnShelleyEra = {
  render: (_, props) => (
    // @ts-ignore ts-migrate(2739) FIXME: Type '{ isEpochsInfoAvailable: false; id: string; ... Remove this comment to see the full error message
    <StakingDelegationCenterStory {...props} isEpochsInfoAvailable={false} />
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

export const PoolsIndex = StakePoolsStory;

PoolsIndex.parameters = {
  id: 'stake-pools',
};

export const // @ts-ignore ts-migrate(2739) FIXME: Type '{ isLoading: true; id: string; name: string;... Remove this comment to see the full error message
  PoolsIndexLoading = {
    render: (props) => <StakePoolsStory {...props} isLoading />,
    name: 'Pools Index - Loading',

    parameters: {
      id: 'stake-pools-loading',
    },
  };

export const StakePoolsList = StakePoolsTableStory;

StakePoolsList.parameters = {
  id: 'stake-pools-table',
};

export const Rewards = StakingRewardsStory;

Rewards.parameters = {
  id: 'rewards',
};

export const DelegationWizard = {
  render: (_, props) => {
    const oversaturationPercentage = number('Oversaturation Percentage', 0, {
      min: 0,
      max: 1000,
      step: 1,
      range: true,
    });
    return (
      // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
      <StakingDelegationSteps
        {...props}
        oversaturationPercentage={oversaturationPercentage}
      />
    );
  },

  parameters: {
    id: 'wizard',
  },
};

export const // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
  DelegationWizardDelegationNotAvailable = {
    render: (_, props) => <StakingDelegationSteps {...props} isDisabled />,
    name: 'Delegation Wizard - Delegation Not Available',

    parameters: {
      id: 'wizard',
    },
  };

export const UndelegateConfirmation = {
  render: (_, props) => (
    <StakingUndelegateConfirmationStory
      {...props}
      isHardwareWallet={boolean('isHardwareWallet', false)}
    />
  ),

  parameters: {
    id: 'undelegate-confirmation',
  },
};

export const UndelegateConfirmationUnknownnStakePool = {
  render: (_, props) => (
    <StakingUndelegateConfirmationStory {...props} unknownStakePool />
  ),

  name: 'Undelegate Confirmation - unknownn stake pool',

  parameters: {
    id: 'undelegate-confirmation-unknown-pool',
  },
};

export const // @ts-ignore ts-migrate(2345) FIXME: Argument of type '({ locale, }: { locale: string; ... Remove this comment to see the full error message
  UndelegateConfirmationResult = {
    render: (_, props) => (
      <StakingUndelegateConfirmationResultStory {...props} />
    ),

    parameters: {
      id: 'undelegate-confirmation-result',
    },
  };
