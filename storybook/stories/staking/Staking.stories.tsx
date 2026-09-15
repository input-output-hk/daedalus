import React from 'react';
import { storiesOf } from '@storybook/react';
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


storiesOf('Decentralization / Staking', module)
  .addDecorator(stakingDecorator) // ====== Stories ======
  .add(
    'Delegation Center',
    (_, props) => (
      // @ts-ignore ts-migrate(2739) FIXME: Type '{ isEpochsInfoAvailable: true; id: string; n... Remove this comment to see the full error message
      <StakingDelegationCenterStory {...props} isEpochsInfoAvailable />
    ),
    {
      id: 'delegation-center',
    }
  )
  .add(
    'Delegation Center - Loading',
    (_, props) => (
      // @ts-ignore ts-migrate(2739) FIXME: Type '{ isLoading: true; isEpochsInfoAvailable: tr... Remove this comment to see the full error message
      <StakingDelegationCenterStory
        {...props}
        isLoading
        isEpochsInfoAvailable
      />
    ),
    {
      id: 'delegation-center-loading',
    }
  )
  .add(
    'Delegation Center - Not an Shelley era',
    (_, props) => (
      // @ts-ignore ts-migrate(2739) FIXME: Type '{ isEpochsInfoAvailable: false; id: string; ... Remove this comment to see the full error message
      <StakingDelegationCenterStory {...props} isEpochsInfoAvailable={false} />
    ),
    {
      id: 'delegation-center-loading',
    }
  )
  .add('Delegation Center - No Wallets', () => (
    <DelegationCenterNoWallets
      onGoToCreateWalletClick={action('onGoToCreateWalletClick')}
      minDelegationFunds={number('minDelegationFunds', 10)}
    />
  ))
  // @ts-ignore ts-migrate(2345) FIXME: Argument of type '(props: Props) => JSX.Element' i... Remove this comment to see the full error message
  .add('Pools Index', StakePoolsStory, {
    id: 'stake-pools',
  })
  .add(
    'Pools Index - Loading',
    // @ts-ignore ts-migrate(2739) FIXME: Type '{ isLoading: true; id: string; name: string;... Remove this comment to see the full error message
    (props) => <StakePoolsStory {...props} isLoading />,
    {
      id: 'stake-pools-loading',
    }
  )
  // @ts-ignore ts-migrate(2345) FIXME: Argument of type '(props: Props) => JSX.Element' i... Remove this comment to see the full error message
  .add('Stake Pools List', StakePoolsTableStory, {
    id: 'stake-pools-table',
  })
  .add('Rewards', StakingRewardsStory, {
    id: 'rewards',
  })
  .add(
    'Delegation Wizard',
    (_, props) => {
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
    {
      id: 'wizard',
    }
  )
  .add(
    'Delegation Wizard - Delegation Not Available',
    // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
    (_, props) => <StakingDelegationSteps {...props} isDisabled />,
    {
      id: 'wizard',
    }
  )
  .add(
    'Undelegate Confirmation',
    (_, props) => (
      <StakingUndelegateConfirmationStory
        {...props}
        isHardwareWallet={boolean('isHardwareWallet', false)}
      />
    ),
    {
      id: 'undelegate-confirmation',
    }
  )
  .add(
    'Undelegate Confirmation - unknownn stake pool',
    (_, props) => (
      <StakingUndelegateConfirmationStory {...props} unknownStakePool />
    ),
    {
      id: 'undelegate-confirmation-unknown-pool',
    }
  )
  .add(
    'Undelegate Confirmation Result',
    // @ts-ignore ts-migrate(2345) FIXME: Argument of type '({ locale, }: { locale: string; ... Remove this comment to see the full error message
    (_, props) => <StakingUndelegateConfirmationResultStory {...props} />,
    {
      id: 'undelegate-confirmation-result',
    }
  );
