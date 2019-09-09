// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import { date, number } from '@storybook/addon-knobs';
import { action } from '@storybook/addon-actions';

import StakingCountdown from '../../../source/renderer/app/components/staking/countdown/StakingCountdown';
import StakingInfo from '../../../source/renderer/app/components/staking/info/StakingInfo';
import DelegationCenterNoWallets from '../../../source/renderer/app/components/staking/delegation-center/DelegationCenterNoWallets';

import { StakePoolsStory } from './StakePools.stories';
import { StakingRewardsStory } from './Rewards.stories';
import { StakingDelegationCenterStory } from './DelegationCenter.stories';
import { StakingDelegationSteps } from './DelegationSteps.stories';
import { StakingEpochsStory } from './Epochs.stories';

import StakingWrapper from './StakingWrapper';

const defaultPercentage = 10;
const defaultStartDateTime = new Date('2019-09-26');
const startDateTimeKnob = (name, defaultValue) => {
  const stringTimestamp = date(name, defaultValue);

  return new Date(stringTimestamp).toISOString();
};

const pageNames = {
  countdown: 'Staking Countdown',
  'delegation-center': 'Delegation Center',
  'stake-pools': 'Stake Pools',
  'stake-pools-tooltip': 'Tooltip',
  rewards: 'Rewards',
  epochs: 'Epochs',
  info: 'Info',
};

storiesOf('Staking|Countdown', module)
  .addDecorator(StakingWrapper)
  // ====== Stories ======
  .add(pageNames.countdown, () => (
    <div>
      <StakingCountdown
        startDateTime={startDateTimeKnob(
          'Decentralization Start DateTime',
          defaultStartDateTime
        )}
        onLearnMoreClick={action('onLearnMoreClick')}
      />
    </div>
  ));

storiesOf('Staking|Descentralization', module)
  .addDecorator(StakingWrapper)
  // ====== Stories ======

  .add('Delegation Steps', () => <StakingDelegationSteps />)

  .add(pageNames['delegation-center'], StakingDelegationCenterStory, {
    id: 'delegation-center',
  })

  .add('Delegation Center - No Wallets', () => (
    <DelegationCenterNoWallets
      onGoToCreateWalletClick={action('onGoToCreateWalletClick')}
    />
  ))

  .add(pageNames['stake-pools'], StakePoolsStory, { id: 'stake-pools' })

  .add(pageNames.rewards, StakingRewardsStory, { id: 'rewards' })

  .add(pageNames.epochs, StakingEpochsStory, { id: 'epochs' })

  .add(
    pageNames.info,
    () => (
      <StakingInfo
        percentage={number('Percentage', defaultPercentage, {
          min: 0,
          max: 100,
          step: 1,
          range: true,
        })}
        onLearnMoreClick={action('onLearnMoreClick')}
      />
    ),
    {
      id: 'info',
    }
  );
