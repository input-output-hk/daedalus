// @flow
import React from 'react';
import { action } from '@storybook/addon-actions';

// Screens
import StakingRewards from '../../../source/renderer/app/components/staking/rewards/StakingRewards';

// Dummy data initialization
import REWARDS from '../../../source/renderer/app/config/stakingRewards.dummy.json';

export const StakingRewardsStory = () => (
  <StakingRewards
    rewards={REWARDS}
    isLoading={false}
    onLearnMoreClick={action('onLearnMoreClick')}
  />
);
