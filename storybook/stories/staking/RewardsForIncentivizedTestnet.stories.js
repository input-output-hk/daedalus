// @flow
import React from 'react';
import { map } from 'lodash';
import BigNumber from 'bignumber.js';
import { action } from '@storybook/addon-actions';

// Screens
import StakingRewardsForIncentivizedTestnet from '../../../source/renderer/app/components/staking/rewards/StakingRewardsForIncentivizedTestnet';

// Dummy data initialization
import REWARDS from '../../../source/renderer/app/config/stakingRewards.dummy.json';

const rewards = map(REWARDS, reward => {
  return {
    ...reward,
    reward: new BigNumber(reward.reward),
  };
});

export const StakingRewardsForIncentivizedTestnetStory = () => (
  <StakingRewardsForIncentivizedTestnet
    rewards={rewards}
    isLoading={false}
    isExporting={false}
    onLearnMoreClick={action('onLearnMoreClick')}
    onExportCsv={action('onExportCsv')}
  />
);
