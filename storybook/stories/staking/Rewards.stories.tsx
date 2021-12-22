import React from 'react';
import { map } from 'lodash';
import BigNumber from 'bignumber.js';
import { action } from '@storybook/addon-actions';
// Screens
import StakingRewards from '../../../source/renderer/app/components/staking/rewards/StakingRewards';
// Dummy data initialization
import REWARDS from '../../../source/renderer/app/config/stakingRewards.dummy.json';

const rewards = map(REWARDS, (reward) => {
  return { ...reward, reward: new BigNumber(reward.reward) };
});
export const StakingRewardsStory = () => (
  <StakingRewards
    // @ts-ignore ts-migrate(2322) FIXME: Type '{ reward: BigNumber; date: string; wallet: s... Remove this comment to see the full error message
    rewards={rewards}
    isLoading={false}
    isExporting={false}
    onLearnMoreClick={action('onLearnMoreClick')}
    onExportCsv={action('onExportCsv')}
    onCopyAddress={action('onCopyAddress')}
    onOpenExternalLink={action('onOpenExternalLink')}
  />
);
