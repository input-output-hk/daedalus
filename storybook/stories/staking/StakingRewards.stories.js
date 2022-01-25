// @flow
import React from 'react';
import { map } from 'lodash';
import BigNumber from 'bignumber.js';
import { action } from '@storybook/addon-actions';

// Screens
import StakingRewards from '../../../source/renderer/app/components/staking/rewards/StakingRewards';

// Dummy data initialization
import REWARDS from '../../../source/renderer/app/config/stakingRewards.dummy.json';

const rewards = map(REWARDS, (reward, index) => {
  return {
    ...reward,
    reward: new BigNumber(reward.reward),
    walletName: reward.wallet,
    isRestoring: index === 1,
    syncingProgress: index === 1 ? 40 : 100,
  };
});

export const StakingRewardsStory = () => (
  <StakingRewards
    rewards={rewards}
    isLoading={false}
    isExporting={false}
    onLearnMoreClick={action('onLearnMoreClick')}
    onExportCsv={action('onExportCsv')}
    onCopyAddress={action('onCopyAddress')}
    onOpenExternalLink={action('onOpenExternalLink')}
    onOpenWalletRewards={action('onOpenExternalLink')}
  />
);
