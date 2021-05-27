// @flow
import React from 'react';
import { action } from '@storybook/addon-actions';

// Screens
import StakingRewardsHistoryDialog from '../../../source/renderer/app/components/staking/rewards/StakingRewardsHistoryDialog';

// Dummy data initialization
import { getRewardsHistory } from '../../../source/renderer/app/config/rewardsHistory.dummy';

const rewards = getRewardsHistory(10);

export const StakingRewardsHistoryStory = () => (
  <StakingRewardsHistoryDialog
    reward={{
      ...rewards[0],
      walletName: 'Wallet name',
      rewardsAddress:
        'stake_test1upqaj6kvt9w69uraqtdfsv2q7l4000k5n5y4r26hnkzenmsel3qv9',
    }}
    rewardsHistory={rewards}
    currentDateFormat="MM/DD/YYYY"
    currentLocale="en-US"
    isFetchingRewardsHistory={false}
    onClose={action('onClose')}
    onCopy={action('onCopy')}
  />
);
