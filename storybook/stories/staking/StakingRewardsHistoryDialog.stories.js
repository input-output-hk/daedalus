// @flow
import React, { useState } from 'react';
import { action } from '@storybook/addon-actions';
import { boolean } from '@storybook/addon-knobs';

// Screens
import StakingRewardsHistoryDialog from '../../../source/renderer/app/components/staking/rewards/StakingRewardsHistoryDialog';

// Dummy data initialization
import { getRewardsHistory } from '../../../source/renderer/app/config/rewardsHistory.dummy';

const rewards = getRewardsHistory(10);

const now = new Date();

export const StakingRewardsHistoryStory = () => {
  const [dateRange, setDateRange] = useState({ startDate: now, endDate: null });
  return (
    <StakingRewardsHistoryDialog
      startDate={dateRange.startDate}
      endDate={dateRange.endDate}
      reward={{
        ...rewards[0],
        walletName: 'Wallet name',
        rewardsAddress:
          'stake_test1upqaj6kvt9w69uraqtdfsv2q7l4000k5n5y4r26hnkzenmsel3qv9',
      }}
      rewardsHistory={rewards}
      currentDateFormat="MM/DD/YYYY"
      currentLocale="en-US"
      isFetchingRewardsHistory={boolean('isFetchingRewardsHistory', true)}
      onClose={action('onClose')}
      onCopy={action('onCopy')}
      onExportCSV={action('onExportCSV')}
      onSetDateRange={(newDateRange) => setDateRange(newDateRange)}
    />
  );
};
