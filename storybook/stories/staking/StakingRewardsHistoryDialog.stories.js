// @flow
import React, { useState } from 'react';
import { action } from '@storybook/addon-actions';
import { boolean, number } from '@storybook/addon-knobs';

// Screens
import StakingRewardsHistoryDialog from '../../../source/renderer/app/components/staking/rewards/StakingRewardsHistoryDialog';

// Dummy data initialization
import { getRewardsHistory } from '../../../source/renderer/app/config/rewardsHistory.dummy';

const now = new Date();

export const StakingRewardsHistoryStory = () => {
  const [dateRange, setDateRange] = useState({ startDate: now, endDate: null });
  const noi = number('Number of itemzz', 10);
  console.log('noi', noi);
  const rewards = getRewardsHistory(noi);
  console.log('rewards', rewards);
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
      isFetchingRewardsHistory={boolean('isFetchingRewardsHistory', false)}
      onClose={action('onClose')}
      onCopy={action('onCopy')}
      onExportCSV={action('onExportCSV')}
      onSetDateRange={(newDateRange) => setDateRange(newDateRange)}
      currentTheme="light-blue"
      onOpenExternalLink={action('onOpenExternalLink')}
    />
  );
};
