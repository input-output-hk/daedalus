// @flow
import React, { useState } from 'react';
import { action } from '@storybook/addon-actions';
import { boolean } from '@storybook/addon-knobs';

// Screens
import StakingRewardsHistoryDialog from '../../../source/renderer/app/components/staking/rewards/StakingRewardsHistoryDialog';

// Dummy data initialization
import { getRewardsHistory } from '../../../source/renderer/app/config/rewardsHistory.dummy';

const now = new Date();

export const StakingRewardsHistoryStory = ({
  currentTheme,
  locale,
}: {
  currentTheme: string,
  locale: string,
}) => {
  const [dateRange, setDateRange] = useState({ startDate: now, endDate: null });
  const rewards = getRewardsHistory(10);
  const reward = {
    date: now.toString(),
    walletId: '1',
    walletName: 'Wallet name',
    reward: rewards[0].amount,
    rewardsAddress:
      'stake_test1upqaj6kvt9w69uraqtdfsv2q7l4000k5n5y4r26hnkzenmsel3qv9',
    pool: rewards[0].pool,
  };
  return (
    <StakingRewardsHistoryDialog
      startDate={dateRange.startDate}
      endDate={dateRange.endDate}
      reward={reward}
      rewardsHistory={rewards}
      currentDateFormat="MM/DD/YYYY"
      currentLocale={locale}
      isFetchingRewardsHistory={boolean('isFetchingRewardsHistory', false)}
      onClose={action('onClose')}
      onCopy={action('onCopy')}
      onExportCSV={action('onExportCSV')}
      onSetDateRange={(newDateRange) => setDateRange(newDateRange)}
      currentTheme={currentTheme}
      onOpenExternalLink={action('onOpenExternalLink')}
    />
  );
};
