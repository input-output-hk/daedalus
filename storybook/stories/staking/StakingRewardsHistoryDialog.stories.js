// @flow
import React, { useState } from 'react';
import { action } from '@storybook/addon-actions';
import { boolean, select, number } from '@storybook/addon-knobs';

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
  const rewardsResults = getRewardsHistory(
    number('Number of items', 10, {
      range: true,
      min: 1,
      max: 300,
      step: 10,
    })
  );
  const rewardsIdsOnly = rewardsResults.map((rew) => ({
    ...rew,
    pool: {
      id: rew.pool.id,
    },
  }));
  let rewards = rewardsResults;

  const rewardsSelect = select(
    'Results',
    {
      Regular: 'regular',
      'Ids only': 'idsOnly',
      'No results': 'noData',
    },
    'Regular'
  );
  if (rewardsSelect === 'idsOnly') {
    rewards = rewardsIdsOnly;
  } else if (rewardsSelect === 'noData') {
    rewards = [];
  }

  const reward = {
    date: now.toString(),
    walletId: '1',
    walletName: 'Wallet name',
    reward: rewardsResults[0].amount,
    rewardsAddress:
      'stake_test1upqaj6kvt9w69uraqtdfsv2q7l4000k5n5y4r26hnkzenmsel3qv9',
    pool: rewardsResults[0].pool,
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
      onCopyAddress={action('onCopy')}
      onExportCSV={action('onExportCSV')}
      onSetDateRange={(newDateRange) => setDateRange(newDateRange)}
      currentTheme={currentTheme}
      onOpenExternalLink={action('onOpenExternalLink')}
      onNoDataClick={action('onNoDataClick')}
    />
  );
};
