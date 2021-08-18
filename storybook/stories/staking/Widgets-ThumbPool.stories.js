// @flow
import React from 'react';
import { action } from '@storybook/addon-actions';
import { boolean, number, text } from '@storybook/addon-knobs';

// Screens
import { ThumbPool } from '../../../source/renderer/app/components/staking/widgets/ThumbPool';

// Dummy data
import STAKE_POOLS from '../../../source/renderer/app/config/stakingStakePools.dummy.json';

const { cost, ranking, ticker, ...pool } = STAKE_POOLS[0];

export const ThumbPoolStory = ({ currentTheme }: { currentTheme: string }) => (
  <div style={{ padding: 30 }}>
    <ThumbPool
      currentTheme={currentTheme}
      isSelected={boolean('isSelected', false)}
      onOpenExternalLink={action('onOpenExternalLink')}
      onSelect={action('onSelect')}
      stakePool={{
        ...pool,
        cost: number('cost', cost),
        ranking: number('ranking', ranking),
        retiring: boolean('retiring', false)
          ? '2030-01-01T01:01:01.000Z'
          : null,
        ticker: text('Pool - Ticker', ticker),
        didNotMetPledge: boolean('didNotMetPledge', false),
      }}
      containerClassName="container"
      numberOfRankedStakePools={number('numberOfRankedStakePools', 100)}
      disabledStakePoolId={STAKE_POOLS[1].id}
      selectOnClick={boolean('selectOnClick', false)}
      showWithSelectButton={boolean('showWithSelectButton', false)}
    />
  </div>
);
