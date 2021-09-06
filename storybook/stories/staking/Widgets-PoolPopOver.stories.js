// @flow
import React from 'react';
import { action } from '@storybook/addon-actions';
import { boolean, number, text } from '@storybook/addon-knobs';

// Screens
import { PoolPopOver } from '../../../source/renderer/app/components/staking/widgets/PoolPopOver';

// Dummy data
import STAKE_POOLS from '../../../source/renderer/app/config/stakingStakePools.dummy.json';

const { cost, ranking, ticker, ...pool } = STAKE_POOLS[0];

export const PoolPopOverStory = ({
  currentTheme,
}: {
  currentTheme: string,
}) => (
  <div style={{ padding: 30 }}>
    <PoolPopOver
      color="#5e6066"
      containerClassName="container"
      currentTheme={currentTheme}
      numberOfRankedStakePools={number('numberOfRankedStakePools', 100)}
      onClose={action('onClose')}
      onOpen={action('onOpen')}
      onOpenExternalLink={action('onOpenExternalLink')}
      onSelect={action('onSelect')}
      openOnHover={boolean('openOnHover', true)}
      openWithDelay={boolean('openWithDelay', true)}
      showWithSelectButton={boolean('showWithSelectButton', false)}
      stakePool={{
        ...pool,
        cost: number('cost', cost),
        ranking: number('ranking', ranking),
        ticker: text('Pool - Ticker', ticker),
        retiring: boolean('retiring', false)
          ? '2030-01-01T01:01:01.000Z'
          : null,
        pledgeNotMet: boolean('pledgeNotMet', false),
      }}
      visible
    >
      <div style={{ background: '#ccc', padding: 10 }}>Hover this area</div>
    </PoolPopOver>
  </div>
);
