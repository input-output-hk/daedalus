// @flow
import React from 'react';
import { number, radios } from '@storybook/addon-knobs';

import StakingStakePools from '../../source/renderer/app/components/staking/stake-pools/StakingStakePools';
import STAKE_POOLS from '../../source/renderer/app/config/stakingStakePools.dummy.json';

const themes = {
  'Light Blue': 'light-blue',
  Cardano: 'cardano',
  'Dark Blue': 'dark-blue',
};

export const StakingStakePoolsStory = () => (
  <StakingStakePools
    stakePoolsList={STAKE_POOLS.slice(
      0,
      number('Pools', 100, {
        range: true,
        min: 37,
        max: 300,
        step: 1,
      })
    )}
    stakePoolsDelegatingList={[
      STAKE_POOLS[1],
      STAKE_POOLS[3],
      STAKE_POOLS[20],
      STAKE_POOLS[36],
    ]}
    onOpenExternalLink={() => {}}
    currentTheme={radios('Theme', themes)}
  />
);
