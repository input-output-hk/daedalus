// @flow
import React from 'react';
import { number, radios } from '@storybook/addon-knobs';

import StakePools from '../../source/renderer/app/components/staking/stake-pools/StakePools';
import { StakePoolThumbnail } from '../../source/renderer/app/components/staking/stake-pools/StakePoolThumbnail';
import STAKE_POOLS from '../../source/renderer/app/config/stakingStakePools.dummy.json';

const themes = {
  'Light Blue': 'light-blue',
  Cardano: 'cardano',
  'Dark Blue': 'dark-blue',
};

export const StakePoolsStory = () => (
  <StakePools
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
    currentTheme={radios('Theme (Only for tooltip colors)', themes)}
  />
);

const tooltipPosition = {
  top: 'top',
  right: 'right',
  bottom: 'bottom',
  left: 'left',
};

export const StakePoolTooltipStory = () => (
  <div
    style={{
      margin: 50,
      position: 'relative',
    }}
  >
    <StakePoolThumbnail
      stakePool={STAKE_POOLS[1]}
      index={0}
      isSelected
      currentTheme={radios('Theme (Only for params colors)', themes)}
      tooltipPosition={radios('tooltipPosition', tooltipPosition, 'right')}
      tooltipOffset={number('tooltipOffset')}
      onClick={() => {}}
      onClose={() => {}}
      onOpenExternalLink={() => {}}
    />
  </div>
);
