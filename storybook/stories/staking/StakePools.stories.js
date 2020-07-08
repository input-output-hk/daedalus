// @flow
import React from 'react';
import { number } from '@storybook/addon-knobs';
import { action } from '@storybook/addon-actions';

import StakePools from '../../../source/renderer/app/components/staking/stake-pools/StakePools';
import STAKE_POOLS from '../../../source/renderer/app/config/stakingStakePools.dummy.json';
import { generateWallet } from '../_support/utils';

const dummyWallets = [
  generateWallet('Dummy1', '1000000000000'),
  generateWallet('Dummy2', '2000000000000'),
];

type Props = {
  currentTheme: string,
  locale: string,
  isLoading: boolean,
};

export const StakePoolsStory = (props: Props) => (
  <StakePools
    stakePoolsList={STAKE_POOLS.slice(
      0,
      number('Pools', 300, {
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
    onOpenExternalLink={action('onOpenExternalLink')}
    currentTheme={props.currentTheme}
    currentLocale={props.locale}
    onDelegate={action('onDelegate')}
    isLoading={props.isLoading}
    isRanking={false}
    onRank={() => null}
    wallets={dummyWallets}
    getStakePoolById={() => null}
  />
);
