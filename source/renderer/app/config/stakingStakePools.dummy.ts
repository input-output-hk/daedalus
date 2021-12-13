import BigNumber from 'bignumber.js';
import STAKE_POOLS from './stakingStakePools.dummy.json';

export default STAKE_POOLS.map(
  ({
    relativeStake,
    potentialRewards,
    cost,
    pledge,
    retiring,
    ...stakePool
  }) => ({
    ...stakePool,
    relativeStake: new BigNumber(relativeStake),
    potentialRewards: new BigNumber(potentialRewards),
    cost: new BigNumber(cost),
    pledge: new BigNumber(pledge),
    retiring: new Date(retiring),
  })
);
