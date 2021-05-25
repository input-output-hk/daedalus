// @flow
import hash from 'hash.js';
import BigNumber from 'bignumber.js';
import { map, random } from 'lodash';
import { GetRewardHistoryRequest } from '../api/staking/types';

const date = new Date();

export const generateId = (index: number) => {
  return hash
    .sha224()
    .update(date.getTime() + index)
    .digest('hex');
};

const ids = [...Array(100).fill()].map((x, index) => generateId(index));

const generateDate = (index: number) =>
  new Date(date.getTime() - index * 1000000000);

const generatePool = (index: number) => ({
  id: ids[index],
  ticker: `POOL${index}`,
  homepage: '',
  relativeStake: new BigNumber(index),
  producedBlocks: 100,
  potentialRewards: new BigNumber(index),
  nonMyopicMemberRewards: 1,
  description: '',
  cost: new BigNumber(index),
  pledge: new BigNumber(index),
  isCharity: false,
  name: `Stake Pool ${index}`,
  profitMargin: 100,
  ranking: index,
  retiring: null,
  saturation: index,
});

const generateEpoch = (index: number) => 130 + index;

const generateReward = (index: number) => new BigNumber(35 + index);

export const rewardsHistory = (numberOfItems: ?number = 10) =>
  map(Array(numberOfItems).fill(), (x, index) => ({
    date: generateDate(index),
    epoch: generateEpoch(index),
    pool: generatePool(index),
    reward: generateReward(index),
  }));

export const getRewardsApiHistoryDummyResponse = (
  // eslint-disable-next-line
  request: GetRewardHistoryRequest
) =>
  map(Array(10).fill(), (x, index) => ({
    date: generateDate(index),
    amount: random(100000000, 1000000000),
    earnedIn: {
      number: random(90, 99),
    },
    stakePool: {
      id: 'REPLACE',
    },
  }));
