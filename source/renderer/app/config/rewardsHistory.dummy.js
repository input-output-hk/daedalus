// @flow
import hash from 'hash.js';
import BigNumber from 'bignumber.js';
import faker from 'faker';
import { map, random } from 'lodash';
import type { GetRewardsHistoryRequest } from '../api/staking/types';
import stakePoolsId from './rewardsHistory.stakePoolsId.dummy.json';

const date = new Date();

export const generateId = (index: number) => {
  const id = hash
    .sha224()
    .update(date.getTime() + index)
    .digest('hex');
  const randomNumber = parseInt(Math.random() * 19, 10);
  return `${id}${randomNumber}`;
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
  name: faker.company.companyName(),
  profitMargin: 100,
  ranking: index,
  retiring: null,
  saturation: index,
});

const generateEpoch = (index: number) => 130 + index;

const generateReward = (index: number) => new BigNumber(35 + index);

const timeout = (ms) => new Promise((resolve) => setTimeout(resolve, ms));

export const getRewardsHistory = (numberOfItems: ?number = 10) =>
  map(Array(numberOfItems).fill(), (x, index) => ({
    // date: generateDate(index),
    epoch: generateEpoch(index),
    pool: generatePool(index),
    amount: generateReward(index),
  }));

export const getRewardsApiHistoryDummyResponse = async (
  // eslint-disable-next-line
  request: GetRewardsHistoryRequest
) => {
  await timeout(2000);
  return map(Array(10).fill(), (x, index) => ({
    address: generateId(index),
    date: generateDate(index),
    amount: random(100000000, 1000000000),
    earnedIn: {
      number: random(90, 99),
    },
    stakePool: {
      id: stakePoolsId[random(0, stakePoolsId.length - 1)],
    },
  }));
};
