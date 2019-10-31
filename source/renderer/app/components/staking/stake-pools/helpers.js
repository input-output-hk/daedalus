// @flow
import type { StakePool, StakePoolsListType } from '../../../api/staking/types';

const searchFields = ['slug', 'name'];

const stakePoolsListSearch = (stakePool: StakePool, rawSearch: string) => {
  const search = rawSearch.replace(/[.*+?^${}()|[\]\\]/g, '\\$&').trim();
  let pass = !search;
  searchFields.forEach((field: string) => {
    if (!pass) pass = RegExp(search, 'i').test(stakePool[field]);
  });
  return pass;
};

export const getFilteredStakePoolsList = (
  stakePoolsList: StakePoolsListType,
  search: string
): StakePoolsListType =>
  stakePoolsList.filter((stakePool: StakePool) =>
    stakePoolsListSearch(stakePool, search)
  );
