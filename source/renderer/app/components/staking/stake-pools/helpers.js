// @flow
import StakePool from '../../../domains/StakePool';
import type { StakePoolProps } from '../../../domains/StakePool';

const searchFields = ['ticker', 'name'];

const stakePoolsListSearch = (stakePool: StakePoolProps, rawSearch: string) => {
  const search = rawSearch.replace(/[.*+?^${}()|[\]\\]/g, '\\$&').trim();
  let pass = !search;
  searchFields.forEach((field: string) => {
    if (!pass) pass = RegExp(search, 'i').test(stakePool[field]);
  });
  return pass;
};

export const getFilteredStakePoolsList = (
  stakePoolsList: Array<StakePool>,
  search: string
): Array<StakePool> =>
  stakePoolsList.filter((stakePool: Object) =>
    stakePoolsListSearch(stakePool, search)
  );
