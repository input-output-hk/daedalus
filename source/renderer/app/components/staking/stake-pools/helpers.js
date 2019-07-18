// @flow
import moment from 'moment';
import type { StakePool, StakePoolsListType } from '../../../api/staking/types';
import type { Filters } from './StakePoolsSearch';

const searchFields = ['slug', 'name'];
const NEW_POOLS_MONTHS_OLD = 3;

const stakePoolsListSearch = (stakePool: StakePool, rawSearch: string) => {
  const search = rawSearch.replace(/[^a-zA-Z0-9 ]/g, '');
  let pass = !search;
  searchFields.forEach((field: string) => {
    if (!pass) pass = RegExp(search, 'i').test(stakePool[field]);
  });
  return pass;
};

const stakePoolsListFilter = (stakePool: StakePool, filters: Filters) =>
  filters.reduce(
    (pass, filter) => filter === 'all' || filtersFn[filter](stakePool),
    true
  );

const filtersFn = {
  charity: (stakePool: StakePool) => stakePool.isCharity,
  new: (stakePool: StakePool) =>
    moment(stakePool.created_at).diff(moment(), 'months', true) <
    NEW_POOLS_MONTHS_OLD,
};

export const getFilteredStakePoolsList = (
  stakePoolsList: StakePoolsListType,
  search: string,
  filters?: Filters = []
): Array<any> =>
  stakePoolsList
    .filter((stakePool: StakePool) => stakePoolsListSearch(stakePool, search))
    .filter((stakePool: StakePool) => stakePoolsListFilter(stakePool, filters));
