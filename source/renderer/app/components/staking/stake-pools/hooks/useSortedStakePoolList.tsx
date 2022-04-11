import { useMemo } from 'react';
import { orderBy } from 'lodash';
import StakePool from '../../../../domains/StakePool';
import { StakePoolSortableProps } from './types';

export enum StakePoolsOrder {
  Asc = 'asc',
  Desc = 'desc',
}

interface UseSortedStakePoolListArgs {
  stakePoolList: StakePool[];
  sortBy: StakePoolSortableProps;
  order: StakePoolsOrder;
}

export const useSortedStakePoolList = ({
  stakePoolList,
  sortBy,
  order,
}: UseSortedStakePoolListArgs) =>
  useMemo(
    () =>
      orderBy(
        stakePoolList.map((stakePool) => {
          let calculatedPledge;
          let calculatedCost;
          let formattedTicker;

          if (sortBy === 'ticker') {
            formattedTicker = stakePool.ticker
              .replace(/[^\w\s]/gi, '')
              .toLowerCase();
          }

          if (sortBy === 'pledge') {
            calculatedPledge = parseFloat(stakePool.pledge.toFixed(2));
          }

          if (sortBy === 'cost') {
            calculatedCost = parseFloat(stakePool.cost.toFixed(2));
          }

          return {
            ...stakePool,
            calculatedPledge,
            calculatedCost,
            formattedTicker,
          };
        }),
        ['formattedTicker', 'calculatedPledge', 'calculatedCost', sortBy],
        [order, order, order, order]
      ),
    [stakePoolList, order, sortBy]
  );
