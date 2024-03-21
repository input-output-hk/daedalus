'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.useSortedStakePoolList = exports.StakePoolsOrder = void 0;
const react_1 = require('react');
const lodash_1 = require('lodash');
var StakePoolsOrder;
(function (StakePoolsOrder) {
  StakePoolsOrder['Asc'] = 'asc';
  StakePoolsOrder['Desc'] = 'desc';
})(
  (StakePoolsOrder = exports.StakePoolsOrder || (exports.StakePoolsOrder = {}))
);
const useSortedStakePoolList = ({ stakePoolList, sortBy, order }) =>
  (0, react_1.useMemo)(
    () =>
      (0, lodash_1.orderBy)(
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
exports.useSortedStakePoolList = useSortedStakePoolList;
//# sourceMappingURL=useSortedStakePoolList.js.map
