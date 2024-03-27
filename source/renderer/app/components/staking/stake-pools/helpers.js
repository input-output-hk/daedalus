'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.getFilteredStakePoolsList = void 0;
const searchFields = ['id', 'ticker', 'name'];
const stakePoolsListSearch = (stakePool, rawSearch) => {
  const search = rawSearch.replace(/[.*+?^${}()|[\]\\]/g, '\\$&').trim();
  let pass = !search;
  searchFields.forEach((field) => {
    if (!pass) pass = RegExp(search, 'i').test(stakePool[field]);
  });
  return pass;
};
const getFilteredStakePoolsList = (stakePoolsList, search) =>
  stakePoolsList.filter((stakePool) => stakePoolsListSearch(stakePool, search));
exports.getFilteredStakePoolsList = getFilteredStakePoolsList;
//# sourceMappingURL=helpers.js.map
