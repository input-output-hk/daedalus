'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.changeWalletSorting = exports.sortWallets = void 0;
const lodash_1 = require('lodash');
const sidebarTypes_1 = require('../types/sidebarTypes');
function sortWallets({ wallets, sortBy, sortOrder }) {
  const indexedWallets = wallets.map((w, index) => ({
    wallet: w,
    index,
    amount: w.amount.toNumber(),
    name: w.name.toLowerCase(),
  }));
  const doOrderBy = (fn) => {
    return (0, lodash_1.orderBy)(
      indexedWallets,
      fn,
      fn.map(() => sortOrder)
    ).map(({ wallet }) => wallet);
  };
  switch (sortBy) {
    case sidebarTypes_1.WalletSortBy.Date:
      return doOrderBy(['index']);
    case sidebarTypes_1.WalletSortBy.Balance:
      return doOrderBy(['amount', 'name', 'index']);
    case sidebarTypes_1.WalletSortBy.Name:
      return doOrderBy(['name', 'amount', 'index']);
    case sidebarTypes_1.WalletSortBy.None:
    default:
      return wallets;
  }
}
exports.sortWallets = sortWallets;
const defaultSortOrderByType = {
  [sidebarTypes_1.WalletSortBy.Name]: sidebarTypes_1.WalletSortOrder.Asc,
  [sidebarTypes_1.WalletSortBy.Date]: sidebarTypes_1.WalletSortOrder.Asc,
  [sidebarTypes_1.WalletSortBy.Balance]: sidebarTypes_1.WalletSortOrder.Desc,
};
function changeWalletSorting({ currentSortBy, sortBy, sortOrder }) {
  if (sortBy === currentSortBy) {
    return reverseWalletSortOrder({
      sortBy,
      sortOrder,
    });
  }
  return {
    sortBy,
    sortOrder: defaultSortOrderByType[sortBy],
  };
}
exports.changeWalletSorting = changeWalletSorting;
function reverseWalletSortOrder({ sortBy, sortOrder }) {
  return {
    sortBy,
    sortOrder:
      sortOrder === sidebarTypes_1.WalletSortOrder.Asc
        ? sidebarTypes_1.WalletSortOrder.Desc
        : sidebarTypes_1.WalletSortOrder.Asc,
  };
}
//# sourceMappingURL=walletSorting.js.map
