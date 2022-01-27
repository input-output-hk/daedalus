// @flow

import { orderBy } from 'lodash';
import { WalletSortBy, WalletSortOrder } from '../types/sidebarTypes';
import type {
  WalletSortConfig,
  WalletSortByOptions,
} from '../types/sidebarTypes';
import type Wallet from '../domains/Wallet';

type IndexedWallet = {
  wallet: Wallet,
  index: number,
  amount: number,
  name: string,
};

type SortWalletsProps = {
  wallets: Wallet[],
} & WalletSortConfig;

export function sortWallets({
  wallets,
  sortBy,
  sortOrder,
}: SortWalletsProps): Wallet[] {
  const indexedWallets: IndexedWallet[] = wallets.map(
    (w: Wallet, index: number) => ({
      wallet: w,
      index,
      amount: w.amount.toNumber(),
      name: w.name.toLowerCase(),
    })
  );

  const doOrderBy = (fn: string[]) => {
    return orderBy(
      indexedWallets,
      fn,
      fn.map(() => sortOrder)
    ).map(({ wallet }) => wallet);
  };

  switch (sortBy) {
    case WalletSortBy.Date:
      return doOrderBy(['index']);
    case WalletSortBy.Balance:
      return doOrderBy(['amount', 'name', 'index']);
    case WalletSortBy.Name:
      return doOrderBy(['name', 'amount', 'index']);
    case WalletSortBy.None:
    default:
      return wallets;
  }
}

const defaultSortOrderByType = {
  [WalletSortBy.Name]: WalletSortOrder.Asc,
  [WalletSortBy.Date]: WalletSortOrder.Asc,
  [WalletSortBy.Balance]: WalletSortOrder.Desc,
};

type ChangeWalletSortingProps = {
  currentSortBy: WalletSortByOptions,
} & WalletSortConfig;

export function changeWalletSorting({
  currentSortBy,
  sortBy,
  sortOrder,
}: ChangeWalletSortingProps) {
  if (sortBy === currentSortBy) {
    return reverseWalletSortOrder({ sortBy, sortOrder });
  }

  return {
    sortBy,
    sortOrder: defaultSortOrderByType[sortBy],
  };
}

function reverseWalletSortOrder({ sortBy, sortOrder }: WalletSortConfig) {
  return {
    sortBy,
    sortOrder:
      sortOrder === WalletSortOrder.Asc
        ? WalletSortOrder.Desc
        : WalletSortOrder.Asc,
  };
}
