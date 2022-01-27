// @flow
import BigNumber from 'bignumber.js';

import { WalletSortBy, WalletSortOrder } from '../types/sidebarTypes';

import { sortWallets } from './walletSorting';

describe('Wallet Sorting', () => {
  const cases = [
    // Sort wallets by DATE from ASC to DESC
    [
      WalletSortBy.Date,
      WalletSortOrder.Desc,
      [
        ['1', 'Wallet A', 1],
        ['2', 'Wallet B', 2],
      ],
      [
        ['2', 'Wallet B', 2],
        ['1', 'Wallet A', 1],
      ],
    ],
    // // Sort wallets by DATE from ASC to DESC
    [
      WalletSortBy.Date,
      WalletSortOrder.Asc,
      [
        ['1', 'Wallet A', 1],
        ['2', 'Wallet B', 2],
      ],
      [
        ['1', 'Wallet A', 1],
        ['2', 'Wallet B', 2],
      ],
    ],
    // Sort wallets by NAME from A-Z to Z-A
    [
      WalletSortBy.Name,
      WalletSortOrder.Desc,
      [
        ['1', 'Wallet A', 1],
        ['2', 'Wallet B', 2],
      ],
      [
        ['2', 'Wallet B', 2],
        ['1', 'Wallet A', 1],
      ],
    ],
    // Sort wallets by NAME from Z-A to A-Z
    [
      WalletSortBy.Name,
      WalletSortOrder.Asc,
      [
        ['2', 'Wallet B', 2],
        ['1', 'Wallet A', 1],
      ],
      [
        ['1', 'Wallet A', 1],
        ['2', 'Wallet B', 2],
      ],
    ],
    // Sort wallets by BALANCE from higher to lower
    [
      WalletSortBy.Balance,
      WalletSortOrder.Desc,
      [
        ['1', 'Wallet A', 1],
        ['2', 'Wallet B', 2],
      ],
      [
        ['2', 'Wallet B', 2],
        ['1', 'Wallet A', 1],
      ],
    ],
    // Sort wallets by BALANCE from lower to higher
    [
      WalletSortBy.Balance,
      WalletSortOrder.Asc,
      [
        ['2', 'Wallet B', 2],
        ['1', 'Wallet A', 1],
      ],
      [
        ['1', 'Wallet A', 1],
        ['2', 'Wallet B', 2],
      ],
    ],
    // Sort wallet with same NAME
    [
      WalletSortBy.Name,
      WalletSortOrder.Asc,
      [
        ['2', 'Wallet A', 2],
        ['1', 'Wallet A', 1],
      ],
      [
        ['1', 'Wallet A', 1],
        ['2', 'Wallet A', 2],
      ],
    ],
    // Sort wallet with same BALANCE
    [
      WalletSortBy.Balance,
      WalletSortOrder.Asc,
      [
        ['2', 'Wallet B', 1],
        ['1', 'Wallet A', 1],
      ],
      [
        ['1', 'Wallet A', 1],
        ['2', 'Wallet B', 1],
      ],
    ],
    // Sort wallets with same NAME & BALANCE
    [
      WalletSortBy.Balance,
      WalletSortOrder.Desc,
      [
        ['1', 'Wallet A', 1],
        ['2', 'Wallet A', 1],
      ],
      [
        ['2', 'Wallet A', 1],
        ['1', 'Wallet A', 1],
      ],
    ],
  ];

  test.each(cases)(
    `should sort wallets from test case %# by %s on %s order`,
    (sortBy, sortOrder, wallets, expected) => {
      const sortedWallets = sortWallets({
        wallets: wallets.map(([id, name, amount]) => ({
          id,
          name,
          amount: new BigNumber(amount),
        })),
        sortBy,
        sortOrder,
      });

      expect(sortedWallets).toEqual(
        expected.map(([id, name, amount]) => ({
          id,
          name,
          amount: new BigNumber(amount),
        }))
      );
    }
  );
});
