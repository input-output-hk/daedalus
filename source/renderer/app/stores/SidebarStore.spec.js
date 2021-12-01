// @flow
import BigNumber from 'bignumber.js';

import type { Api } from '../api/index';
import type { ActionsMap } from '../actions/index';
import type {
  WalletSortByOptions,
  WalletSortOrderOptions,
} from '../types/sidebarTypes';
import { WalletSortBy, WalletSortOrder } from '../types/sidebarTypes';

import SidebarStore from './SidebarStore';

describe('discreetWalletAmount replacer', () => {
  const api: Api = ({
    ada: jest.fn(),
    localStorage: jest.fn(),
  }: any);

  const actions: ActionsMap = (jest.fn(): any);

  function setupStore({
    sortBy,
    sortOrder,
    wallets,
  }: {
    sortBy: WalletSortByOptions,
    sortOrder: WalletSortOrderOptions,
    wallets: Array<{ id: string, name: string, amount: BigNumber }>,
  }) {
    const sidebarStore = new SidebarStore(api, actions);
    sidebarStore.walletSortConfig.sortBy = sortBy;
    sidebarStore.walletSortConfig.sortOrder = sortOrder;

    return sidebarStore._sortWallets((wallets: any));
  }

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
      const sortedWallets = setupStore({
        sortBy,
        sortOrder,
        wallets: wallets.map(([id, name, amount]) => ({
          id,
          name,
          amount: new BigNumber(amount),
        })),
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
