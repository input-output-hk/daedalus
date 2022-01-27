// @flow
import BigNumber from 'bignumber.js';

import type { Api } from '../api/index';
import type { ActionsMap } from '../actions/index';
import { WalletSortBy, WalletSortOrder } from '../types/sidebarTypes';
import type { SidebarWalletType } from '../types/sidebarTypes';

import SidebarStore from './SidebarStore';

describe('Sidebar Store', () => {
  const api: Api = ({
    ada: jest.fn(),
    localStorage: jest.fn(),
  }: any);

  const actions: ActionsMap = (jest.fn(): any);

  function setupStore({
    wallets,
  }: {
    wallets: Array<{
      id: string,
      name: string,
      amount: BigNumber,
      isLegacy?: boolean,
    }>,
  }) {
    const sidebarStore = new SidebarStore(api, actions);
    sidebarStore.stores = ({
      wallets: { all: wallets },
      hardwareWallets: jest.fn(),
      walletSettings: {
        getWalletsRecoveryPhraseVerificationData: jest
          .fn()
          .mockReturnValue({ hasNotification: false }),
      },
      networkStatus: {
        isConnected: true,
      },
    }: any);

    return sidebarStore;
  }

  function pickAssertionProps(wallets: SidebarWalletType[]) {
    return wallets.map(({ id, amount, title, isLegacy }) => ({
      id,
      amount,
      title,
      isLegacy,
    }));
  }

  it('should sort wallets initially by DATE from oldest to newest', () => {
    const sidebarStore = setupStore({
      wallets: [
        {
          id: '1',
          name: 'Wallet A',
          amount: new BigNumber(1),
          isLegacy: false,
        },
        {
          id: '2',
          name: 'Wallet B',
          amount: new BigNumber(2),
          isLegacy: false,
        },
      ],
    });

    expect(pickAssertionProps(sidebarStore.wallets)).toEqual([
      {
        id: '1',
        title: 'Wallet A',
        amount: new BigNumber(1),
        isLegacy: false,
      },
      {
        id: '2',
        title: 'Wallet B',
        amount: new BigNumber(2),
        isLegacy: false,
      },
    ]);
  });

  const defaultSortingCases = [
    // Sort wallets by NAME from A-Z as default order
    [
      WalletSortBy.Name,
      [
        ['1', 'Sigurd', 1600],
        ['2', 'Hel', 25],
        ['3', 'Loki', 0],
        ['4', 'empty', 0],
        ['5', 'empty', 0],
        ['6', 'Odin', 246, true],
        ['7', 'Jormungand', 0, true],
      ],
      [
        ['4', 'empty', 0],
        ['5', 'empty', 0],
        ['2', 'Hel', 25],
        ['3', 'Loki', 0],
        ['1', 'Sigurd', 1600],
        ['7', 'Jormungand', 0, true],
        ['6', 'Odin', 246, true],
      ],
    ],
    // Sort wallets by BALANCE from from higher to lower as default order
    [
      WalletSortBy.Balance,
      [
        ['1', 'Wallet A', 1],
        ['2', 'Wallet B', 2],
      ],
      [
        ['2', 'Wallet B', 2],
        ['1', 'Wallet A', 1],
      ],
    ],
  ];

  test.each(defaultSortingCases)(
    `should set default sorting for %s`,
    (sortBy, wallets, expected) => {
      const sidebarStore = setupStore({
        wallets: wallets.map(([id, name, amount, isLegacy = false]) => ({
          id,
          name,
          amount: new BigNumber(amount),
          isLegacy,
        })),
      });

      sidebarStore.onChangeWalletSortType(sortBy);

      expect(pickAssertionProps(sidebarStore.wallets)).toEqual(
        expected.map(([id, title, amount, isLegacy = false]) => ({
          id,
          amount: new BigNumber(amount),
          title,
          isLegacy,
        }))
      );
    }
  );

  it('should move Byron wallets at the bottom', () => {
    const sidebarStore = setupStore({
      wallets: [
        {
          id: '1',
          name: 'Byron Wallet A',
          amount: new BigNumber(1000),
          isLegacy: true,
        },
        {
          id: '2',
          name: 'Byron Wallet B',
          amount: new BigNumber(200),
          isLegacy: true,
        },
        {
          id: '3',
          name: 'Shelley Wallet A',
          amount: new BigNumber(1000),
          isLegacy: false,
        },
        {
          id: '4',
          name: 'Shelley Wallet B',
          amount: new BigNumber(200),
          isLegacy: false,
        },
      ],
    });

    sidebarStore.onChangeWalletSortType(WalletSortBy.Balance);

    expect(pickAssertionProps(sidebarStore.wallets)).toEqual([
      {
        id: '3',
        title: 'Shelley Wallet A',
        amount: new BigNumber(1000),
        isLegacy: false,
      },
      {
        id: '4',
        title: 'Shelley Wallet B',
        amount: new BigNumber(200),
        isLegacy: false,
      },
      {
        id: '1',
        title: 'Byron Wallet A',
        amount: new BigNumber(1000),
        isLegacy: true,
      },
      {
        id: '2',
        title: 'Byron Wallet B',
        amount: new BigNumber(200),
        isLegacy: true,
      },
    ]);
  });

  const reverseSortingOrderCases = [
    // Sort wallets by DATE by reversing from ASC to DESC
    [
      WalletSortBy.Date,
      WalletSortOrder.Asc,
      [
        ['1', 'Wallet A', 1],
        ['2', 'Wallet B', 2],
      ],
      [
        ['2', 'Wallet B', 2],
        ['1', 'Wallet A', 1],
      ],
    ],
    // Sort wallets by NAME by reversing from A-Z to Z-A
    [
      WalletSortBy.Name,
      WalletSortOrder.Asc,
      [
        ['1', 'Wallet A', 1],
        ['2', 'Wallet B', 2],
      ],
      [
        ['2', 'Wallet B', 2],
        ['1', 'Wallet A', 1],
      ],
    ],
    // Sort wallets by BALANCE by reversing from higher amount to lower ones
    [
      WalletSortBy.Balance,
      WalletSortOrder.Desc,
      [
        ['2', 'Wallet B', 2],
        ['1', 'Wallet A', 1],
      ],
      [
        ['1', 'Wallet A', 1],
        ['2', 'Wallet B', 2],
      ],
    ],
  ];

  test.each(reverseSortingOrderCases)(
    `should reverse sorting for %s on %s order`,
    (sortBy, sortOrder, wallets, expected) => {
      const sidebarStore = setupStore({
        wallets: wallets.map(([id, name, amount, isLegacy = false]) => ({
          id,
          name,
          amount: new BigNumber(amount),
          isLegacy,
        })),
      });

      sidebarStore.walletSortConfig = {
        sortBy,
        sortOrder,
      };

      sidebarStore.onChangeWalletSortType(sortBy);

      expect(pickAssertionProps(sidebarStore.wallets)).toEqual(
        expected.map(([id, title, amount, isLegacy = false]) => ({
          id,
          amount: new BigNumber(amount),
          title,
          isLegacy,
        }))
      );
    }
  );
});
