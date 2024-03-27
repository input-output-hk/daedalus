'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const bignumber_js_1 = __importDefault(require('bignumber.js'));
const sidebarTypes_1 = require('../types/sidebarTypes');
const SidebarStore_1 = __importDefault(require('./SidebarStore'));
const analytics_1 = require('../analytics');
describe('Sidebar Store', () => {
  const api = {
    ada: jest.fn(),
    localStorage: jest.fn(),
  };
  const actions = jest.fn();
  function setupStore({ wallets }) {
    const sidebarStore = new SidebarStore_1.default(
      api,
      actions,
      analytics_1.noopAnalyticsTracker
    );
    sidebarStore.stores = {
      wallets: {
        all: wallets,
      },
      hardwareWallets: jest.fn(),
      walletSettings: {
        getWalletsRecoveryPhraseVerificationData: jest.fn().mockReturnValue({
          hasNotification: false,
        }),
      },
      networkStatus: {
        isConnected: true,
      },
    };
    return sidebarStore;
  }
  function pickAssertionProps(wallets) {
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
          amount: new bignumber_js_1.default(1),
          isLegacy: false,
        },
        {
          id: '2',
          name: 'Wallet B',
          amount: new bignumber_js_1.default(2),
          isLegacy: false,
        },
      ],
    });
    expect(pickAssertionProps(sidebarStore.wallets)).toEqual([
      {
        id: '1',
        title: 'Wallet A',
        amount: new bignumber_js_1.default(1),
        isLegacy: false,
      },
      {
        id: '2',
        title: 'Wallet B',
        amount: new bignumber_js_1.default(2),
        isLegacy: false,
      },
    ]);
  });
  const defaultSortingCases = [
    // Sort wallets by NAME from A-Z as default order
    [
      sidebarTypes_1.WalletSortBy.Name,
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
    [
      sidebarTypes_1.WalletSortBy.Balance,
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
          amount: new bignumber_js_1.default(amount),
          isLegacy,
        })),
      });
      sidebarStore.onChangeWalletSortType(sortBy);
      expect(pickAssertionProps(sidebarStore.wallets)).toEqual(
        expected.map(([id, title, amount, isLegacy = false]) => ({
          id,
          amount: new bignumber_js_1.default(amount),
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
          amount: new bignumber_js_1.default(1000),
          isLegacy: true,
        },
        {
          id: '2',
          name: 'Byron Wallet B',
          amount: new bignumber_js_1.default(200),
          isLegacy: true,
        },
        {
          id: '3',
          name: 'Shelley Wallet A',
          amount: new bignumber_js_1.default(1000),
          isLegacy: false,
        },
        {
          id: '4',
          name: 'Shelley Wallet B',
          amount: new bignumber_js_1.default(200),
          isLegacy: false,
        },
      ],
    });
    sidebarStore.onChangeWalletSortType(sidebarTypes_1.WalletSortBy.Balance);
    expect(pickAssertionProps(sidebarStore.wallets)).toEqual([
      {
        id: '3',
        title: 'Shelley Wallet A',
        amount: new bignumber_js_1.default(1000),
        isLegacy: false,
      },
      {
        id: '4',
        title: 'Shelley Wallet B',
        amount: new bignumber_js_1.default(200),
        isLegacy: false,
      },
      {
        id: '1',
        title: 'Byron Wallet A',
        amount: new bignumber_js_1.default(1000),
        isLegacy: true,
      },
      {
        id: '2',
        title: 'Byron Wallet B',
        amount: new bignumber_js_1.default(200),
        isLegacy: true,
      },
    ]);
  });
  const reverseSortingOrderCases = [
    // Sort wallets by DATE by reversing from ASC to DESC
    [
      sidebarTypes_1.WalletSortBy.Date,
      sidebarTypes_1.WalletSortOrder.Asc,
      [
        ['1', 'Wallet A', 1],
        ['2', 'Wallet B', 2],
      ],
      [
        ['2', 'Wallet B', 2],
        ['1', 'Wallet A', 1],
      ],
    ],
    [
      sidebarTypes_1.WalletSortBy.Name,
      sidebarTypes_1.WalletSortOrder.Asc,
      [
        ['1', 'Wallet A', 1],
        ['2', 'Wallet B', 2],
      ],
      [
        ['2', 'Wallet B', 2],
        ['1', 'Wallet A', 1],
      ],
    ],
    [
      sidebarTypes_1.WalletSortBy.Balance,
      sidebarTypes_1.WalletSortOrder.Desc,
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
          amount: new bignumber_js_1.default(amount),
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
          amount: new bignumber_js_1.default(amount),
          title,
          isLegacy,
        }))
      );
    }
  );
});
//# sourceMappingURL=SidebarStore.spec.js.map
