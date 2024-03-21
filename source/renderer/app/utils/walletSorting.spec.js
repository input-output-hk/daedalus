'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const bignumber_js_1 = __importDefault(require('bignumber.js'));
const sidebarTypes_1 = require('../types/sidebarTypes');
const walletSorting_1 = require('./walletSorting');
describe('Wallet Sorting', () => {
  const cases = [
    // Sort wallets by DATE from ASC to DESC
    [
      sidebarTypes_1.WalletSortBy.Date,
      sidebarTypes_1.WalletSortOrder.Desc,
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
      sidebarTypes_1.WalletSortBy.Date,
      sidebarTypes_1.WalletSortOrder.Asc,
      [
        ['1', 'Wallet A', 1],
        ['2', 'Wallet B', 2],
      ],
      [
        ['1', 'Wallet A', 1],
        ['2', 'Wallet B', 2],
      ],
    ],
    [
      sidebarTypes_1.WalletSortBy.Name,
      sidebarTypes_1.WalletSortOrder.Desc,
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
        ['2', 'Wallet B', 2],
        ['1', 'Wallet A', 1],
      ],
      [
        ['1', 'Wallet A', 1],
        ['2', 'Wallet B', 2],
      ],
    ],
    [
      sidebarTypes_1.WalletSortBy.Balance,
      sidebarTypes_1.WalletSortOrder.Desc,
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
      sidebarTypes_1.WalletSortOrder.Asc,
      [
        ['2', 'Wallet B', 2],
        ['1', 'Wallet A', 1],
      ],
      [
        ['1', 'Wallet A', 1],
        ['2', 'Wallet B', 2],
      ],
    ],
    [
      sidebarTypes_1.WalletSortBy.Name,
      sidebarTypes_1.WalletSortOrder.Asc,
      [
        ['2', 'Wallet A', 2],
        ['1', 'Wallet A', 1],
      ],
      [
        ['1', 'Wallet A', 1],
        ['2', 'Wallet A', 2],
      ],
    ],
    [
      sidebarTypes_1.WalletSortBy.Balance,
      sidebarTypes_1.WalletSortOrder.Asc,
      [
        ['2', 'Wallet B', 1],
        ['1', 'Wallet A', 1],
      ],
      [
        ['1', 'Wallet A', 1],
        ['2', 'Wallet B', 1],
      ],
    ],
    [
      sidebarTypes_1.WalletSortBy.Balance,
      sidebarTypes_1.WalletSortOrder.Desc,
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
      const sortedWallets = (0, walletSorting_1.sortWallets)({
        wallets: wallets.map(([id, name, amount]) => ({
          id,
          name,
          amount: new bignumber_js_1.default(amount),
        })),
        sortBy,
        sortOrder,
      });
      expect(sortedWallets).toEqual(
        expected.map(([id, name, amount]) => ({
          id,
          name,
          amount: new bignumber_js_1.default(amount),
        }))
      );
    }
  );
});
//# sourceMappingURL=walletSorting.spec.js.map
