import type { Api } from '../api/index';
import type { ActionsMap } from '../actions/index';
import { noopAnalyticsTracker } from '../analytics';
import { exportWalletsChannel } from '../ipc/exportWalletsChannel';
import { logger } from '../utils/logging';
import WalletMigrationStore from './WalletMigrationStore';

jest.mock('../ipc/exportWalletsChannel', () => ({
  exportWalletsChannel: { request: jest.fn() },
}));
jest.mock('../ipc/show-file-dialog-channels', () => ({
  showOpenDialogChannel: { send: jest.fn() },
}));
jest.mock('../ipc/generateWalletMigrationReportChannel', () => ({
  generateWalletMigrationReportChannel: { send: jest.fn() },
}));
jest.mock('../utils/logging', () => ({
  logger: { debug: jest.fn(), info: jest.fn(), error: jest.fn() },
}));

const EXPORT_WALLETS_TIMEOUT_MS = 30000;
const request = exportWalletsChannel.request as unknown as jest.Mock;

function makeStore() {
  const api = {
    ada: { restoreExportedByronWallet: jest.fn() },
    localStorage: {
      getWalletMigrationStatus: jest.fn(),
      setWalletMigrationStatus: jest.fn(),
    },
  } as unknown as Api;
  const actions = jest.fn() as unknown as ActionsMap;
  const store = new WalletMigrationStore(api, actions, noopAnalyticsTracker);
  store.stores = {
    profile: { currentLocale: 'en-US' },
    wallets: { getWalletById: jest.fn() },
  } as any;
  return store;
}

describe('WalletMigrationStore', () => {
  beforeEach(() => {
    jest.useFakeTimers();
    request.mockReset();
    (logger.error as jest.Mock).mockReset();
  });

  afterEach(() => {
    jest.useRealTimers();
  });

  describe('_exportWallets', () => {
    it('ends with an export error when the request never resolves', async () => {
      request.mockReturnValue(new Promise(() => {}));
      const store = makeStore();

      const done = store._exportWallets();
      expect(store.isExportRunning).toBe(true);

      jest.advanceTimersByTime(EXPORT_WALLETS_TIMEOUT_MS + 1);
      await done;

      expect(store.isExportRunning).toBe(false);
      expect(store.exportErrors).not.toBe('');
      expect(store.exportedWallets).toEqual([]);
      expect(logger.error).toHaveBeenCalled();
    });

    it('ends with an export error when the request rejects', async () => {
      request.mockRejectedValue(new Error('main process failed'));
      const store = makeStore();

      await store._exportWallets();

      expect(store.isExportRunning).toBe(false);
      expect(store.exportErrors).not.toBe('');
      expect(store.exportedWallets).toEqual([]);
      expect(logger.error).toHaveBeenCalled();
      expect(jest.getTimerCount()).toBe(0);
    });

    it('stores the exported wallets and clears the timer when the request resolves', async () => {
      request.mockResolvedValue({
        wallets: [
          { id: 'abc', name: 'Savings', isEmptyPassphrase: false },
          { id: 'def', name: 'Spending', isEmptyPassphrase: true },
        ],
        errors: '',
      });
      const store = makeStore();

      await store._exportWallets();

      expect(store.isExportRunning).toBe(false);
      expect(store.exportErrors).toBe('');
      expect(store.exportedWallets.map((wallet) => wallet.id)).toEqual([
        'abc',
        'def',
      ]);
      expect(jest.getTimerCount()).toBe(0);
    });
  });
});
