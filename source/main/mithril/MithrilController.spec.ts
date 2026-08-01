import type { MithrilPartialSyncStatusSnapshot } from '../../common/types/mithril-partial-sync.types';
import { makeIdlePartialSyncStatus } from '../../common/types/mithril-partial-sync.types';
import { MithrilController } from './MithrilController';

jest.mock('../utils/logging', () => ({
  logger: {
    warn: jest.fn(),
    info: jest.fn(),
  },
}));

const createStatusSnapshot = (
  status: MithrilPartialSyncStatusSnapshot['status']
): MithrilPartialSyncStatusSnapshot => ({
  status,
  allowedRecoveryActions: [],
  transferProgress: {},
  progressItems: [],
  error: null,
});

describe('MithrilController', () => {
  const createController = () => new MithrilController();

  beforeEach(() => {
    jest.clearAllMocks();
  });

  describe('reapPartialSyncOnShutdown (shutdown reap)', () => {
    it('calls cancelMithril on the watchdog handle when one is present', () => {
      const controller = createController();
      const cancelMithril = jest.fn();
      controller.setWatchdogHandle({
        cancelMithril,
        startMithril: jest.fn(),
      } as any);

      controller.reapPartialSyncOnShutdown();

      expect(cancelMithril).toHaveBeenCalledTimes(1);
    });

    it('is a no-op when no watchdog handle is set', () => {
      const controller = createController();

      expect(() => controller.reapPartialSyncOnShutdown()).not.toThrow();
    });

    it('swallows a throwing cancelMithril so it can never rethrow into safeExit', () => {
      const controller = createController();
      const killError = new Error('cancel exploded');
      controller.setWatchdogHandle({
        cancelMithril: () => {
          throw killError;
        },
        startMithril: jest.fn(),
      } as any);

      expect(() => controller.reapPartialSyncOnShutdown()).not.toThrow();

      const { warn: warnLog } = require('../utils/logging').logger;
      expect(warnLog).toHaveBeenCalledWith(
        'MithrilController: failed to reap partial sync process on shutdown',
        expect.objectContaining({ error: killError })
      );
    });
  });

  describe('broadcastPartialSyncStatus', () => {
    it('updates internal status and calls the sender', async () => {
      const controller = createController();
      const sender = jest.fn().mockResolvedValue(undefined);
      controller.setPartialSyncStatusSender(sender);

      const status = createStatusSnapshot('downloading');
      await controller.broadcastPartialSyncStatus(status);

      expect(controller.getPartialSyncStatus()).toEqual(status);
      expect(sender).toHaveBeenCalledWith(status);
    });
  });

  describe('onWatchdogMithrilStatus', () => {
    it('broadcasts merged status with the new phase', async () => {
      const controller = createController();
      const sender = jest.fn().mockResolvedValue(undefined);
      controller.setPartialSyncStatusSender(sender);

      controller.onWatchdogMithrilStatus('downloading');

      // Allow microtask queue to flush
      await Promise.resolve();

      expect(controller.getPartialSyncStatus().status).toBe('downloading');
    });
  });

  describe('onWatchdogMithrilNotNeeded', () => {
    it('resets status to idle', async () => {
      const controller = createController();
      const sender = jest.fn().mockResolvedValue(undefined);
      controller.setPartialSyncStatusSender(sender);

      controller.onWatchdogMithrilStatus('downloading');
      await Promise.resolve();

      controller.onWatchdogMithrilNotNeeded();
      await Promise.resolve();

      expect(controller.getPartialSyncStatus().status).toBe('idle');
    });
  });

  describe('startPartialSync', () => {
    it('delegates to watchdog handle when present', async () => {
      const controller = createController();
      const startMithril = jest.fn();
      controller.setWatchdogHandle({
        startMithril,
        cancelMithril: jest.fn(),
      } as any);

      await controller.startPartialSync();

      expect(startMithril).toHaveBeenCalledWith({
        force: true,
        wipeChain: false,
      });
    });
  });

  describe('cancelPartialSync', () => {
    it('delegates to watchdog handle when present', async () => {
      const controller = createController();
      const cancelMithril = jest.fn();
      controller.setWatchdogHandle({
        cancelMithril,
        startMithril: jest.fn(),
      } as any);

      await controller.cancelPartialSync();

      expect(cancelMithril).toHaveBeenCalledTimes(1);
    });
  });
});
