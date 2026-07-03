import type {
  MithrilPartialSyncStatus,
  MithrilPartialSyncStatusSnapshot,
} from '../../common/types/mithril-partial-sync.types';
import { MithrilController } from './MithrilController';

const mockIsPartialSyncEnabled = jest.fn();
const mockIsPartialSyncInProgress = jest.fn();
const mockGetPartialSyncBehindness = jest.fn();
const mockForceKillForShutdown = jest.fn();
const mockOnChainDirectoryChanged = jest.fn();
const mockAdoptRecoverySnapshot = jest.fn();
const mockServiceWipeAndFullSync = jest.fn();
const mockServiceRestartNormal = jest.fn();

jest.mock('../utils/logging', () => ({
  logger: {
    warn: jest.fn(),
    info: jest.fn(),
  },
}));

jest.mock('../utils/chainStorageCoordinator', () => ({
  chainStorageCoordinator: {
    isPartialSyncEnabled: (...args) => mockIsPartialSyncEnabled(...args),
    isPartialSyncInProgress: (...args) => mockIsPartialSyncInProgress(...args),
    syncMithrilWorkDir: jest.fn().mockResolvedValue(undefined),
  },
  getMithrilBootstrapService: jest.fn(() => ({
    status: { status: 'idle', snapshot: null, error: null },
    onStatus: jest.fn(),
  })),
}));

jest.mock('./MithrilPartialSyncService', () => ({
  MithrilPartialSyncService: jest.fn().mockImplementation(() => ({
    status: {
      status: 'idle',
      allowedRecoveryActions: [],
      transferProgress: {},
      progressItems: [],
      error: null,
    },
    onStatus: jest.fn(),
    getPartialSyncBehindness: (...args) =>
      mockGetPartialSyncBehindness(...args),
    forceKillForShutdown: (...args) => mockForceKillForShutdown(...args),
    onChainDirectoryChanged: (...args) => mockOnChainDirectoryChanged(...args),
    adoptRecoverySnapshot: (...args) => mockAdoptRecoverySnapshot(...args),
    wipeAndFullSync: (...args) => mockServiceWipeAndFullSync(...args),
    restartNormal: (...args) => mockServiceRestartNormal(...args),
  })),
}));

// The controller constructor instantiates the startup gate, whose module pulls
// the electron import chain (mithrilPartialSyncNodeStartup → dialog); stub it
// so the controller can be constructed in isolation.
jest.mock('./MithrilStartupGate', () => ({
  MithrilStartupGate: jest.fn().mockImplementation(() => ({
    configure: jest.fn(),
    onBootstrapDecision: jest.fn(),
    onBootstrapStatus: jest.fn(),
    onPartialSyncStatus: jest.fn(),
    syncPendingDecision: jest.fn(),
    resetOnDirectoryChange: jest.fn(),
    state: 'idle',
  })),
}));

const createStatusSnapshot = (
  status: MithrilPartialSyncStatus
): MithrilPartialSyncStatusSnapshot => ({
  status,
  allowedRecoveryActions: [],
  transferProgress: {},
  progressItems: [],
  error: null,
});

// All nine partial-sync working statuses (isMithrilPartialSyncWorkingStatus).
const WORKING_STATUSES: MithrilPartialSyncStatus[] = [
  'stopping-node',
  'cancelling',
  'preparing',
  'downloading',
  'verifying',
  'converting',
  'installing',
  'finalizing',
  'starting-node',
];

describe('MithrilController', () => {
  const createController = () => new MithrilController();

  beforeEach(() => {
    jest.clearAllMocks();
    mockIsPartialSyncEnabled.mockReturnValue(true);
    mockIsPartialSyncInProgress.mockReturnValue(false);
    mockGetPartialSyncBehindness.mockResolvedValue({
      isSignificantlyBehind: true,
      behindByImmutables: 42,
      certifiedEpoch: 320,
    });
  });

  describe('getPartialSyncAvailability (main-side probe guard)', () => {
    it('returns not-behind without probing behind-ness while the status seam reports a working status', async () => {
      for (const status of WORKING_STATUSES) {
        const controller = createController();
        controller.setPartialSyncStatus(createStatusSnapshot(status));

        await expect(controller.getPartialSyncAvailability()).resolves.toEqual({
          isEnabled: true,
          isSignificantlyBehind: false,
        });
      }

      expect(mockGetPartialSyncBehindness).not.toHaveBeenCalled();
    });

    it('returns not-behind without probing behind-ness while the status seam reports terminal cancelled', async () => {
      const controller = createController();
      controller.setPartialSyncStatus(createStatusSnapshot('cancelled'));

      await expect(controller.getPartialSyncAvailability()).resolves.toEqual({
        isEnabled: true,
        isSignificantlyBehind: false,
      });
      expect(mockGetPartialSyncBehindness).not.toHaveBeenCalled();
    });

    it('still probes behind-ness and merges the figures for idle, failed, and completed', async () => {
      let expectedProbes = 0;
      for (const status of ['idle', 'failed', 'completed'] as const) {
        const controller = createController();
        controller.setPartialSyncStatus(createStatusSnapshot(status));

        await expect(controller.getPartialSyncAvailability()).resolves.toEqual({
          isEnabled: true,
          isSignificantlyBehind: true,
          behindByImmutables: 42,
          certifiedEpoch: 320,
        });

        expectedProbes += 1;
        expect(mockGetPartialSyncBehindness).toHaveBeenCalledTimes(
          expectedProbes
        );
      }
    });

    it('passes the probe-failed flag through to the availability payload', async () => {
      mockGetPartialSyncBehindness.mockResolvedValue({
        isSignificantlyBehind: false,
        isProbeFailed: true,
      });
      const controller = createController();
      controller.setPartialSyncStatus(createStatusSnapshot('idle'));

      await expect(controller.getPartialSyncAvailability()).resolves.toEqual({
        isEnabled: true,
        isSignificantlyBehind: false,
        isProbeFailed: true,
      });
    });

    it('short-circuits to disabled before the status guard or the behind-ness probe', async () => {
      mockIsPartialSyncEnabled.mockReturnValue(false);
      const controller = createController();
      controller.setPartialSyncStatus(createStatusSnapshot('idle'));

      await expect(controller.getPartialSyncAvailability()).resolves.toEqual({
        isEnabled: false,
        isSignificantlyBehind: false,
      });
      expect(mockGetPartialSyncBehindness).not.toHaveBeenCalled();
    });
  });

  describe('resetStartupGateOnDirectoryChange', () => {
    it('drops the behind-ness caches together with the startup-gate reset', () => {
      const controller = createController();

      controller.resetStartupGateOnDirectoryChange();

      expect(mockOnChainDirectoryChanged).toHaveBeenCalledTimes(1);
      expect(
        (controller as any)._startupGate.resetOnDirectoryChange
      ).toHaveBeenCalledTimes(1);
    });
  });

  describe('reapPartialSyncOnShutdown (shutdown reap)', () => {
    // The partial-sync service is class-mocked in this spec, so the observable
    // here is the delegation to forceKillForShutdown(); the sync-mode SIGKILL
    // it issues — killProcessTree(child, 'SIGKILL', { sync: true }) — is
    // pinned in MithrilPartialSyncService.spec.ts.
    it('delegates to the service sync force-kill and logs while partial sync is active', () => {
      const controller = createController();
      controller.setPartialSyncStatus(createStatusSnapshot('downloading'));

      controller.reapPartialSyncOnShutdown();

      expect(mockForceKillForShutdown).toHaveBeenCalledTimes(1);
      const { info: infoLog } = require('../utils/logging').logger;
      expect(infoLog).toHaveBeenCalledWith(
        'MithrilController: reaping active partial sync process on shutdown',
        expect.objectContaining({ status: 'downloading' })
      );
    });

    it('reaps when the coordinator reports work in progress even though the status seam is idle', () => {
      mockIsPartialSyncInProgress.mockReturnValue(true);
      const controller = createController();
      controller.setPartialSyncStatus(createStatusSnapshot('idle'));

      controller.reapPartialSyncOnShutdown();

      expect(mockForceKillForShutdown).toHaveBeenCalledTimes(1);
    });

    it('is a no-op when partial sync is inactive: the service force-kill is never called', () => {
      const controller = createController();
      controller.setPartialSyncStatus(createStatusSnapshot('idle'));

      controller.reapPartialSyncOnShutdown();

      expect(mockForceKillForShutdown).not.toHaveBeenCalled();
      const { info: infoLog, warn: warnLog } =
        require('../utils/logging').logger;
      expect(infoLog).not.toHaveBeenCalled();
      expect(warnLog).not.toHaveBeenCalled();
    });

    it('swallows a throwing force-kill so it can never rethrow into safeExit', () => {
      const killError = new Error('taskkill exploded');
      mockForceKillForShutdown.mockImplementationOnce(() => {
        throw killError;
      });
      const controller = createController();
      controller.setPartialSyncStatus(createStatusSnapshot('cancelling'));

      expect(() => controller.reapPartialSyncOnShutdown()).not.toThrow();

      const { warn: warnLog } = require('../utils/logging').logger;
      expect(warnLog).toHaveBeenCalledWith(
        'MithrilController: failed to reap partial sync process on shutdown',
        expect.objectContaining({ error: killError })
      );
    });
  });

  describe('recovery-action wiring (cross-session snapshot adoption)', () => {
    it('seeds the service with the controller-held snapshot before delegating wipe-and-full-sync', async () => {
      const controller = createController();
      const failedSnapshot: MithrilPartialSyncStatusSnapshot = {
        status: 'failed',
        allowedRecoveryActions: ['wipe-and-full-sync'],
        transferProgress: {},
        progressItems: [],
        error: {
          message: 'node failed to start',
          stage: 'starting-node',
        },
      };
      controller.setPartialSyncStatus(failedSnapshot);

      await controller._getPartialSyncDependencies().handlers.wipeAndFullSync();

      expect(mockAdoptRecoverySnapshot).toHaveBeenCalledWith(failedSnapshot);
      expect(mockServiceWipeAndFullSync).toHaveBeenCalledTimes(1);
      expect(
        mockAdoptRecoverySnapshot.mock.invocationCallOrder[0]
      ).toBeLessThan(mockServiceWipeAndFullSync.mock.invocationCallOrder[0]);
    });

    it('seeds the service with the controller-held snapshot before delegating restart-normal', async () => {
      const controller = createController();
      const failedSnapshot: MithrilPartialSyncStatusSnapshot = {
        status: 'failed',
        allowedRecoveryActions: ['retry', 'restart-normal'],
        transferProgress: {},
        progressItems: [],
        error: {
          message: 'download failed',
          stage: 'downloading',
        },
      };
      controller.setPartialSyncStatus(failedSnapshot);

      await controller._getPartialSyncDependencies().handlers.restartNormal();

      expect(mockAdoptRecoverySnapshot).toHaveBeenCalledWith(failedSnapshot);
      expect(mockServiceRestartNormal).toHaveBeenCalledTimes(1);
      expect(
        mockAdoptRecoverySnapshot.mock.invocationCallOrder[0]
      ).toBeLessThan(mockServiceRestartNormal.mock.invocationCallOrder[0]);
    });
  });
});
