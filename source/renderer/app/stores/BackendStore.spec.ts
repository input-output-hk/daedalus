import type { Api } from '../api/index';
import type { ActionsMap } from '../actions/index';
import BackendStore from './BackendStore';
import { noopAnalyticsTracker } from '../analytics';
import { mithrilCommandChannel } from '../ipc/mithrilCommandChannel';
import { validateChainStorageChannel } from '../ipc/chainStorageChannel';
import { getCachedBackendStatusChannel } from '../ipc/backendStatusChannel';

jest.mock('../utils/logging', () => ({
  logger: { debug: jest.fn(), info: jest.fn(), error: jest.fn() },
}));

jest.mock('../ipc/mithrilCommandChannel', () => ({
  mithrilCommandChannel: { send: jest.fn() },
}));

jest.mock('../ipc/backendStatusChannel', () => ({
  getCachedBackendStatusChannel: { request: jest.fn() },
}));

jest.mock('../ipc/chainStorageChannel', () => ({
  validateChainStorageChannel: { request: jest.fn() },
  confirmChainStorageChannel: { request: jest.fn() },
}));

// ── BackendStore unit tests ───────────────────────────────────────────────────
//
// These tests do NOT call initialize() so Electron IPC channels are never
// registered.  We exercise observable state and action handlers directly.

function makeStore() {
  const api = { ada: jest.fn(), localStorage: jest.fn() } as unknown as Api;
  const actions = jest.fn() as unknown as ActionsMap;
  return new BackendStore(api, actions, noopAnalyticsTracker);
}

// ── Bug 2 regression: isStopping observable ───────────────────────────────────
//
// Before the fix isStopping did not exist.  SyncingConnectingPage hardcoded
// isNodeStopping=false, so a clean shutdown showed "network connection lost"
// while the wallet was draining.
//
// After the fix BackendStore.isStopping is set to true when the watchdog emits
// `stopped`, and SyncingConnectingPage passes it as isNodeStopping.

describe('BackendStore.isStopping (Bug 2 fix)', () => {
  it('starts as false', () => {
    const store = makeStore();
    expect(store.isStopping).toBe(false);
  });

  it('becomes true after _onWatchdogStopped is called', async () => {
    const store = makeStore();
    // Cast to access the action handler directly without IPC plumbing.
    await (store as any)._onWatchdogStopped();
    expect(store.isStopping).toBe(true);
  });

  it('stays true after repeated calls', async () => {
    const store = makeStore();
    await (store as any)._onWatchdogStopped();
    await (store as any)._onWatchdogStopped();
    expect(store.isStopping).toBe(true);
  });
});

// ── loadingPhase computed ─────────────────────────────────────────────────────

describe('BackendStore.loadingPhase', () => {
  it('returns starting when hasChain is null', () => {
    const store = makeStore();
    expect(store.loadingPhase).toBe('starting');
  });

  it('returns node-starting when hasChain is true but walletPort is null', () => {
    const store = makeStore();
    (store as any).hasChain = true;
    expect(store.loadingPhase).toBe('node-starting');
  });

  it('returns ready when hasChain is true and walletPort is set', () => {
    const store = makeStore();
    (store as any).hasChain = true;
    (store as any).walletPort = 8090;
    expect(store.loadingPhase).toBe('ready');
  });

  it('returns error when walletUnrecoverable', () => {
    const store = makeStore();
    (store as any).hasChain = true;
    (store as any).walletPort = 8090;
    (store as any).walletUnrecoverable = true;
    expect(store.loadingPhase).toBe('error');
  });

  it('returns mithril-syncing when mithrilPhase is active', () => {
    const store = makeStore();
    (store as any).hasChain = true;
    (store as any).mithrilPhase = 'downloading';
    expect(store.loadingPhase).toBe('mithril-syncing');
  });

  it('returns node-starting when mithrilPhase is completed', () => {
    const store = makeStore();
    (store as any).hasChain = true;
    (store as any).mithrilPhase = 'completed';
    expect(store.loadingPhase).toBe('node-starting');
  });

  it('returns stopping while Daedalus quits, over any other phase', () => {
    const store = makeStore();
    (store as any).hasChain = true;
    (store as any).walletPort = 8090;
    (store as any).walletUnrecoverable = true;
    (store as any).mithrilPhase = 'downloading';
    (store as any).isStopping = true;
    expect(store.loadingPhase).toBe('stopping');
  });
});

const damage = {
  kind: 'ledger_point_missing' as const,
  slot: 200102375,
  lastGoodSlot: null,
  detectedAt: '2026-10-16T12:00:00.000Z',
};

describe('BackendStore chain damage', () => {
  beforeEach(() => {
    (mithrilCommandChannel.send as jest.Mock).mockClear();
  });

  it('returns chain-damaged when a hole was detected while the node starts', () => {
    const store = makeStore();
    (store as any).hasChain = true;
    (store as any).chainDamage = damage;
    expect(store.loadingPhase).toBe('chain-damaged');
  });

  it('returns chain-damaged over ready', () => {
    const store = makeStore();
    (store as any).hasChain = true;
    (store as any).walletPort = 8090;
    (store as any).chainDamage = damage;
    expect(store.loadingPhase).toBe('chain-damaged');
  });

  it('returns chain-damaged over an unrecoverable error', () => {
    const store = makeStore();
    (store as any).hasChain = true;
    (store as any).nodeUnrecoverable = true;
    (store as any).chainDamage = damage;
    expect(store.loadingPhase).toBe('chain-damaged');
  });

  it('returns stopping over chain-damaged while Daedalus quits', () => {
    const store = makeStore();
    (store as any).hasChain = true;
    (store as any).chainDamage = damage;
    (store as any).isStopping = true;
    expect(store.loadingPhase).toBe('stopping');
  });

  it.each(['preparing', 'downloading', 'installing', 'error'])(
    'returns mithril-syncing over chain-damaged while the Mithril phase is %s',
    (phase) => {
      const store = makeStore();
      (store as any).hasChain = true;
      (store as any).chainDamage = damage;
      (store as any).mithrilPhase = phase;
      expect(store.loadingPhase).toBe('mithril-syncing');
    }
  );

  it('returns chain-damaged once the Mithril phase is cancelled or completed', () => {
    const store = makeStore();
    (store as any).hasChain = true;
    (store as any).chainDamage = damage;
    (store as any).mithrilPhase = 'cancelled';
    expect(store.loadingPhase).toBe('chain-damaged');
    (store as any).mithrilPhase = 'completed';
    expect(store.loadingPhase).toBe('chain-damaged');
  });

  it('returns the previous phase after dismissChainDamage', () => {
    const store = makeStore();
    (store as any).hasChain = true;
    (store as any).walletPort = 8090;
    (store as any).chainDamage = damage;
    expect(store.loadingPhase).toBe('chain-damaged');
    store.dismissChainDamage();
    expect(store.loadingPhase).toBe('ready');
  });

  it('copies chainDamage from the poll', async () => {
    const store = makeStore();
    (getCachedBackendStatusChannel.request as jest.Mock).mockResolvedValue({
      hasChain: true,
      blockSyncProgress: {
        replayedBlock: 0,
        validatingChunk: 0,
        pushingLedger: 0,
      },
      chainDamage: damage,
    });
    await store._pollBackendStatus();
    expect(store.chainDamage).toEqual(damage);
  });

  it('sends start_mithril with wipe_chain and no force from rebuildChain', () => {
    const store = makeStore();
    store.rebuildChain();
    expect(mithrilCommandChannel.send).toHaveBeenCalledTimes(1);
    expect(mithrilCommandChannel.send).toHaveBeenCalledWith({
      cmd: 'start_mithril',
      wipe_chain: true,
    });
  });

  it('goes to the bootstrap decision, not the location picker, when a rebuild leaves no chain', () => {
    const store = makeStore();
    // A session that started with an existing chain never confirmed a location.
    (store as any).hasChain = true;
    store.rebuildChain();
    (store as any).hasChain = false;
    (store as any).mithrilPhase = 'cancelled';
    expect(store.loadingPhase).toBe('bootstrap-decision');
  });

  it('still shows the location picker when no chain exists and no rebuild was requested', () => {
    const store = makeStore();
    (store as any).hasChain = false;
    expect(store.loadingPhase).toBe('chain-storage-setup');
  });
});

describe('BackendStore unrecoverable backend', () => {
  beforeEach(() => {
    (mithrilCommandChannel.send as jest.Mock).mockClear();
  });

  it('returns error from loadingPhase when the node is unrecoverable', () => {
    const store = makeStore();
    (store as any).hasChain = true;
    (store as any).nodeUnrecoverable = true;
    expect(store.loadingPhase).toBe('error');
  });

  it('retries the wallet alone when only the wallet is unrecoverable', () => {
    const store = makeStore();
    (store as any).walletUnrecoverable = true;
    store.retryBackend();
    expect(mithrilCommandChannel.send).toHaveBeenCalledWith({
      cmd: 'restart_wallet',
    });
  });

  it('retries the node when the node is unrecoverable', () => {
    const store = makeStore();
    (store as any).walletUnrecoverable = true;
    (store as any).nodeUnrecoverable = true;
    store.retryBackend();
    expect(mithrilCommandChannel.send).toHaveBeenCalledWith({
      cmd: 'restart_node',
    });
  });
});

describe('BackendStore._onBackendStopStatus', () => {
  it('sets isStopping and records the stop progress', async () => {
    const store = makeStore();
    const progress = {
      stage: 'stopping_node',
      elapsedMs: 5000,
      timeoutMs: 300_000,
    };
    await (store as any)._onBackendStopStatus({
      quitting: true,
      restart: null,
      progress,
    });
    expect(store.isStopping).toBe(true);
    expect(store.backendStopProgress).toEqual(progress);
    expect(store.loadingPhase).toBe('stopping');
  });

  it('sets isStopping before the watchdog reports any progress', async () => {
    const store = makeStore();
    await (store as any)._onBackendStopStatus({
      quitting: true,
      restart: null,
      progress: null,
    });
    expect(store.isStopping).toBe(true);
    expect(store.backendStopProgress).toBeNull();
  });

  it('returns stopping for a requested restart without marking a quit', async () => {
    const store = makeStore();
    (store as any).hasChain = true;
    (store as any).walletPort = 8090;
    await (store as any)._onBackendStopStatus({
      quitting: false,
      restart: 'node',
      progress: null,
    });
    expect(store.isStopping).toBe(false);
    expect(store.loadingPhase).toBe('stopping');
  });

  it('returns to the startup phases once the requested restart has stopped the node', async () => {
    const store = makeStore();
    (store as any).hasChain = true;
    await (store as any)._onBackendStopStatus({
      quitting: false,
      restart: null,
      progress: null,
    });
    expect(store.loadingPhase).toBe('node-starting');
  });
});

describe('BackendStore._pollBackendStatus', () => {
  it('copies the code of the Mithril error from the polled state', async () => {
    const store = makeStore();
    (getCachedBackendStatusChannel.request as jest.Mock).mockResolvedValue({
      hasChain: false,
      blockSyncProgress: {
        replayedBlock: 0,
        validatingChunk: 0,
        pushingLedger: 0,
      },
      mithrilPhase: 'error',
      lastError: 'connection refused',
      lastErrorCode: 'PROBE_FAILED',
    });
    await store._pollBackendStatus();
    expect(store.lastError).toBe('connection refused');
    expect(store.lastErrorCode).toBe('PROBE_FAILED');
  });
});

describe('BackendStore.checkCustomChainPath', () => {
  const request = validateChainStorageChannel.request as jest.Mock;

  beforeEach(() => {
    request.mockReset();
  });

  it("records the configured folder when it holds another network's database", async () => {
    const store = makeStore();
    const validation = {
      isValid: false,
      path: '/mnt/cardano',
      reason: 'chain-subdirectory-other-network',
    };
    (store as any).customChainPath = '/mnt/cardano';
    request.mockResolvedValue(validation);

    await store.checkCustomChainPath();

    expect(request).toHaveBeenCalledWith({ path: '/mnt/cardano' });
    expect(store.customChainPathValidation).toEqual(validation);
  });

  it('leaves the configured folder as it was for any other result', async () => {
    const store = makeStore();
    (store as any).customChainPath = '/mnt/cardano';
    request.mockResolvedValue({
      isValid: false,
      path: '/mnt/cardano',
      reason: 'insufficient-space',
    });

    await store.checkCustomChainPath();

    expect(store.customChainPathValidation).toBeNull();
  });

  it('checks nothing when no folder is configured', async () => {
    const store = makeStore();

    await store.checkCustomChainPath();

    expect(request).not.toHaveBeenCalled();
    expect(store.customChainPathValidation).toBeNull();
  });
});
