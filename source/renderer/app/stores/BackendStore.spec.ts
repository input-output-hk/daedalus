import { serialize } from 'v8';
import type { Api } from '../api/index';
import type { ActionsMap } from '../actions/index';
import BackendStore from './BackendStore';
import { noopAnalyticsTracker } from '../analytics';
import { logger } from '../utils/logging';
import { mithrilCommandChannel } from '../ipc/mithrilCommandChannel';
import {
  confirmChainStorageChannel,
  validateChainStorageChannel,
} from '../ipc/chainStorageChannel';
import { getCachedBackendStatusChannel } from '../ipc/backendStatusChannel';

// The real logger sends its arguments through ipcRenderer, which clones them
// and throws on a MobX observable. The mock serializes its arguments too.
jest.mock('../utils/logging', () => {
  const cloning = () =>
    jest.fn((...args: Array<unknown>) => {
      serialize(args);
    });
  return {
    logger: {
      debug: cloning(),
      info: cloning(),
      error: cloning(),
      warn: cloning(),
    },
  };
});

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

  it.each(['preparing', 'downloading', 'installing'])(
    'returns mithril-syncing over chain-damaged while the Mithril phase is %s',
    (phase) => {
      const store = makeStore();
      (store as any).hasChain = true;
      (store as any).chainDamage = damage;
      (store as any).mithrilPhase = phase;
      expect(store.loadingPhase).toBe('mithril-syncing');
    }
  );

  it('keeps the error view of a rebuild over chain-damaged', () => {
    const store = makeStore();
    (store as any).hasChain = true;
    (store as any).chainDamage = damage;
    (store as any).mithrilPhase = 'error';
    (store as any).lastErrorCode = 'INVALID_CHAIN_PATH';
    (store as any).lastErrorRebuild = true;
    expect(store.loadingPhase).toBe('mithril-syncing');
  });

  it('offers the rebuild over the error of a partial sync that failed before the damage was found', async () => {
    const store = makeStore();
    const polled = {
      hasChain: true,
      walletPort: null,
      blockSyncProgress: {
        replayedBlock: 0,
        validatingChunk: 0,
        pushingLedger: 0,
      },
      mithrilPhase: 'error',
      lastErrorCode: 'PARTIAL_SYNC_DOWNLOAD_COMMAND_FAILED',
      lastErrorRebuild: false,
      chainDamage: null,
    };
    (getCachedBackendStatusChannel.request as jest.Mock).mockResolvedValue(
      polled
    );
    await store._pollBackendStatus();
    expect(store.loadingPhase).toBe('mithril-syncing');
    // The watchdog started the node again on the chain, and its output shows
    // the damage while the error view is still up.
    (getCachedBackendStatusChannel.request as jest.Mock).mockResolvedValue({
      ...polled,
      chainDamage: { ...damage, kind: 'node_rejects_canonical_block' },
    });
    await store._pollBackendStatus();
    expect(store.loadingPhase).toBe('chain-damaged');
    // Not now returns to the error the damage screen replaced.
    store.dismissChainDamage();
    expect(store.loadingPhase).toBe('mithril-syncing');
  });

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

  it('keeps lastChainDamage from the poll after chainDamage is cleared', async () => {
    const store = makeStore();
    const state = {
      hasChain: true,
      blockSyncProgress: {
        replayedBlock: 0,
        validatingChunk: 0,
        pushingLedger: 0,
      },
      chainDamage: damage,
      lastChainDamage: damage,
    };
    (getCachedBackendStatusChannel.request as jest.Mock).mockResolvedValue(
      state
    );
    await store._pollBackendStatus();
    (getCachedBackendStatusChannel.request as jest.Mock).mockResolvedValue({
      ...state,
      chainDamage: null,
    });
    await store._pollBackendStatus();
    expect(store.chainDamage).toBeNull();
    expect(store.lastChainDamage).toEqual(damage);
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

  it('sends the rebuild and logs plain values while a chain damage is set', () => {
    const store = makeStore();
    (store as any).chainDamage = damage;
    // Assigned through the observable, so the stored value is a MobX proxy.
    expect(() => store.rebuildChain()).not.toThrow();
    expect(mithrilCommandChannel.send).toHaveBeenCalledWith({
      cmd: 'start_mithril',
      wipe_chain: true,
    });
    expect(logger.info).toHaveBeenCalledWith(
      'BackendStore: rebuilding the chain database',
      { kind: 'ledger_point_missing', slot: 200102375 }
    );
  });

  it('goes to the bootstrap decision, not the location picker, when a rebuild leaves no chain', async () => {
    const store = makeStore();
    // A session that started with an existing chain never confirmed a location.
    (store as any).hasChain = true;
    store.rebuildChain();
    (store as any).hasChain = false;
    await (store as any)._onMithrilStatus({ phase: 'cancelled' });
    expect(store.loadingPhase).toBe('bootstrap-decision');
  });

  it('still shows the location picker when no chain exists and no rebuild was requested', () => {
    const store = makeStore();
    (store as any).hasChain = false;
    expect(store.loadingPhase).toBe('chain-storage-setup');
  });
});

describe('BackendStore Not now on the damaged data screen', () => {
  beforeEach(() => {
    (mithrilCommandChannel.send as jest.Mock).mockClear();
  });

  const polled = (overrides: Record<string, unknown> = {}) => ({
    hasChain: true,
    walletPort: 8090,
    blockSyncProgress: {
      replayedBlock: 0,
      validatingChunk: 0,
      pushingLedger: 0,
    },
    mithrilPhase: null,
    chainDamage: damage,
    lastChainDamage: damage,
    ...overrides,
  });

  it('has main record the dismissal', () => {
    const store = makeStore();
    store.dismissChainDamage();
    expect(mithrilCommandChannel.send).toHaveBeenCalledWith({
      cmd: 'dismiss_chain_damage',
    });
  });

  it('keeps the screen hidden after a reload of the window', async () => {
    // A new store stands for the renderer after a reload; main still holds
    // the damage and the dismissal.
    const store = makeStore();
    (getCachedBackendStatusChannel.request as jest.Mock).mockResolvedValue(
      polled({ chainDamageDismissed: true })
    );
    await store._pollBackendStatus();
    expect(store.loadingPhase).toBe('ready');
  });

  it('keeps a dismissal in this window until main reports it', async () => {
    const store = makeStore();
    store.dismissChainDamage();
    (getCachedBackendStatusChannel.request as jest.Mock).mockResolvedValue(
      polled({ chainDamageDismissed: undefined })
    );
    await store._pollBackendStatus();
    expect(store.loadingPhase).toBe('ready');
  });

  it('shows the screen after a reload while the watchdog holds the node', async () => {
    const store = makeStore();
    (getCachedBackendStatusChannel.request as jest.Mock).mockResolvedValue(
      polled({ chainDamageDismissed: true, nodeHeld: true, walletPort: null })
    );
    await store._pollBackendStatus();
    expect(store.loadingPhase).toBe('chain-damaged');
  });
});

describe('BackendStore chain damage found before the node started', () => {
  it('returns chain-damaged after dismissChainDamage while the node is held', () => {
    const store = makeStore();
    (store as any).hasChain = true;
    (store as any).chainDamage = damage;
    store.dismissChainDamage();
    expect(store.loadingPhase).toBe('node-starting');
    (store as any).nodeHeld = true;
    expect(store.loadingPhase).toBe('chain-damaged');
  });

  it('returns mithril-syncing over chain-damaged while a rebuild runs for a held node', () => {
    const store = makeStore();
    (store as any).hasChain = true;
    (store as any).chainDamage = damage;
    (store as any).nodeHeld = true;
    (store as any).mithrilPhase = 'downloading';
    expect(store.loadingPhase).toBe('mithril-syncing');
  });

  it('copies nodeHeld from the poll and defaults it to false', async () => {
    const store = makeStore();
    const state = {
      hasChain: true,
      blockSyncProgress: {
        replayedBlock: 0,
        validatingChunk: 0,
        pushingLedger: 0,
      },
      chainDamage: damage,
      nodeHeld: true,
    };
    (getCachedBackendStatusChannel.request as jest.Mock).mockResolvedValue(
      state
    );
    await store._pollBackendStatus();
    expect(store.nodeHeld).toBe(true);
    (getCachedBackendStatusChannel.request as jest.Mock).mockResolvedValue({
      ...state,
      nodeHeld: undefined,
    });
    await store._pollBackendStatus();
    expect(store.nodeHeld).toBe(false);
  });
});

describe('BackendStore startup silence', () => {
  beforeEach(() => {
    (mithrilCommandChannel.send as jest.Mock).mockClear();
  });

  const silentStore = () => {
    const store = makeStore();
    (store as any).hasChain = true;
    (store as any).nodeSilentSecs = 300;
    (store as any).nodeSilenceEpisode = 1;
    return store;
  };

  it('shows the silence prompt while the node starts and is silent', () => {
    const store = silentStore();
    expect(store.loadingPhase).toBe('node-starting');
    expect(store.showNodeSilencePrompt).toBe(true);
  });

  it('shows no silence prompt without a silence or outside the node start', () => {
    const store = silentStore();
    (store as any).nodeSilentSecs = null;
    expect(store.showNodeSilencePrompt).toBe(false);
    (store as any).nodeSilentSecs = 300;
    (store as any).walletPort = 8090;
    expect(store.showNodeSilencePrompt).toBe(false);
    (store as any).walletPort = null;
    (store as any).isStopping = true;
    expect(store.showNodeSilencePrompt).toBe(false);
  });

  it('hides the prompt after keepWaitingForNode until the next silence', () => {
    const store = silentStore();
    store.keepWaitingForNode();
    expect(store.showNodeSilencePrompt).toBe(false);
    (store as any).nodeSilenceEpisode = 2;
    expect(store.showNodeSilencePrompt).toBe(true);
    expect((mithrilCommandChannel.send as jest.Mock).mock.calls).toEqual([
      [{ cmd: 'dismiss_node_silence', episode: 1 }],
    ]);
  });

  it('sends restart_node from restartSilentNode and hides the prompt', () => {
    const store = silentStore();
    store.restartSilentNode();
    expect((mithrilCommandChannel.send as jest.Mock).mock.calls).toEqual([
      [{ cmd: 'dismiss_node_silence', episode: 1 }],
      [{ cmd: 'restart_node' }],
    ]);
    expect(store.showNodeSilencePrompt).toBe(false);
  });

  const polledSilence = (dismissedSilenceEpisode: number | undefined) => ({
    hasChain: true,
    mithrilPhase: null,
    walletPort: null,
    blockSyncProgress: {
      replayedBlock: 0,
      validatingChunk: 0,
      pushingLedger: 0,
    },
    nodeSilentSecs: 300,
    nodeSilenceEpisode: 1,
    dismissedSilenceEpisode,
  });

  it('keeps a prompt dismissed before a reload of the window hidden', async () => {
    // A new store stands for the renderer after a reload; main still holds
    // the silence and the dismissal.
    const store = makeStore();
    (getCachedBackendStatusChannel.request as jest.Mock).mockResolvedValue(
      polledSilence(1)
    );
    await store._pollBackendStatus();
    expect(store.loadingPhase).toBe('node-starting');
    expect(store.showNodeSilencePrompt).toBe(false);
    (getCachedBackendStatusChannel.request as jest.Mock).mockResolvedValue({
      ...polledSilence(1),
      nodeSilenceEpisode: 2,
    });
    await store._pollBackendStatus();
    expect(store.showNodeSilencePrompt).toBe(true);
  });

  it('keeps a prompt dismissed in this window hidden until main reports the dismissal', async () => {
    const store = silentStore();
    store.keepWaitingForNode();
    (getCachedBackendStatusChannel.request as jest.Mock).mockResolvedValue(
      polledSilence(undefined)
    );
    await store._pollBackendStatus();
    expect(store.showNodeSilencePrompt).toBe(false);
  });

  it('copies the silence from the poll', async () => {
    const store = makeStore();
    (getCachedBackendStatusChannel.request as jest.Mock).mockResolvedValue({
      hasChain: true,
      blockSyncProgress: {
        replayedBlock: 0,
        validatingChunk: 0,
        pushingLedger: 0,
      },
      nodeSilentSecs: 300,
      nodeSilenceEpisode: 3,
    });
    await store._pollBackendStatus();
    expect(store.nodeSilentSecs).toBe(300);
    expect(store.nodeSilenceEpisode).toBe(3);
  });
});

describe('BackendStore Mithril download waiting for the network', () => {
  it('copies mithrilWaitingForNetwork from the poll and defaults it to false', async () => {
    const store = makeStore();
    const state = {
      hasChain: false,
      blockSyncProgress: {
        replayedBlock: 0,
        validatingChunk: 0,
        pushingLedger: 0,
      },
      mithrilPhase: 'downloading',
      mithrilWaitingForNetwork: true,
    };
    (getCachedBackendStatusChannel.request as jest.Mock).mockResolvedValue(
      state
    );
    await store._pollBackendStatus();
    expect(store.mithrilWaitingForNetwork).toBe(true);
    (getCachedBackendStatusChannel.request as jest.Mock).mockResolvedValue({
      ...state,
      mithrilWaitingForNetwork: undefined,
    });
    await store._pollBackendStatus();
    expect(store.mithrilWaitingForNetwork).toBe(false);
  });
});

describe('BackendStore.retryMithril', () => {
  beforeEach(() => {
    (mithrilCommandChannel.send as jest.Mock).mockClear();
    (confirmChainStorageChannel.request as jest.Mock)
      .mockReset()
      .mockResolvedValue(undefined);
  });

  const failed = (
    store: BackendStore,
    code: string | null,
    rebuild: boolean
  ) => {
    (store as any).hasChain = false;
    (store as any).mithrilPhase = 'error';
    (store as any).lastErrorCode = code;
    (store as any).lastErrorRebuild = rebuild;
  };

  it('runs a plain Mithril sync when the error did not come from a rebuild', () => {
    const store = makeStore();
    failed(store, 'PARTIAL_SYNC_DOWNLOAD_COMMAND_FAILED', false);
    store.retryMithril();
    expect(mithrilCommandChannel.send).toHaveBeenCalledWith({
      cmd: 'start_mithril',
    });
  });

  it.each([
    'PROBE_FAILED',
    'WIPE_FAILED',
    'INSTALL_FAILED',
    'INSUFFICIENT_SPACE',
  ])('repeats the rebuild after a rebuild failed with %s', (code) => {
    const store = makeStore();
    failed(store, code, true);
    store.retryMithril();
    expect(mithrilCommandChannel.send).toHaveBeenCalledTimes(1);
    expect(mithrilCommandChannel.send).toHaveBeenCalledWith({
      cmd: 'start_mithril',
      wipe_chain: true,
    });
    expect(store.loadingPhase).toBe('mithril-syncing');
  });

  it.each(['INVALID_CHAIN_PATH', 'CHAIN_DIR_OTHER_NETWORK'])(
    'asks for a storage location after a rebuild failed with %s',
    (code) => {
      const store = makeStore();
      failed(store, code, true);
      store.retryMithril();
      expect(mithrilCommandChannel.send).not.toHaveBeenCalled();
      expect(store.loadingPhase).toBe('chain-storage-setup');
    }
  );

  it('shows the picker even when the node database still exists', () => {
    const store = makeStore();
    failed(store, 'INVALID_CHAIN_PATH', true);
    (store as any).hasChain = true;
    store.retryMithril();
    expect(store.loadingPhase).toBe('chain-storage-setup');
  });

  it('asks to confirm the rebuild in a location picked after a location error, and sends both together', async () => {
    const store = makeStore();
    failed(store, 'CHAIN_DIR_OTHER_NETWORK', true);
    store.retryMithril();
    await store.setChainStorageDirectory('/new');
    expect(confirmChainStorageChannel.request).not.toHaveBeenCalled();
    store.confirmStorageLocation();
    expect(store.loadingPhase).toBe('mithril-syncing');
    expect(store._rebuildConfirmationRequested).toBe(true);
    expect(store.pendingChainLocation).toBe('/new');
    store.rebuildChain();
    expect(confirmChainStorageChannel.request).toHaveBeenCalledWith({
      customPath: '/new',
      rebuild: true,
    });
    expect(mithrilCommandChannel.send).not.toHaveBeenCalled();
    expect(store.pendingChainLocation).toBeNull();
  });

  it('asks for a location again when Retry follows Back from that confirmation', async () => {
    const store = makeStore();
    failed(store, 'INVALID_CHAIN_PATH', true);
    store.retryMithril();
    await store.setChainStorageDirectory('/new');
    store.confirmStorageLocation();
    store.cancelRebuildConfirmation();
    expect(store.pendingChainLocation).toBeNull();
    store.retryMithril();
    expect(store.loadingPhase).toBe('chain-storage-setup');
    expect(confirmChainStorageChannel.request).not.toHaveBeenCalled();
    expect(mithrilCommandChannel.send).not.toHaveBeenCalled();
  });

  it('asks to confirm a rebuild when a partial sync failed to install', () => {
    const store = makeStore();
    failed(store, 'INSTALL_FAILED', false);
    store.retryMithril();
    expect(mithrilCommandChannel.send).not.toHaveBeenCalled();
    expect(store._rebuildConfirmationRequested).toBe(true);
  });

  it('leaves the confirmation from Back without sending anything', () => {
    const store = makeStore();
    failed(store, 'INSTALL_FAILED', false);
    store.retryMithril();
    store.cancelRebuildConfirmation();
    expect(store._rebuildConfirmationRequested).toBe(false);
    expect(mithrilCommandChannel.send).not.toHaveBeenCalled();
  });

  it('sends the rebuild only when the confirmation is confirmed', () => {
    const store = makeStore();
    failed(store, 'INSTALL_FAILED', false);
    store.retryMithril();
    store.rebuildChain();
    expect(mithrilCommandChannel.send).toHaveBeenCalledTimes(1);
    expect(mithrilCommandChannel.send).toHaveBeenCalledWith({
      cmd: 'start_mithril',
      wipe_chain: true,
    });
    expect(store._rebuildConfirmationRequested).toBe(false);
  });

  it('drops the confirmation when the Mithril phase leaves the error', async () => {
    const store = makeStore();
    failed(store, 'INSTALL_FAILED', false);
    store.retryMithril();
    (getCachedBackendStatusChannel.request as jest.Mock).mockResolvedValue({
      hasChain: false,
      blockSyncProgress: {
        replayedBlock: 0,
        validatingChunk: 0,
        pushingLedger: 0,
      },
      mithrilPhase: 'preparing',
    });
    await store._pollBackendStatus();
    expect(store._rebuildConfirmationRequested).toBe(false);
  });

  it('copies lastErrorRebuild from the poll', async () => {
    const store = makeStore();
    (getCachedBackendStatusChannel.request as jest.Mock).mockResolvedValue({
      hasChain: false,
      blockSyncProgress: {
        replayedBlock: 0,
        validatingChunk: 0,
        pushingLedger: 0,
      },
      mithrilPhase: 'error',
      lastErrorRebuild: true,
    });
    await store._pollBackendStatus();
    expect(store.lastErrorRebuild).toBe(true);
  });
});

describe('BackendStore rebuild starting', () => {
  beforeEach(() => {
    (mithrilCommandChannel.send as jest.Mock).mockClear();
    (confirmChainStorageChannel.request as jest.Mock)
      .mockReset()
      .mockResolvedValue(undefined);
  });

  // The state main reports after a partial sync failed to install, the
  // first Mithril error of the session
  const installFailed = (overrides: Record<string, unknown> = {}) => ({
    hasChain: true,
    blockSyncProgress: {
      replayedBlock: 0,
      validatingChunk: 0,
      pushingLedger: 0,
    },
    mithrilPhase: 'error',
    lastError: 'install failed',
    lastErrorCode: 'INSTALL_FAILED',
    lastErrorRebuild: false,
    installRetryRequired: true,
    installPending: true,
    mithrilErrorCount: 1,
    ...overrides,
  });

  const confirmedRebuild = async () => {
    const store = makeStore();
    (getCachedBackendStatusChannel.request as jest.Mock).mockResolvedValue(
      installFailed()
    );
    await store._pollBackendStatus();
    store.retryMithril();
    store.rebuildChain();
    return store;
  };

  it('shows the rebuild preparing, not the error it replaces, until the watchdog reports it', async () => {
    const store = await confirmedRebuild();
    expect(store.rebuildStarting).toBe(true);
    expect(store.loadingPhase).toBe('mithril-syncing');
    // Main has cleared the rebuild flag of the old error and still reports it.
    (getCachedBackendStatusChannel.request as jest.Mock).mockResolvedValue(
      installFailed({ lastErrorRebuild: false })
    );
    await store._pollBackendStatus();
    expect(store.rebuildStarting).toBe(true);
    await (store as any)._onMithrilStatus({ phase: 'preparing' });
    expect(store.rebuildStarting).toBe(false);
    expect(store.loadingPhase).toBe('mithril-syncing');
  });

  it('ends when a poll reports a phase of the run', async () => {
    const store = await confirmedRebuild();
    (getCachedBackendStatusChannel.request as jest.Mock).mockResolvedValue(
      installFailed({ mithrilPhase: 'downloading' })
    );
    await store._pollBackendStatus();
    expect(store.rebuildStarting).toBe(false);
  });

  it('ends with the error the rebuild was refused with', async () => {
    const store = await confirmedRebuild();
    (getCachedBackendStatusChannel.request as jest.Mock).mockResolvedValue(
      installFailed({
        lastErrorCode: 'REBUILD_MARKER_FAILED',
        lastErrorRebuild: true,
        mithrilErrorCount: 2,
      })
    );
    await store._pollBackendStatus();
    expect(store.rebuildStarting).toBe(false);
    expect(store.loadingPhase).toBe('mithril-syncing');
    expect(store.lastErrorCode).toBe('REBUILD_MARKER_FAILED');
  });

  it('ends when the watchdog starts the node in place of the rebuild, as on a network without Mithril', async () => {
    const store = makeStore();
    const polled = {
      hasChain: true,
      walletPort: 8090,
      nodeStartedAt: 1000,
      blockSyncProgress: {
        replayedBlock: 0,
        validatingChunk: 0,
        pushingLedger: 0,
      },
      mithrilPhase: null,
    };
    (getCachedBackendStatusChannel.request as jest.Mock).mockResolvedValue(
      polled
    );
    await store._pollBackendStatus();
    store.rebuildChain();
    await store._pollBackendStatus();
    expect(store.rebuildStarting).toBe(true);
    (getCachedBackendStatusChannel.request as jest.Mock).mockResolvedValue({
      ...polled,
      walletPort: null,
      nodeStartedAt: 2000,
    });
    await store._pollBackendStatus();
    expect(store.rebuildStarting).toBe(false);
    expect(store.loadingPhase).toBe('node-starting');
  });

  it('ends when the location and the rebuild could not be sent', async () => {
    (confirmChainStorageChannel.request as jest.Mock).mockRejectedValue(
      new Error('no main process')
    );
    const store = makeStore();
    (store as any).hasChain = true;
    (store as any).mithrilPhase = 'error';
    (store as any).lastErrorCode = 'INVALID_CHAIN_PATH';
    (store as any).lastErrorRebuild = true;
    store.retryMithril();
    await store.setChainStorageDirectory('/new');
    store.confirmStorageLocation();
    store.rebuildChain();
    expect(store.rebuildStarting).toBe(true);
    await new Promise((resolve) => setTimeout(resolve, 0));
    expect(store.rebuildStarting).toBe(false);
  });

  it('shows the rebuild preparing at once when it is confirmed on the damaged data screen', () => {
    const store = makeStore();
    (store as any).hasChain = true;
    (store as any).walletPort = 8090;
    (store as any).chainDamage = damage;
    expect(store.loadingPhase).toBe('chain-damaged');
    store.rebuildChain();
    expect(store.loadingPhase).toBe('mithril-syncing');
  });
});

describe('BackendStore choosing another location after a failed sync', () => {
  beforeEach(() => {
    (mithrilCommandChannel.send as jest.Mock).mockClear();
    (confirmChainStorageChannel.request as jest.Mock)
      .mockReset()
      .mockResolvedValue(undefined);
  });

  const failed = (store: BackendStore, code: string, rebuild: boolean) => {
    (store as any).hasChain = true;
    (store as any).mithrilPhase = 'error';
    (store as any).lastErrorCode = code;
    (store as any).lastErrorRebuild = rebuild;
    (store as any).defaultChainPath = '/state/chain';
  };

  it.each(['INSUFFICIENT_SPACE', 'CHAIN_DIR_NOT_REPLACEABLE'])(
    'offers another location after %s',
    (code) => {
      const store = makeStore();
      failed(store, code, true);
      expect(store.canChooseAnotherLocation).toBe(true);
    }
  );

  it.each(['PROBE_FAILED', 'INSTALL_FAILED', 'INVALID_CHAIN_PATH'])(
    'offers no separate location choice after %s',
    (code) => {
      const store = makeStore();
      failed(store, code, true);
      expect(store.canChooseAnotherLocation).toBe(false);
    }
  );

  it('shows the picker when another location is chosen', () => {
    const store = makeStore();
    failed(store, 'INSUFFICIENT_SPACE', true);
    store.chooseAnotherLocation();
    expect(store.loadingPhase).toBe('chain-storage-setup');
  });

  it('marks the picker as asked for from the error view until a location is confirmed', () => {
    const store = makeStore();
    failed(store, 'INSUFFICIENT_SPACE', false);
    expect(store.chainStoragePickerRequested).toBe(false);
    store.chooseAnotherLocation();
    expect(store.chainStoragePickerRequested).toBe(true);
    store.confirmStorageLocation();
    expect(store.chainStoragePickerRequested).toBe(false);
  });

  it('marks the picker as asked for after a location error of a rebuild', () => {
    const store = makeStore();
    failed(store, 'INVALID_CHAIN_PATH', true);
    store.retryMithril();
    expect(store.chainStoragePickerRequested).toBe(true);
  });

  it("does not mark a first run's picker as asked for", () => {
    const store = makeStore();
    (store as any).hasChain = false;
    expect(store.loadingPhase).toBe('chain-storage-setup');
    expect(store.chainStoragePickerRequested).toBe(false);
  });

  it('marks the picker as leading to a rebuild after a rebuild error', () => {
    const store = makeStore();
    failed(store, 'INSUFFICIENT_SPACE', true);
    expect(store.locationPickerForRebuild).toBe(false);
    store.chooseAnotherLocation();
    expect(store.locationPickerForRebuild).toBe(true);
    store.confirmStorageLocation();
    expect(store.locationPickerForRebuild).toBe(false);
  });

  it('marks the picker as leading to a rebuild after a location error of a rebuild', () => {
    const store = makeStore();
    failed(store, 'INVALID_CHAIN_PATH', true);
    store.retryMithril();
    expect(store.loadingPhase).toBe('chain-storage-setup');
    expect(store.locationPickerForRebuild).toBe(true);
  });

  it('does not mark the picker as leading to a rebuild after a sync that was not a rebuild', () => {
    const store = makeStore();
    failed(store, 'INSUFFICIENT_SPACE', false);
    store.chooseAnotherLocation();
    expect(store.locationPickerForRebuild).toBe(false);
  });

  it('rebuilds in the default location when the picker resets to it', async () => {
    const store = makeStore();
    failed(store, 'INSUFFICIENT_SPACE', true);
    store.chooseAnotherLocation();
    await store.resetChainStorageDirectory();
    expect(confirmChainStorageChannel.request).not.toHaveBeenCalled();
    store.confirmStorageLocation();
    expect(store.pendingChainLocation).toBe('/state/chain');
    store.rebuildChain();
    expect(confirmChainStorageChannel.request).toHaveBeenCalledWith({
      customPath: null,
      rebuild: true,
    });
  });

  it('drops the picked location on Back and changes nothing', async () => {
    const store = makeStore();
    failed(store, 'CHAIN_DIR_NOT_REPLACEABLE', true);
    store.chooseAnotherLocation();
    await store.setChainStorageDirectory('/new');
    store.confirmStorageLocation();
    store.cancelRebuildConfirmation();
    expect(store._rebuildConfirmationRequested).toBe(false);
    expect(store.pendingChainLocation).toBeNull();
    expect(store.loadingPhase).toBe('mithril-syncing');
    expect(confirmChainStorageChannel.request).not.toHaveBeenCalled();
    expect(mithrilCommandChannel.send).not.toHaveBeenCalled();
  });

  it('rebuilds where it is when the picker keeps the location', () => {
    const store = makeStore();
    failed(store, 'INSUFFICIENT_SPACE', true);
    store.chooseAnotherLocation();
    store.confirmStorageLocation();
    expect(store._rebuildConfirmationRequested).toBe(true);
    expect(store.pendingChainLocation).toBeNull();
    store.rebuildChain();
    expect(mithrilCommandChannel.send).toHaveBeenCalledWith({
      cmd: 'start_mithril',
      wipe_chain: true,
    });
    expect(confirmChainStorageChannel.request).not.toHaveBeenCalled();
  });

  it('applies a location at once and returns to the error after a sync that was not a rebuild', async () => {
    const store = makeStore();
    failed(store, 'INSUFFICIENT_SPACE', false);
    (store as any).hasChain = false;
    store.chooseAnotherLocation();
    await store.setChainStorageDirectory('/new');
    expect(confirmChainStorageChannel.request).toHaveBeenCalledWith({
      customPath: '/new',
    });
    store.confirmStorageLocation();
    expect(store._rebuildConfirmationRequested).toBe(false);
    expect(store.loadingPhase).toBe('mithril-syncing');
  });

  it('drops a picked location when the Mithril phase leaves the error', async () => {
    const store = makeStore();
    failed(store, 'INSUFFICIENT_SPACE', true);
    store.chooseAnotherLocation();
    await store.setChainStorageDirectory('/new');
    store.confirmStorageLocation();
    (getCachedBackendStatusChannel.request as jest.Mock).mockResolvedValue({
      hasChain: true,
      blockSyncProgress: {
        replayedBlock: 0,
        validatingChunk: 0,
        pushingLedger: 0,
      },
      mithrilPhase: 'preparing',
    });
    await store._pollBackendStatus();
    expect(store.pendingChainLocation).toBeNull();
  });
});

describe('BackendStore after a rebuild removed the chain data', () => {
  beforeEach(() => {
    (mithrilCommandChannel.send as jest.Mock).mockClear();
  });

  const failedAfterWipe = (store: BackendStore) => {
    (store as any).hasChain = false;
    (store as any).chainPathConfirmed = true;
    (store as any).mithrilPhase = 'error';
    (store as any).lastErrorRebuild = true;
    (store as any).lastErrorCode = 'PARTIAL_SYNC_DOWNLOAD_COMMAND_FAILED';
  };

  it('shows the Mithril error first', () => {
    const store = makeStore();
    failedAfterWipe(store);
    expect(store.loadingPhase).toBe('mithril-syncing');
  });

  it('goes to the bootstrap decision from showBootstrapDecision', () => {
    const store = makeStore();
    failedAfterWipe(store);
    store.showBootstrapDecision();
    expect(store.loadingPhase).toBe('bootstrap-decision');
    expect(mithrilCommandChannel.send).not.toHaveBeenCalled();
  });

  it('shows the Mithril screen again when a Mithril run is started', () => {
    const store = makeStore();
    failedAfterWipe(store);
    store.showBootstrapDecision();
    store.startMithril();
    expect(store.loadingPhase).toBe('mithril-syncing');
  });

  it('leaves the dismissed error once the Mithril phase moves on', async () => {
    const store = makeStore();
    failedAfterWipe(store);
    store.showBootstrapDecision();
    await (store as any)._onMithrilStatus({ phase: 'preparing' });
    expect(store.loadingPhase).toBe('mithril-syncing');
    (store as any).mithrilPhase = 'error';
    expect(store.loadingPhase).toBe('mithril-syncing');
  });

  it('does not show the error during a sync from genesis after startNode', () => {
    const store = makeStore();
    failedAfterWipe(store);
    store.startNode();
    expect(mithrilCommandChannel.send).toHaveBeenCalledWith({
      cmd: 'start_node',
    });
    (store as any).hasChain = true;
    expect(store.loadingPhase).toBe('node-starting');
  });

  it('keeps the error view hidden through polls that still report it', async () => {
    const store = makeStore();
    failedAfterWipe(store);
    store.showBootstrapDecision();
    (getCachedBackendStatusChannel.request as jest.Mock).mockResolvedValue({
      hasChain: false,
      blockSyncProgress: {
        replayedBlock: 0,
        validatingChunk: 0,
        pushingLedger: 0,
      },
      mithrilPhase: 'error',
      lastErrorRebuild: true,
    });
    await store._pollBackendStatus();
    expect(store.loadingPhase).toBe('bootstrap-decision');
  });

  it('skips the location picker for a rebuild pending after a restart', () => {
    const store = makeStore();
    (store as any).hasChain = false;
    expect(store.loadingPhase).toBe('chain-storage-setup');
    (store as any).rebuildPending = true;
    expect(store.loadingPhase).toBe('bootstrap-decision');
  });

  it('skips the location picker for an install pending after a restart', () => {
    const store = makeStore();
    (store as any).hasChain = false;
    expect(store.loadingPhase).toBe('chain-storage-setup');
    (store as any).installPending = true;
    expect(store.loadingPhase).toBe('bootstrap-decision');
  });

  it('copies installPending from the poll', async () => {
    const store = makeStore();
    (getCachedBackendStatusChannel.request as jest.Mock).mockResolvedValue({
      hasChain: false,
      blockSyncProgress: {
        replayedBlock: 0,
        validatingChunk: 0,
        pushingLedger: 0,
      },
      mithrilPhase: null,
      rebuildPending: false,
      installPending: true,
    });
    await store._pollBackendStatus();
    expect(store.installPending).toBe(true);
    expect(store.rebuildPending).toBe(false);
    expect(store.loadingPhase).toBe('bootstrap-decision');
  });

  it('copies installRetryRequired from the poll', async () => {
    const store = makeStore();
    (getCachedBackendStatusChannel.request as jest.Mock).mockResolvedValue({
      hasChain: true,
      blockSyncProgress: {
        replayedBlock: 0,
        validatingChunk: 0,
        pushingLedger: 0,
      },
      mithrilPhase: 'error',
      lastErrorCode: 'INSTALL_FAILED',
      installRetryRequired: true,
    });
    await store._pollBackendStatus();
    expect(store.installRetryRequired).toBe(true);
  });

  it('copies rebuildPending from the poll', async () => {
    const store = makeStore();
    (getCachedBackendStatusChannel.request as jest.Mock).mockResolvedValue({
      hasChain: false,
      blockSyncProgress: {
        replayedBlock: 0,
        validatingChunk: 0,
        pushingLedger: 0,
      },
      mithrilPhase: null,
      rebuildPending: true,
    });
    await store._pollBackendStatus();
    expect(store.rebuildPending).toBe(true);
    expect(store.loadingPhase).toBe('bootstrap-decision');
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

describe('BackendStore wallet no-response', () => {
  const polled = (overrides: Record<string, unknown>) => ({
    hasChain: true,
    blockSyncProgress: {
      replayedBlock: 0,
      validatingChunk: 0,
      pushingLedger: 0,
    },
    mithrilPhase: null,
    walletUnrecoverable: false,
    nodeUnrecoverable: false,
    ...overrides,
  });

  it('mirrors the count and the last event from the poll', async () => {
    const store = makeStore();
    const last = {
      at: 1000,
      lastError: 'timeout after 10000 ms',
      willRestart: true,
    };
    (getCachedBackendStatusChannel.request as jest.Mock).mockResolvedValue(
      polled({ walletUnresponsiveRestarts: 2, lastWalletUnresponsive: last })
    );
    await store._pollBackendStatus();
    expect(store.walletUnresponsiveRestarts).toBe(2);
    expect(store.lastWalletUnresponsive).toEqual(last);
  });

  it('reads a poll from a state without the fields as none', async () => {
    const store = makeStore();
    (getCachedBackendStatusChannel.request as jest.Mock).mockResolvedValue(
      polled({})
    );
    await store._pollBackendStatus();
    expect(store.walletUnresponsiveRestarts).toBe(0);
    expect(store.lastWalletUnresponsive).toBeNull();
  });
});

describe('BackendStore Mithril run after the backend was given up on', () => {
  const polled = (overrides: Record<string, unknown>) => ({
    hasChain: true,
    blockSyncProgress: {
      replayedBlock: 0,
      validatingChunk: 0,
      pushingLedger: 0,
    },
    mithrilPhase: null,
    walletUnrecoverable: false,
    nodeUnrecoverable: false,
    ...overrides,
  });

  const givenUp = async () => {
    const store = makeStore();
    (getCachedBackendStatusChannel.request as jest.Mock).mockResolvedValue(
      polled({ walletUnrecoverable: true, nodeUnrecoverable: true })
    );
    await store._pollBackendStatus();
    expect(store.loadingPhase).toBe('error');
    return store;
  };

  beforeEach(() => {
    (mithrilCommandChannel.send as jest.Mock).mockClear();
  });

  it('shows the Mithril progress as soon as the run starts preparing', async () => {
    const store = await givenUp();
    await (store as any)._onMithrilStatus({ phase: 'preparing' });
    expect(store.nodeUnrecoverable).toBe(false);
    expect(store.walletUnrecoverable).toBe(false);
    expect(store.loadingPhase).toBe('mithril-syncing');
  });

  it('keeps the flags for any other Mithril phase pushed before the next poll', async () => {
    const store = await givenUp();
    await (store as any)._onMithrilStatus({ phase: 'completed' });
    expect(store.nodeUnrecoverable).toBe(true);
    expect(store.walletUnrecoverable).toBe(true);
  });

  it('shows the Mithril progress and then the error view with Retry when a rebuild fails to install', async () => {
    const store = await givenUp();
    (getCachedBackendStatusChannel.request as jest.Mock).mockResolvedValue(
      polled({ mithrilPhase: 'downloading' })
    );
    await store._pollBackendStatus();
    expect(store.loadingPhase).toBe('mithril-syncing');
    (getCachedBackendStatusChannel.request as jest.Mock).mockResolvedValue(
      polled({
        hasChain: false,
        mithrilPhase: 'error',
        lastErrorCode: 'INSTALL_FAILED',
        lastErrorRebuild: true,
        rebuildPending: true,
        installRetryRequired: true,
      })
    );
    await store._pollBackendStatus();
    expect(store.loadingPhase).toBe('mithril-syncing');
    expect(store.installRetryRequired).toBe(true);
    store.retryMithril();
    expect(mithrilCommandChannel.send).toHaveBeenCalledWith({
      cmd: 'start_mithril',
      wipe_chain: true,
    });
  });

  it('shows the generic error again when a process is given up on after the run', async () => {
    const store = await givenUp();
    await (store as any)._onMithrilStatus({ phase: 'preparing' });
    (getCachedBackendStatusChannel.request as jest.Mock).mockResolvedValue(
      polled({ mithrilPhase: 'completed', nodeUnrecoverable: true })
    );
    await store._pollBackendStatus();
    expect(store.loadingPhase).toBe('error');
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

describe('BackendStore after a rebuild in a newly picked location was refused', () => {
  beforeEach(() => {
    (mithrilCommandChannel.send as jest.Mock).mockClear();
    (confirmChainStorageChannel.request as jest.Mock)
      .mockReset()
      .mockResolvedValue(undefined);
  });

  it('leaves the stopping screen for the error view, and its sync choice for the bootstrap decision', async () => {
    const store = makeStore();
    (store as any).hasChain = true;
    (store as any).mithrilPhase = 'error';
    (store as any).lastErrorCode = 'INVALID_CHAIN_PATH';
    (store as any).lastErrorRebuild = true;
    store.retryMithril();
    await store.setChainStorageDirectory('/new');
    store.confirmStorageLocation();
    store.rebuildChain();
    // Main pushes the restart that the location change asked for.
    await (store as any)._onBackendStopStatus({
      quitting: false,
      restart: 'node',
      progress: null,
    });
    expect(store.loadingPhase).toBe('stopping');
    // The rebuild is refused in the new location, which holds no chain, and
    // main has ended the restart.
    (getCachedBackendStatusChannel.request as jest.Mock).mockResolvedValue({
      hasChain: false,
      blockSyncProgress: {
        replayedBlock: 0,
        validatingChunk: 0,
        pushingLedger: 0,
      },
      mithrilPhase: 'error',
      lastErrorCode: 'CHAIN_DIR_OTHER_NETWORK',
      lastErrorRebuild: true,
      mithrilErrorCount: 1,
      requestedRestart: null,
    });
    await store._pollBackendStatus();
    expect(store.loadingPhase).toBe('mithril-syncing');
    store.showBootstrapDecision();
    expect(store.loadingPhase).toBe('bootstrap-decision');
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
