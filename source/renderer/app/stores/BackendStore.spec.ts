import type { Api } from '../api/index';
import type { ActionsMap } from '../actions/index';
import BackendStore from './BackendStore';
import { noopAnalyticsTracker } from '../analytics';

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
});
