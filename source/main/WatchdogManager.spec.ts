/**
 * @jest-environment node
 */
import { EventEmitter } from 'events';
import WatchdogManager, {
  INSTALLER_RESULT_TIMEOUT_MS,
  STOP_ACK_TIMEOUT_MS,
  STOP_MARGIN_MS,
} from './WatchdogManager';

jest.mock('./utils/logging', () => ({
  logger: {
    debug: jest.fn(),
    info: jest.fn(),
    error: jest.fn(),
    warn: jest.fn(),
  },
}));

// A manager wired to a fake IPC channel: `channel` stands in for the readline
// interface that start() creates, events arrive as JSON lines the way the
// watchdog writes them, and outgoing commands are recorded instead of sent.
const makeManager = () => {
  const manager = new WatchdogManager();
  const channel = new EventEmitter();
  const sent: Array<Record<string, unknown>> = [];
  (manager as any)._attachChannel(channel);
  jest
    .spyOn(manager, 'sendCommand')
    .mockImplementation((cmd: Record<string, unknown>) => {
      sent.push(cmd);
    });
  const emit = (event: Record<string, unknown>) =>
    channel.emit('line', JSON.stringify(event));
  return { manager, channel, sent, emit };
};

const progress = (stage: string, elapsedMs: number, timeoutMs: number) => ({
  event: 'backend_stop_progress',
  stage,
  elapsed_ms: elapsedMs,
  timeout_ms: timeoutMs,
});

// Resolves to true once `promise` has settled, false if it is still pending.
const isSettled = async (promise: Promise<unknown>): Promise<boolean> => {
  let settled = false;
  promise.then(() => {
    settled = true;
  });
  await Promise.resolve();
  await Promise.resolve();
  return settled;
};

// A manager whose commands go to a fake IPC socket, so sendCommand runs as is.
const makeManagerWithSocket = () => {
  const manager = new WatchdogManager();
  const channel = new EventEmitter();
  (manager as any)._attachChannel(channel);
  (manager as any).socket = { writable: true, write: jest.fn() };
  const emit = (event: Record<string, unknown>) =>
    channel.emit('line', JSON.stringify(event));
  return { manager, emit };
};

describe('WatchdogManager requested restart', () => {
  it('marks a node restart sent while the node runs, until the new node starts', () => {
    const { manager, emit } = makeManagerWithSocket();
    emit({ event: 'node_started', pid: 1, started_at_unix_ms: 0 });
    manager.sendCommand({ cmd: 'set_node_extra_args', args: [] });
    expect(manager.getState().requestedRestart).toBe('node');
    emit(progress('stopping_node', 0, 300_000));
    emit({ event: 'node_shutdown_ms', ms: 900, force_killed: false });
    expect(manager.getState().requestedRestart).toBe('node');
    emit({ event: 'node_started', pid: 2, started_at_unix_ms: 0 });
    expect(manager.getState().requestedRestart).toBeNull();
  });

  it('marks a wallet restart until the wallet is ready again', () => {
    const { manager, emit } = makeManagerWithSocket();
    emit({ event: 'node_started', pid: 1, started_at_unix_ms: 0 });
    manager.sendCommand({ cmd: 'restart_wallet' });
    expect(manager.getState().requestedRestart).toBe('wallet');
    emit({ event: 'wallet_started', pid: 3, started_at_unix_ms: 0 });
    expect(manager.getState().requestedRestart).toBe('wallet');
    emit({ event: 'wallet_ready', port: 8090, waited_ms: 0 });
    expect(manager.getState().requestedRestart).toBeNull();
  });

  it('marks nothing when no node is running to restart', () => {
    const { manager, emit } = makeManagerWithSocket();
    manager.sendCommand({ cmd: 'set_chain_path', path: '/x' });
    expect(manager.getState().requestedRestart).toBeNull();
    emit({ event: 'node_started', pid: 1, started_at_unix_ms: 0 });
    emit({ event: 'node_exited', code: 1, signal: null });
    manager.sendCommand({ cmd: 'restart_node' });
    expect(manager.getState().requestedRestart).toBeNull();
  });
});

describe('WatchdogManager unrecoverable state', () => {
  it('records node_unrecoverable', () => {
    const { manager, emit } = makeManager();
    emit({ event: 'node_unrecoverable', crashes: 5 });
    expect(manager.getState().nodeUnrecoverable).toBe(true);
  });

  it('clears both unrecoverable flags when a node starts again', () => {
    const { manager, emit } = makeManager();
    emit({ event: 'wallet_unrecoverable', attempt: 1 });
    emit({ event: 'node_unrecoverable', crashes: 5 });
    emit({ event: 'node_started', pid: 2, started_at_unix_ms: 0 });
    expect(manager.getState().nodeUnrecoverable).toBe(false);
    expect(manager.getState().walletUnrecoverable).toBe(false);
  });

  it('clears the wallet flag when a wallet starts again', () => {
    const { manager, emit } = makeManager();
    emit({ event: 'wallet_unrecoverable', attempt: 1 });
    emit({ event: 'wallet_started', pid: 3, started_at_unix_ms: 0 });
    expect(manager.getState().walletUnrecoverable).toBe(false);
  });
});

describe('WatchdogManager Mithril error', () => {
  it('keeps the code of a mithril_error as lastErrorCode', () => {
    const { manager, emit } = makeManager();
    expect(manager.getState().lastErrorCode).toBeNull();
    emit({
      event: 'mithril_error',
      code: 'PROBE_FAILED',
      message: 'connection refused',
    });
    expect(manager.getState().mithrilPhase).toBe('error');
    expect(manager.getState().lastError).toBe('connection refused');
    expect(manager.getState().lastErrorCode).toBe('PROBE_FAILED');
  });

  it('clears lastErrorCode when an error without a code replaces lastError', () => {
    const { manager, emit } = makeManager();
    emit({
      event: 'mithril_error',
      code: 'PROBE_FAILED',
      message: 'connection refused',
    });
    emit({ event: 'error', message: 'cardano-node unrecoverable' });
    expect(manager.getState().lastError).toBe('cardano-node unrecoverable');
    expect(manager.getState().lastErrorCode).toBeNull();
  });
});

describe('WatchdogManager chain damage', () => {
  const damage = {
    event: 'chain_damage',
    kind: 'wallet_chain_discontinuity',
    slot: 200102408,
    last_good_slot: 200080892,
  };

  it('starts with no chain damage', () => {
    const { manager } = makeManager();
    expect(manager.getState().chainDamage).toBeNull();
  });

  it('sets chainDamage from a chain_damage event, with the time it arrived', () => {
    const { manager, emit } = makeManager();
    const before = Date.now();
    emit(damage);
    const found = manager.getState().chainDamage;
    expect(found).toMatchObject({
      kind: 'wallet_chain_discontinuity',
      slot: 200102408,
      lastGoodSlot: 200080892,
    });
    expect(Date.parse(found!.detectedAt)).toBeGreaterThanOrEqual(before);
  });

  it('keeps null slots as null', () => {
    const { manager, emit } = makeManager();
    emit({
      event: 'chain_damage',
      kind: 'ledger_point_missing',
      slot: null,
      last_good_slot: null,
    });
    expect(manager.getState().chainDamage).toMatchObject({
      kind: 'ledger_point_missing',
      slot: null,
      lastGoodSlot: null,
    });
  });

  it('clears chainDamage when a node starts', () => {
    const { manager, emit } = makeManager();
    emit(damage);
    emit({ event: 'node_started', pid: 2, started_at_unix_ms: 0 });
    expect(manager.getState().chainDamage).toBeNull();
  });

  it('clears chainDamage when a Mithril sync starts preparing', () => {
    const { manager, emit } = makeManager();
    emit(damage);
    emit({ event: 'mithril_status', phase: 'downloading' });
    expect(manager.getState().chainDamage).not.toBeNull();
    emit({ event: 'mithril_status', phase: 'preparing' });
    expect(manager.getState().chainDamage).toBeNull();
  });
});

describe('WatchdogManager node held for chain damage', () => {
  const held = {
    event: 'chain_damage',
    kind: 'ledger_point_missing',
    slot: 136771192,
    last_good_slot: null,
    node_held: true,
  };

  it('records that the watchdog holds the node for damage found before it started', () => {
    const { manager, emit } = makeManager();
    expect(manager.getState().nodeHeld).toBe(false);
    emit(held);
    expect(manager.getState().nodeHeld).toBe(true);
    expect(manager.getState().chainDamage).toEqual(
      expect.objectContaining({ kind: 'ledger_point_missing', slot: 136771192 })
    );
  });

  it('records damage found in a running node as not held', () => {
    const { manager, emit } = makeManager();
    emit({ ...held, node_held: false });
    expect(manager.getState().nodeHeld).toBe(false);
    emit({ ...held, node_held: undefined });
    expect(manager.getState().nodeHeld).toBe(false);
  });

  it('clears nodeHeld when a node starts or a Mithril sync starts preparing', () => {
    const { manager, emit } = makeManager();
    emit(held);
    emit({ event: 'node_started', pid: 1, started_at_unix_ms: 0 });
    expect(manager.getState().nodeHeld).toBe(false);
    emit(held);
    emit({ event: 'mithril_status', phase: 'preparing', rebuild: true });
    expect(manager.getState().nodeHeld).toBe(false);
  });

  it('ends a requested node restart when the watchdog holds the new node', () => {
    const { manager, emit } = makeManagerWithSocket();
    emit({ event: 'node_started', pid: 1, started_at_unix_ms: 0 });
    manager.sendCommand({ cmd: 'restart_node' });
    emit({ event: 'node_shutdown_ms', ms: 200, force_killed: false });
    expect(manager.getState().requestedRestart).toBe('node');
    emit(held);
    expect(manager.getState().requestedRestart).toBeNull();
  });
});

describe('WatchdogManager rebuild state', () => {
  it('records whether a mithril_error came from a rebuild', () => {
    const { manager, emit } = makeManager();
    expect(manager.getState().lastErrorRebuild).toBe(false);
    emit({
      event: 'mithril_error',
      code: 'INSUFFICIENT_SPACE',
      message: 'not enough space',
      rebuild: true,
    });
    expect(manager.getState().lastErrorRebuild).toBe(true);
    emit({ event: 'mithril_error', code: 'DOWNLOAD_FAILED', message: 'x' });
    expect(manager.getState().lastErrorRebuild).toBe(false);
  });

  it.each([
    ['a Mithril run starts', { cmd: 'start_mithril' }],
    ['a rebuild starts', { cmd: 'start_mithril', wipe_chain: true }],
    ['the node is started without Mithril', { cmd: 'start_node' }],
  ])('clears lastErrorRebuild when %s', (_name, cmd) => {
    const { manager, emit } = makeManagerWithSocket();
    emit({ event: 'mithril_error', code: 'X', message: 'x', rebuild: true });
    manager.sendCommand(cmd);
    expect(manager.getState().lastErrorRebuild).toBe(false);
  });

  it('clears lastErrorRebuild when the pipeline completes', () => {
    const { manager, emit } = makeManager();
    emit({ event: 'mithril_error', code: 'X', message: 'x', rebuild: true });
    emit({ event: 'mithril_status', phase: 'completed' });
    expect(manager.getState().lastErrorRebuild).toBe(false);
  });

  it('clears the Mithril phase and progress when the node is started', () => {
    const { manager, emit } = makeManagerWithSocket();
    emit({ event: 'mithril_status', phase: 'downloading' });
    emit({ event: 'mithril_error', code: 'X', message: 'x' });
    expect(manager.getState().mithrilPhase).toBe('error');
    manager.sendCommand({ cmd: 'start_node' });
    expect(manager.getState().mithrilPhase).toBeNull();
    expect(manager.getState().mithrilProgress).toBeNull();
  });

  it('keeps the Mithril phase when a Mithril run is started', () => {
    const { manager, emit } = makeManagerWithSocket();
    emit({ event: 'mithril_error', code: 'X', message: 'x' });
    manager.sendCommand({ cmd: 'start_mithril' });
    expect(manager.getState().mithrilPhase).toBe('error');
  });

  it('keeps lastErrorRebuild when other commands are sent', () => {
    const { manager, emit } = makeManagerWithSocket();
    emit({ event: 'mithril_error', code: 'X', message: 'x', rebuild: true });
    manager.sendCommand({ cmd: 'probe_mithril' });
    expect(manager.getState().lastErrorRebuild).toBe(true);
  });

  it('maps rebuild_pending from chain_status and defaults it to false', () => {
    const { manager, emit } = makeManager();
    expect(manager.getState().rebuildPending).toBe(false);
    emit({ event: 'chain_status', has_chain: false, rebuild_pending: true });
    expect(manager.getState().rebuildPending).toBe(true);
    emit({ event: 'chain_status', has_chain: false });
    expect(manager.getState().rebuildPending).toBe(false);
  });

  it('maps install_pending from chain_status and defaults it to false', () => {
    const { manager, emit } = makeManager();
    expect(manager.getState().installPending).toBe(false);
    emit({
      event: 'chain_status',
      has_chain: false,
      rebuild_pending: false,
      install_pending: true,
    });
    expect(manager.getState().installPending).toBe(true);
    expect(manager.getState().rebuildPending).toBe(false);
    emit({ event: 'chain_status', has_chain: false });
    expect(manager.getState().installPending).toBe(false);
  });

  it('clears installPending when a node starts or the pipeline completes', () => {
    const { manager, emit } = makeManager();
    const pending = {
      event: 'chain_status',
      has_chain: false,
      rebuild_pending: false,
      install_pending: true,
    };
    emit(pending);
    emit({ event: 'node_started', pid: 2, started_at_unix_ms: 0 });
    expect(manager.getState().installPending).toBe(false);
    emit(pending);
    emit({ event: 'mithril_status', phase: 'completed', rebuild: true });
    expect(manager.getState().installPending).toBe(false);
  });

  it('clears rebuildPending when a node starts or the pipeline completes', () => {
    const { manager, emit } = makeManager();
    emit({ event: 'chain_status', has_chain: false, rebuild_pending: true });
    emit({ event: 'node_started', pid: 2, started_at_unix_ms: 0 });
    expect(manager.getState().rebuildPending).toBe(false);
    emit({ event: 'chain_status', has_chain: false, rebuild_pending: true });
    emit({ event: 'mithril_status', phase: 'completed' });
    expect(manager.getState().rebuildPending).toBe(false);
  });
});

describe('WatchdogManager failed install', () => {
  const installFailed = (rebuild: boolean) => ({
    event: 'mithril_error',
    code: 'INSTALL_FAILED',
    message: 'install staged db: permission denied',
    rebuild,
  });

  it('records the wait for another install until a node starts', () => {
    const { manager, emit } = makeManager();
    expect(manager.getState().installRetryRequired).toBe(false);
    emit(installFailed(false));
    expect(manager.getState().installRetryRequired).toBe(true);
    emit({ event: 'node_started', pid: 2, started_at_unix_ms: 0 });
    expect(manager.getState().installRetryRequired).toBe(false);
  });

  it('ends the wait when the watchdog reports that a rebuild left no chain', () => {
    const { manager, emit } = makeManager();
    emit(installFailed(false));
    emit({ event: 'chain_status', has_chain: false, rebuild_pending: true });
    expect(manager.getState().installRetryRequired).toBe(false);
  });

  it('keeps the wait through other errors', () => {
    const { manager, emit } = makeManager();
    emit(installFailed(false));
    emit({
      event: 'mithril_error',
      code: 'PROBE_FAILED',
      message: 'x',
      rebuild: true,
    });
    expect(manager.getState().installRetryRequired).toBe(true);
  });

  it('keeps the error when start_node is sent during the wait, which the watchdog ignores', () => {
    const { manager, emit } = makeManagerWithSocket();
    emit({ event: 'mithril_status', phase: 'installing' });
    emit(installFailed(true));
    manager.sendCommand({ cmd: 'start_node' });
    expect(manager.getState().mithrilPhase).toBe('error');
    expect(manager.getState().lastErrorRebuild).toBe(true);
  });

  it('marks the rebuild pending when the install of a rebuild fails', () => {
    const { manager, emit } = makeManager();
    emit(installFailed(true));
    expect(manager.getState().rebuildPending).toBe(true);
  });

  it('marks no rebuild pending when the install of a partial sync fails', () => {
    const { manager, emit } = makeManager();
    emit(installFailed(false));
    expect(manager.getState().rebuildPending).toBe(false);
  });

  it('marks the install pending when an install that was not a rebuild fails', () => {
    const { manager, emit } = makeManager();
    emit(installFailed(false));
    expect(manager.getState().installPending).toBe(true);
  });

  it('marks no install pending when the install of a rebuild fails', () => {
    const { manager, emit } = makeManager();
    emit(installFailed(true));
    expect(manager.getState().installPending).toBe(false);
  });

  it('keeps an unfinished install an install when the rebuild that follows fails to install', () => {
    const { manager, emit } = makeManager();
    emit({
      event: 'chain_status',
      has_chain: false,
      rebuild_pending: false,
      install_pending: true,
    });
    emit(installFailed(true));
    expect(manager.getState().installPending).toBe(true);
    expect(manager.getState().rebuildPending).toBe(false);
  });
});

describe('WatchdogManager last chain damage', () => {
  const damage = {
    event: 'chain_damage',
    kind: 'ledger_point_missing',
    slot: 200102375,
    last_good_slot: null,
  };

  it('is set with chainDamage', () => {
    const { manager, emit } = makeManager();
    expect(manager.getState().lastChainDamage).toBeNull();
    emit(damage);
    expect(manager.getState().lastChainDamage).toEqual(
      manager.getState().chainDamage
    );
    expect(manager.getState().lastChainDamage).not.toBeNull();
  });

  it('survives a node start and a Mithril sync preparing', () => {
    const { manager, emit } = makeManager();
    emit(damage);
    emit({ event: 'node_started', pid: 2, started_at_unix_ms: 0 });
    emit({ event: 'mithril_status', phase: 'preparing' });
    expect(manager.getState().chainDamage).toBeNull();
    expect(manager.getState().lastChainDamage).not.toBeNull();
  });

  it('is cleared when a rebuild completes', () => {
    const { manager, emit } = makeManagerWithSocket();
    emit(damage);
    manager.sendCommand({ cmd: 'start_mithril', wipe_chain: true });
    emit({ event: 'mithril_status', phase: 'preparing', rebuild: true });
    emit({ event: 'mithril_status', phase: 'completed', rebuild: true });
    expect(manager.getState().lastChainDamage).toBeNull();
  });

  it('is cleared when the bootstrap decision is accepted after a failed rebuild and that sync completes', () => {
    const { manager, emit } = makeManagerWithSocket();
    emit(damage);
    manager.sendCommand({ cmd: 'start_mithril', wipe_chain: true });
    emit({ event: 'mithril_status', phase: 'preparing', rebuild: true });
    emit({
      event: 'mithril_error',
      code: 'PARTIAL_SYNC_DOWNLOAD_COMMAND_FAILED',
      message: 'x',
      rebuild: true,
    });
    emit({ event: 'chain_status', has_chain: false, rebuild_pending: true });
    // Accept on the bootstrap decision sends a plain sync, which the watchdog
    // runs as a rebuild.
    manager.sendCommand({ cmd: 'start_mithril' });
    emit({ event: 'mithril_status', phase: 'preparing', rebuild: true });
    emit({ event: 'mithril_status', phase: 'finalizing', rebuild: true });
    emit({ event: 'node_started', pid: 2, started_at_unix_ms: 0 });
    expect(manager.getState().lastChainDamage).not.toBeNull();
    emit({ event: 'mithril_status', phase: 'completed', rebuild: true });
    expect(manager.getState().lastChainDamage).toBeNull();
  });

  it('is kept when a partial sync completes', () => {
    const { manager, emit } = makeManagerWithSocket();
    emit(damage);
    manager.sendCommand({ cmd: 'start_mithril' });
    emit({ event: 'mithril_status', phase: 'completed', rebuild: false });
    expect(manager.getState().lastChainDamage).not.toBeNull();
  });

  it('is kept when a rebuild was asked for but the sync that completed was not one', () => {
    const { manager, emit } = makeManagerWithSocket();
    emit(damage);
    manager.sendCommand({ cmd: 'start_mithril', wipe_chain: true });
    emit({ event: 'mithril_error', code: 'X', message: 'x', rebuild: true });
    emit({ event: 'mithril_status', phase: 'completed', rebuild: false });
    expect(manager.getState().lastChainDamage).not.toBeNull();
  });
});

describe('WatchdogManager.stop', () => {
  beforeEach(() => {
    jest.useFakeTimers();
  });

  afterEach(() => {
    jest.useRealTimers();
  });

  it('sends one stop command and resolves when the watchdog reports stopped', async () => {
    const { manager, sent, emit } = makeManager();
    const stopping = manager.stop();
    expect(sent).toEqual([{ cmd: 'stop' }]);
    expect(await isSettled(stopping)).toBe(false);

    emit({ event: 'stopped' });
    expect(await isSettled(stopping)).toBe(true);
  });

  it('returns the same stop to a second caller without sending another command', async () => {
    const { manager, sent, emit } = makeManager();
    const first = manager.stop();
    const second = manager.stop();
    expect(second).toBe(first);
    expect(sent).toHaveLength(1);

    emit({ event: 'stopped' });
    expect(await isSettled(second)).toBe(true);
  });

  it('resolves when the IPC channel closes', async () => {
    const { manager, channel } = makeManager();
    const stopping = manager.stop();
    channel.emit('close');
    expect(await isSettled(stopping)).toBe(true);
  });

  it('resolves at once when the watchdog has already stopped', async () => {
    const { manager, sent, emit } = makeManager();
    emit({ event: 'stopped' });
    expect(await isSettled(manager.stop())).toBe(true);
    expect(sent).toHaveLength(0);
  });

  it('records shutdownRequested and the latest stop progress in state', () => {
    const { manager, emit } = makeManager();
    manager.stop();
    emit(progress('stopping_node', 4000, 300_000));
    expect(manager.getState().shutdownRequested).toBe(true);
    expect(manager.getState().backendStopProgress).toEqual({
      stage: 'stopping_node',
      elapsedMs: 4000,
      timeoutMs: 300_000,
    });
  });

  it('gives up when the watchdog sends nothing within the acknowledgement timeout', async () => {
    const { manager } = makeManager();
    const stopping = manager.stop();
    jest.advanceTimersByTime(STOP_ACK_TIMEOUT_MS - 1);
    expect(await isSettled(stopping)).toBe(false);
    jest.advanceTimersByTime(1);
    expect(await isSettled(stopping)).toBe(true);
  });

  it('keeps waiting while the watchdog reports a node stop within its bound', async () => {
    const { manager, emit } = makeManager();
    const nodeBoundMs = 300_000;
    const stopping = manager.stop();
    emit(progress('stopping_wallet', 0, 10_000));
    jest.advanceTimersByTime(2000);
    emit(progress('stopping_node', 0, nodeBoundMs));
    // Well past both the acknowledgement timeout and the old fixed 45 s.
    jest.advanceTimersByTime(nodeBoundMs);
    expect(await isSettled(stopping)).toBe(false);

    emit({ event: 'stopped' });
    expect(await isSettled(stopping)).toBe(true);
  });

  it('gives up a margin after the reported bound when the watchdog goes quiet', async () => {
    const { manager, emit } = makeManager();
    const nodeBoundMs = 300_000;
    const stopping = manager.stop();
    emit(progress('stopping_node', 0, nodeBoundMs));
    jest.advanceTimersByTime(nodeBoundMs + STOP_MARGIN_MS - 1);
    expect(await isSettled(stopping)).toBe(false);
    jest.advanceTimersByTime(1);
    expect(await isSettled(stopping)).toBe(true);
  });

  it('does not shorten the deadline when a later event reports less time left', async () => {
    const { manager, emit } = makeManager();
    const stopping = manager.stop();
    emit(progress('stopping_node', 0, 300_000));
    jest.advanceTimersByTime(1000);
    emit(progress('stopping_wallet', 0, 1000));
    jest.advanceTimersByTime(STOP_ACK_TIMEOUT_MS + STOP_MARGIN_MS);
    expect(await isSettled(stopping)).toBe(false);
  });
});

describe('WatchdogManager update installer', () => {
  beforeEach(() => {
    jest.useFakeTimers();
  });

  afterEach(() => {
    jest.useRealTimers();
  });

  it('sends install_update with the installer path', () => {
    const { manager, sent } = makeManager();
    manager.requestInstall('C:\\Downloads\\installer.exe');
    expect(sent).toEqual([
      {
        cmd: 'install_update',
        path: 'C:\\Downloads\\installer.exe',
        args: [],
      },
    ]);
  });

  it('waits past stopped for the installer report, then resolves', async () => {
    const { manager, emit } = makeManager();
    manager.requestInstall('/tmp/installer.pkg');
    const stopping = manager.stop();
    emit({ event: 'stopped' });
    expect(await isSettled(stopping)).toBe(false);
    emit({ event: 'update_installer_launched', pid: 77 });
    expect(await isSettled(stopping)).toBe(true);
    expect(manager.getInstallResult()).toEqual({
      launched: true,
      message: null,
    });
  });

  it('records a failed installer start with its reason', async () => {
    const { manager, emit } = makeManager();
    manager.requestInstall('/tmp/installer.pkg');
    const stopping = manager.stop();
    emit({ event: 'stopped' });
    emit({ event: 'update_installer_failed', message: 'not found' });
    expect(await isSettled(stopping)).toBe(true);
    expect(manager.getInstallResult()).toEqual({
      launched: false,
      message: 'not found',
    });
  });

  it('reports a failure when the watchdog never answers about the installer', async () => {
    const { manager, emit } = makeManager();
    manager.requestInstall('/tmp/installer.pkg');
    const stopping = manager.stop();
    emit({ event: 'stopped' });
    jest.advanceTimersByTime(INSTALLER_RESULT_TIMEOUT_MS);
    expect(await isSettled(stopping)).toBe(true);
    expect(manager.getInstallResult()?.launched).toBe(false);
  });

  it('reports nothing when no installer was requested', () => {
    const { manager } = makeManager();
    expect(manager.getInstallResult()).toBeNull();
  });
});
