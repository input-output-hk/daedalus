/**
 * @jest-environment node
 */
import { EventEmitter } from 'events';
import WatchdogManager, {
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
