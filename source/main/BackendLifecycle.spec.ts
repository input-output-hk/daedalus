import { IPC_REQUEST_CANCELLED_MESSAGE } from '../common/ipc/lib/IpcChannel';
import BackendLifecycle from './BackendLifecycle';
import { revokeCip30Sessions } from './cip30/runtime';
import {
  backendStopStatusChannel,
  watchdogStoppedChannel,
} from './ipc/nodePushChannel';
import { walletPortChannel } from './ipc/mithrilPushChannel';
import { logger } from './utils/logging';

type Deferred<T> = {
  promise: Promise<T>;
  resolve: (value: T | PromiseLike<T>) => void;
  reject: (reason?: unknown) => void;
};

const deferred = <T>(): Deferred<T> => {
  const promiseWithResolvers = Promise as unknown as {
    withResolvers<U>(): Deferred<U>;
  };
  return promiseWithResolvers.withResolvers<T>();
};

let walletReady: Deferred<number>;
let watchdogEvent: (event: Record<string, unknown>) => void;
const manager = {
  getState: jest.fn(),
  onEvent: jest.fn((handler: (event: Record<string, unknown>) => void) => {
    watchdogEvent = handler;
  }),
  sendCommand: jest.fn(),
  start: jest.fn(),
  stop: jest.fn(() => Promise.resolve()),
  walletReadyPromise: Promise.resolve(0),
};

jest.mock('./WatchdogManager', () => jest.fn(() => manager));

jest.mock('./cip30/runtime', () => ({ revokeCip30Sessions: jest.fn() }));
jest.mock('./ipc/mithrilPushChannel', () => ({
  mithrilProgressChannel: { send: jest.fn(() => Promise.resolve()) },
  mithrilStatusChannel: { send: jest.fn(() => Promise.resolve()) },
  walletPortChannel: { send: jest.fn(() => Promise.resolve()) },
}));
jest.mock('./ipc/nodePushChannel', () => ({
  nodeStartupStatusChannel: { send: jest.fn(() => Promise.resolve()) },
  nodeBlockSyncProgressChannel: { send: jest.fn(() => Promise.resolve()) },
  watchdogStoppedChannel: { send: jest.fn(() => Promise.resolve()) },
  backendStopStatusChannel: { send: jest.fn(() => Promise.resolve()) },
}));
jest.mock('./utils/logging', () => ({
  logger: { error: jest.fn(), info: jest.fn(), warn: jest.fn() },
}));

const settle = async () => {
  await Promise.resolve();
  await Promise.resolve();
};

describe('BackendLifecycle', () => {
  beforeEach(() => {
    jest.clearAllMocks();
    walletReady = deferred<number>();
    manager.walletReadyPromise = walletReady.promise;
    manager.stop.mockResolvedValue(undefined);
    manager.getState.mockReturnValue({
      requestedRestart: null,
      backendStopProgress: null,
      walletPort: 8090,
    });
    delete process.env.TLS_CA_CERT;
    delete process.env.TLS_CLIENT_CERT;
    delete process.env.TLS_CLIENT_KEY;
  });

  it('revokes sessions when the watchdog stops and consumes cancellation', async () => {
    (watchdogStoppedChannel.send as jest.Mock).mockRejectedValueOnce(
      new Error(IPC_REQUEST_CANCELLED_MESSAGE)
    );
    const lifecycle = new BackendLifecycle();
    lifecycle.start();

    watchdogEvent({ event: 'stopped' });
    await settle();

    expect(revokeCip30Sessions).toHaveBeenCalledTimes(1);
    expect(logger.error).not.toHaveBeenCalled();
  });

  it('pushes each wallet readiness event without duplicating the initial promise', async () => {
    const lifecycle = new BackendLifecycle();
    lifecycle.start();

    walletReady.resolve(8090);
    await settle();
    expect(walletPortChannel.send).not.toHaveBeenCalled();

    watchdogEvent({ event: 'wallet_ready', port: 8090 });
    watchdogEvent({ event: 'wallet_ready', port: 8091 });
    await settle();

    expect(walletPortChannel.send).toHaveBeenCalledTimes(2);
    expect(walletPortChannel.send).toHaveBeenNthCalledWith(
      1,
      { port: 8090, ca: [], cert: [], key: [] },
      expect.anything()
    );
    expect(walletPortChannel.send).toHaveBeenNthCalledWith(
      2,
      { port: 8091, ca: [], cert: [], key: [] },
      expect.anything()
    );
    expect(revokeCip30Sessions).not.toHaveBeenCalled();
  });

  it('revokes sessions when wallet startup fails', async () => {
    const lifecycle = new BackendLifecycle();
    lifecycle.start();
    walletReady.reject(new Error('startup failed'));
    await settle();

    expect(revokeCip30Sessions).toHaveBeenCalledTimes(1);
  });

  it('shares a controlled stop and retains backend state and progress', async () => {
    const stopped = deferred<void>();
    manager.stop.mockReturnValue(stopped.promise);
    const lifecycle = new BackendLifecycle();
    lifecycle.start();

    const firstStop = lifecycle.stop();
    expect(revokeCip30Sessions).toHaveBeenCalledTimes(1);
    expect(lifecycle.isStopping()).toBe(true);
    expect(backendStopStatusChannel.send).toHaveBeenLastCalledWith(
      { quitting: true, restart: null, progress: null },
      expect.anything()
    );
    expect(lifecycle.stop()).toBe(firstStop);
    expect(manager.stop).toHaveBeenCalledTimes(1);

    const progress = {
      stage: 'stopping_wallet',
      elapsedMs: 1000,
      timeoutMs: 10000,
    };
    manager.getState.mockReturnValue({
      requestedRestart: null,
      backendStopProgress: progress,
      walletPort: 8090,
    });
    watchdogEvent({ event: 'backend_stop_progress' });
    expect(backendStopStatusChannel.send).toHaveBeenLastCalledWith(
      { quitting: true, restart: null, progress },
      expect.anything()
    );
    expect(lifecycle.getState()).toEqual(
      expect.objectContaining({ backendStopProgress: progress, walletPort: 8090 })
    );

    stopped.resolve(undefined);
    await firstStop;
    expect(lifecycle.getState()).toEqual(
      expect.objectContaining({ backendStopProgress: progress })
    );
  });
});
