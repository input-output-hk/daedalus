import { IPC_REQUEST_CANCELLED_MESSAGE } from '../common/ipc/lib/IpcChannel';
import BackendLifecycle from './BackendLifecycle';
import { revokeCip30Sessions } from './cip30/runtime';
import { watchdogStoppedChannel } from './ipc/nodePushChannel';
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

  it('revokes sessions on startup failure and explicit stop', async () => {
    const lifecycle = new BackendLifecycle();
    lifecycle.start();
    walletReady.reject(new Error('startup failed'));
    await settle();
    await lifecycle.stop();

    expect(revokeCip30Sessions).toHaveBeenCalledTimes(2);
    expect(manager.stop).toHaveBeenCalledTimes(1);
  });
});
