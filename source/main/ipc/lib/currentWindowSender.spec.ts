import { logger } from '../../utils/logging';
import {
  awaitIpcResponse,
  consumeIpcResponse,
  createCurrentWindowSender,
  isExpectedIpcLifecycleError,
  TRUSTED_WINDOW_UNAVAILABLE_MESSAGE,
} from './currentWindowSender';
import { IpcRequestCancelledError } from '../../../common/ipc/lib/IpcChannel';

jest.mock('../../utils/logging', () => ({
  logger: { error: jest.fn() },
}));

describe('currentWindowSender', () => {
  it('routes existing callbacks to the latest bound window', () => {
    const first = {
      isDestroyed: () => false,
      webContents: { send: jest.fn() },
    } as any;
    const second = {
      isDestroyed: () => false,
      webContents: { send: jest.fn() },
    } as any;
    const current = createCurrentWindowSender();

    current.bind(first);
    const callbackSender = current.sender;
    current.bind(second);
    callbackSender.send('channel', 'message');

    expect(first.webContents.send).not.toHaveBeenCalled();
    expect(second.webContents.send).toHaveBeenCalledWith('channel', 'message');
  });

  it('fails closed when no live trusted window is available', () => {
    const current = createCurrentWindowSender();

    expect(() => current.sender.send('channel')).toThrow(
      TRUSTED_WINDOW_UNAVAILABLE_MESSAGE
    );
  });

  it('never falls back to a destroyed or alternate window', () => {
    const destroyed = {
      isDestroyed: () => true,
      webContents: { send: jest.fn() },
    } as any;
    const current = createCurrentWindowSender();
    current.bind(destroyed);

    expect(() => current.sender.send('channel')).toThrow(
      TRUSTED_WINDOW_UNAVAILABLE_MESSAGE
    );
    expect(destroyed.webContents.send).not.toHaveBeenCalled();
  });

  it('consumes expected lifecycle rejection without logging', async () => {
    consumeIpcResponse(
      Promise.reject(new Error(TRUSTED_WINDOW_UNAVAILABLE_MESSAGE)),
      'channel'
    );
    await Promise.resolve();

    expect(logger.error).not.toHaveBeenCalled();
  });

  it('preserves unexpected awaited failures', async () => {
    await expect(
      awaitIpcResponse(Promise.reject(new Error('boom')))
    ).rejects.toThrow('boom');
  });

  it('classifies and settles typed navigation cancellation', async () => {
    const cancellation = new IpcRequestCancelledError();

    expect(isExpectedIpcLifecycleError(cancellation)).toBe(true);
    await expect(awaitIpcResponse(Promise.reject(cancellation))).resolves.toBe(
      undefined
    );
  });

  it('logs only fixed metadata for unexpected notification failures', async () => {
    consumeIpcResponse(
      Promise.reject(new Error('sensitive payload')),
      'channel'
    );
    await Promise.resolve();

    expect(logger.error).toHaveBeenCalledWith(
      'Main-to-renderer IPC notification failed',
      {
        channel: 'channel',
      }
    );
  });
});
