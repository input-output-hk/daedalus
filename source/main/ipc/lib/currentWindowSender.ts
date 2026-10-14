import type { BrowserWindow } from 'electron';
import {
  IPC_REQUEST_CANCELLED_MESSAGE,
  IpcSender,
} from '../../../common/ipc/lib/IpcChannel';
import { logger } from '../../utils/logging';

export const TRUSTED_WINDOW_UNAVAILABLE_MESSAGE =
  'Trusted main window is unavailable';

export const createCurrentWindowSender = () => {
  let currentWindow: BrowserWindow | null = null;
  return {
    bind: (window: BrowserWindow): void => {
      currentWindow = window;
    },
    sender: {
      send: (channel, ...args) => {
        if (!currentWindow || currentWindow.isDestroyed())
          throw new Error(TRUSTED_WINDOW_UNAVAILABLE_MESSAGE);
        currentWindow.webContents.send(channel, ...args);
      },
    } as IpcSender,
  };
};

export const currentWindowSender = createCurrentWindowSender();

export const isExpectedIpcLifecycleError = (error: unknown): boolean =>
  error instanceof Error &&
  (error.message === IPC_REQUEST_CANCELLED_MESSAGE ||
    error.message === TRUSTED_WINDOW_UNAVAILABLE_MESSAGE);

export const consumeIpcResponse = (
  promise: Promise<unknown>,
  channel: string
): void => {
  promise.catch((error) => {
    if (isExpectedIpcLifecycleError(error)) return;
    logger.error('Main-to-renderer IPC notification failed', { channel });
  });
};

export const awaitIpcResponse = async <T>(
  promise: Promise<T>
): Promise<T | undefined> => {
  try {
    return await promise;
  } catch (error) {
    if (isExpectedIpcLifecycleError(error)) return undefined;
    throw error;
  }
};
