import type { BrowserWindow } from 'electron';
import type { IpcSender } from '../../../common/ipc/lib/IpcChannel';

export const createCurrentWindowSender = () => {
  let currentWindow: BrowserWindow | null = null;
  return {
    bind: (window: BrowserWindow): void => {
      currentWindow = window;
    },
    sender: {
      send: (channel, ...args) => {
        if (!currentWindow || currentWindow.isDestroyed())
          throw new Error('Trusted main window is unavailable');
        currentWindow.webContents.send(channel, ...args);
      },
    } as IpcSender,
  };
};
