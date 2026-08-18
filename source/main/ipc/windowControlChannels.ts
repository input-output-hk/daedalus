import type { BrowserWindow } from 'electron';
import {
  CLOSE_WINDOW_CHANNEL,
  RESIZE_WINDOW_CHANNEL,
} from '../../common/ipc/api';
import type {
  CloseWindowMainResponse,
  CloseWindowRendererRequest,
  ResizeWindowMainResponse,
  ResizeWindowRendererRequest,
} from '../../common/ipc/api';
import { MainIpcChannel } from './lib/MainIpcChannel';

const closeWindowChannel = new MainIpcChannel<
  CloseWindowRendererRequest,
  CloseWindowMainResponse
>(CLOSE_WINDOW_CHANNEL);
const resizeWindowChannel = new MainIpcChannel<
  ResizeWindowRendererRequest,
  ResizeWindowMainResponse
>(RESIZE_WINDOW_CHANNEL);

export const handleWindowControlRequests = (window: BrowserWindow): void => {
  closeWindowChannel.onReceive(async () => {
    setTimeout(() => {
      if (!window.isDestroyed()) window.close();
    }, 0);
  });
  resizeWindowChannel.onReceive(async ({ width, height, animate }) => {
    if (!window.isDestroyed()) window.setSize(width, height, animate);
  });
};
