import {
  CLOSE_WINDOW_CHANNEL,
  RESIZE_WINDOW_CHANNEL,
} from '../../../common/ipc/api';
import type {
  CloseWindowMainResponse,
  CloseWindowRendererRequest,
  ResizeWindowMainResponse,
  ResizeWindowRendererRequest,
} from '../../../common/ipc/api';
import { RendererIpcChannel } from './lib/RendererIpcChannel';

export const closeWindowChannel = new RendererIpcChannel<
  CloseWindowMainResponse,
  CloseWindowRendererRequest
>(CLOSE_WINDOW_CHANNEL);
export const resizeWindowChannel = new RendererIpcChannel<
  ResizeWindowMainResponse,
  ResizeWindowRendererRequest
>(RESIZE_WINDOW_CHANNEL);
