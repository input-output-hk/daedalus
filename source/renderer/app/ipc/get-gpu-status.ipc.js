// @flow
import { RendererIpcChannel } from './lib/RendererIpcChannel';
import { GET_GPU_STATUS_CHANNEL } from '../../../common/ipc/api';
import type {
  GetGPUStatusRequest,
  GetGPUStatusResponse,
} from '../../../common/ipc/api';

export const getGPUStatusChannel: RendererIpcChannel<
  GetGPUStatusResponse,
  GetGPUStatusRequest
> = new RendererIpcChannel(GET_GPU_STATUS_CHANNEL);
