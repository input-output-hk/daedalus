import { RendererIpcChannel } from './lib/RendererIpcChannel';
import { GET_GPU_STATUS_CHANNEL } from '../../../common/ipc/api';
import type {
  GetGPUStatusRendererRequest,
  GetGPUStatusMainResponse,
} from '../../../common/ipc/api';

export const getGPUStatusChannel: RendererIpcChannel<
  GetGPUStatusMainResponse,
  GetGPUStatusRendererRequest
> = new RendererIpcChannel(GET_GPU_STATUS_CHANNEL);
