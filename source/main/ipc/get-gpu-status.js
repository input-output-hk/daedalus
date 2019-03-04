// @flow
import { app } from 'electron';
import { MainIpcChannel } from './lib/MainIpcChannel';
import type {
  GetGPUStatusRequest,
  GetGPUStatusResponse,
} from '../../common/ipc/api';
import { GET_GPU_STATUS_CHANNEL } from '../../common/ipc/api';

export const getGPUStatusChannel: (
  MainIpcChannel<GetGPUStatusRequest, GetGPUStatusResponse>
) = new MainIpcChannel(GET_GPU_STATUS_CHANNEL);

export default () => {
  getGPUStatusChannel.onRequest(() => (
    Promise.resolve(app.getGPUFeatureStatus())
  ));
};
