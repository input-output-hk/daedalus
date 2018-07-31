// @flow
import { app, ipcMain } from 'electron';
import { GET_GPU_STATUS } from '../../common/ipc-api';

export default () => {
  ipcMain.on(GET_GPU_STATUS.REQUEST, (event) => {
    const sender = event.sender;
    sender.send(GET_GPU_STATUS.SUCCESS, app.getGPUFeatureStatus());
  });
};
