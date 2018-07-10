// @flow
import { app, ipcMain } from 'electron';
import { GET_GPU } from '../../common/ipc-api';

export default () => {
  ipcMain.on(GET_GPU.REQUEST, (event) => {
    const sender = event.sender;
    sender.send(GET_GPU.SUCCESS, app.getGPUFeatureStatus());
  });
};
