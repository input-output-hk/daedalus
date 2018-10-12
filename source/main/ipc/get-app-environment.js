// @flow
import { ipcMain } from 'electron';
import { GET_APP_ENVIRONMENT } from '../../common/ipc-api';
import { environment } from '../../common/environment';

export default () => {
  ipcMain.on(GET_APP_ENVIRONMENT.REQUEST, (event) => {
    const sender = event.sender;
    sender.send(GET_APP_ENVIRONMENT.SUCCESS, environment);
  });
};
