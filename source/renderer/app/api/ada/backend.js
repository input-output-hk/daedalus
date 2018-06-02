import { ipcRenderer, remote } from 'electron';
import { UPDATE_API } from '../../../../common/ipc-api';
import { ApiParams } from '../common';

const originalCa = remote.getGlobal('ca');
const params: ApiParams = {
  ca: originalCa,
  port: 8090
};

ipcRenderer.on(UPDATE_API.REQUEST, (event, paramsIn) => {
  params.ca = paramsIn.ca;
  params.clientCert = paramsIn.clientCert;
  params.clientKey = paramsIn.clientKey;
  params.port = paramsIn.port;
});
ipcRenderer.send(UPDATE_API.CLIENT_REQUEST);

export const apiParams = params;
