// @flow
import { ipcRenderer } from 'electron';
import { IpcChannel } from '../../../common/ipc-api/lib/IpcChannel';
import { RESTART_CARDANO_NODE_CHANNEL } from '../../../common/ipc-api';
import type { RestartCardanoNodeMainResponse } from '../../../common/ipc-api';

export const restartCardanoNodeChannel = (): (
  // IpcChannel<Request, AwaitedResponse, ReceivedRequest, Response>
  IpcChannel<void, RestartCardanoNodeMainResponse, void, void>
) => (
  new IpcChannel(RESTART_CARDANO_NODE_CHANNEL, ipcRenderer, ipcRenderer)
);
