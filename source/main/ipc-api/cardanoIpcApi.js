// @flow
import { ipcMain } from 'electron';
import { IpcChannel } from '../../common/ipc-api/lib/IpcChannel';
import { RESTART_CARDANO_NODE_CHANNEL } from '../../common/ipc-api';
import type { RestartCardanoNodeMainResponse } from '../../common/ipc-api';
import type { IpcSender } from '../../common/ipc-api/lib/IpcChannel';

export const restartCardanoNodeChannel = (sender: IpcSender): (
  // IpcChannel<Request, AwaitedResponse, ReceivedRequest, Response>
  IpcChannel<void, void, void, RestartCardanoNodeMainResponse>
) => (
  new IpcChannel(RESTART_CARDANO_NODE_CHANNEL, sender, ipcMain)
);
