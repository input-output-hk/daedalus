// @flow
import { IpcChannel } from '../../../common/ipc-api/lib/IpcChannel';
import { RESTART_CARDANO_NODE_CHANNEL } from '../../../common/ipc-api';
import type { IpcReceiver, IpcSender } from '../../../common/ipc-api/lib/IpcChannel';
import type { RestartCardanoNodeMainResponse } from '../../../common/ipc-api';

export const restartCardanoNodeChannel = (sender: IpcSender, receiver: IpcReceiver): IpcChannel<
  void, RestartCardanoNodeMainResponse, void, void
> => new IpcChannel(RESTART_CARDANO_NODE_CHANNEL, sender, receiver);
