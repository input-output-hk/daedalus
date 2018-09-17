// @flow
import { IpcChannel } from './lib/IpcChannel';
import { RESTART_CARDANO_NODE_CHANNEL } from '../../../common/ipc-api';

export const restartCardanoNodeChannel: IpcChannel<void, void | string> = (
  new IpcChannel(RESTART_CARDANO_NODE_CHANNEL)
);
